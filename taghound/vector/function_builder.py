from __future__ import annotations

import re
from typing import Any, Mapping, Union

import pandas as pd

from taghound.constants import (
    ComparisonKey,
    ComparisonOperator,
    LogicalOperator,
    PdEvalFn,
    RuleKey,
)


def create_vector_function(rule: Mapping[str, Any], merge_pattern: str) -> PdEvalFn:
    """Create a function that will evaluate the vector condition.

    Args:
        rule (Mapping[str, Any]): The rule to evaluate.
        merge_pattern (str): The pattern to merge the regex patterns.

    Returns:
        PdEvalFn: The function that will evaluate the vector condition.
    """
    if not isinstance(rule, dict):
        raise TypeError(f"Rule must be a dictionary. Given: {type(rule)}")

    if RuleKey.ROOT_OR.value in rule:

        def vector_mask(df: pd.DataFrame) -> pd.Series:
            return _evaluate_criteria(df, rule, merge_pattern=merge_pattern)

    elif RuleKey.ROOT_AND.value in rule:

        def vector_mask(df: pd.DataFrame) -> pd.Series:
            return _evaluate_criteria(df, rule, merge_pattern=merge_pattern)

    else:
        raise ValueError(
            f"Rule must contain either '{RuleKey.ROOT_OR.value}' or "
            f"'{RuleKey.ROOT_AND.value}'."
        )

    return vector_mask


def _evaluate_criteria(
    df: pd.DataFrame, rule: Mapping[str, Union[list, dict]], merge_pattern: str
) -> pd.Series:
    """ Evaluate the criteria based on the rule.
    
    Args:
        df (pd.DataFrame): The DataFrame to evaluate.
        rule (Mapping[str, Union[list, dict]]): The rule to evaluate.
        merge_pattern (str): The pattern to merge the regex patterns.

    Returns:
        pd.Series: The Series condition.
    """
    if df.empty:
        return pd.Series([False] * len(df), index=df.index)

    if LogicalOperator.AND.value in rule:
        mask = pd.Series([True] * len(df), index=df.index)
        for cond in rule[LogicalOperator.AND.value]:
            mask &= _evaluate_criteria(df, cond, merge_pattern=merge_pattern)
        return mask

    elif LogicalOperator.OR.value in rule:
        mask = pd.Series([False] * len(df), index=df.index)
        for cond in rule[LogicalOperator.OR.value]:
            try:
                mask |= _evaluate_criteria(df, cond, merge_pattern=merge_pattern)
            except Exception as e:
                print(f"Error evaluating condition: {cond}")
                print(df)
                print(cond)
                raise e
        return mask

    else:
        # If 'key' is missing, raise an error
        if (
            ComparisonKey.KEY.value not in rule
            or ComparisonKey.OPERATOR.value not in rule
            or ComparisonKey.VALUE.value not in rule
        ):
            raise ValueError(
                f"Criteria must contain '{ComparisonKey.KEY.value}', "
                f"'{ComparisonKey.OPERATOR.value}', and "
                f"'{ComparisonKey.VALUE.value}'."
                f" Given: {rule.keys()}"
            )
        column = rule[ComparisonKey.KEY.value]
        operator = rule[ComparisonKey.OPERATOR.value]
        const_value = rule[ComparisonKey.VALUE.value]
        if not isinstance(column, str):
            raise ValueError(
                f"Value for key '{ComparisonKey.KEY.value}' must be a string. "
                f"Given: {type(column)}"
            )
        if not isinstance(operator, str):
            raise ValueError(
                f"Value for key '{ComparisonKey.OPERATOR.value}' must be a string. "
                f"Given: {type(operator)}"
            )
        if not isinstance(const_value, (str, list, bool, int, float)):
            raise ValueError(
                f"Value for key '{ComparisonKey.VALUE.value}' must be a string, "
                f"list, or boolean. Given: {type(const_value)}"
            )

        return _make_series_condition(
            df=df,
            column=column,
            operator=operator,
            const_value=const_value,
            merge_pattern=merge_pattern,
        )


def _make_series_condition(
    df: pd.DataFrame,
    column: str,
    operator: str,
    const_value: Union[str, list, bool],
    merge_pattern: str,
) -> pd.Series[bool]:
    """Create a Series condition based on the operator and value.

    Args:
        df (pd.DataFrame): The DataFrame to evaluate.
        column (str): The column to evaluate.
        operator (str): The operator to use.
        const_value (Union[str, list, bool]): The value to compare against.
        merge_pattern (str): The merge pattern to use.

    Returns:
        pd.Series[bool]: The Series condition.
    """
    if column not in df.columns:
        raise ValueError(f"Column '{column}' not found in DataFrame.")

    if (
        operator == ComparisonOperator.REGEX_MATCH.value
        or operator == ComparisonOperator.REGEX_NOT_MATCH.value
    ):
        const_value = (
            "|".join(const_value) if isinstance(const_value, list) else const_value
        )
        const_value = merge_pattern.format(pattern=const_value)

        if operator == ComparisonOperator.REGEX_MATCH.value:
            return df[column].str.contains(const_value, regex=True, flags=re.IGNORECASE)

        elif operator == ComparisonOperator.REGEX_NOT_MATCH.value:
            return ~df[column].str.contains(
                const_value, regex=True, flags=re.IGNORECASE
            )

    elif operator == ComparisonOperator.EQUAL.value:
        if isinstance(const_value, (list, tuple, set)):
            return df[column].isin(const_value)
        return df[column] == const_value

    elif operator == ComparisonOperator.NOT_EQUAL.value:
        if isinstance(const_value, (list, tuple, set)):
            return ~df[column].isin(const_value)
        return df[column] != const_value

    elif operator == ComparisonOperator.IN.value:
        if not isinstance(const_value, (list, tuple, set)):
            raise ValueError(
                f"Value for operator '{ComparisonOperator.IN.value}' must be a list."
                f" Given: {type(const_value)}"
            )
        return df[column].isin(const_value)

    elif operator == ComparisonOperator.NOT_IN.value:
        if not isinstance(const_value, (list, tuple, set)):
            raise ValueError(
                f"Value for operator '{ComparisonOperator.NOT_IN.value}' must be a list."
                f" Given: {type(const_value)}"
            )
        return ~df[column].isin(const_value)

    else:
        raise ValueError(f"Unsupported operation: {operator}")
