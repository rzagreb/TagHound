"""Build per-rule check functions that evaluate a whole DataFrame at once."""

from __future__ import annotations

import operator as op
import re
from collections.abc import Callable, Mapping
from typing import Any

import pandas as pd

from taghound.constants import (
    DEFAULT_COMPARISON_OPERATOR,
    ComparisonKey,
    ComparisonOperator,
    ComparisonOperatorListOnly,
    ComparisonOperatorNumericOnly,
    ComparisonOperatorRegexOnly,
    LogicalOperator,
    PdEvalFn,
    RuleKey,
)

_NUMERIC_OPERATORS: Mapping[ComparisonOperator, Callable] = {
    ComparisonOperator.GREATER_THAN: op.gt,
    ComparisonOperator.GREATER_THAN_OR_EQUAL: op.ge,
    ComparisonOperator.LESS_THAN: op.lt,
    ComparisonOperator.LESS_THAN_OR_EQUAL: op.le,
}


def create_vector_function(rule: Mapping[str, Any], merge_pattern: str) -> PdEvalFn:
    """Create a function that will evaluate the vector condition.

    The rule tree is parsed and validated once here; the returned function only
    applies the prebuilt column operations to the given DataFrame.

    Args:
        rule (Mapping[str, Any]): The rule to evaluate.
        merge_pattern (str): The pattern to merge the regex patterns.

    Returns:
        PdEvalFn: The function that will evaluate the vector condition.
    """
    if not isinstance(rule, dict):
        raise TypeError(f"Rule must be a dictionary. Given: {type(rule)}")

    if RuleKey.ROOT_OR.value not in rule and RuleKey.ROOT_AND.value not in rule:
        raise ValueError(
            f"Rule must contain either '{RuleKey.ROOT_OR.value}' or "
            f"'{RuleKey.ROOT_AND.value}'."
        )

    criteria_fn = _build_criteria_function(rule, merge_pattern=merge_pattern)

    def vector_mask(df: pd.DataFrame) -> pd.Series:
        if df.empty:
            return pd.Series([False] * len(df), index=df.index)
        return criteria_fn(df)

    return vector_mask


def _build_criteria_function(rule: Mapping[str, Any], merge_pattern: str) -> PdEvalFn:
    """Recursively build the evaluation function for a rule node.

    Args:
        rule (Mapping[str, Any]): A logical node (`and`/`or` over child nodes)
            or a comparison condition (`key`/`op`/`value`).
        merge_pattern (str): The pattern to merge the regex patterns.

    Returns:
        PdEvalFn: The function that evaluates this node against a DataFrame.
    """
    if LogicalOperator.AND.value in rule:
        child_fns = [
            _build_criteria_function(cond, merge_pattern=merge_pattern)
            for cond in rule[LogicalOperator.AND.value]
        ]

        def and_mask(df: pd.DataFrame) -> pd.Series:
            mask = pd.Series(True, index=df.index)
            for child_fn in child_fns:
                mask &= child_fn(df)
            return mask

        return and_mask

    if LogicalOperator.OR.value in rule:
        child_fns = [
            _build_criteria_function(cond, merge_pattern=merge_pattern)
            for cond in rule[LogicalOperator.OR.value]
        ]

        def or_mask(df: pd.DataFrame) -> pd.Series:
            mask = pd.Series(False, index=df.index)
            for child_fn in child_fns:
                mask |= child_fn(df)
            return mask

        return or_mask

    if ComparisonKey.KEY.value not in rule or ComparisonKey.VALUE.value not in rule:
        raise ValueError(
            f"Criteria must contain '{ComparisonKey.KEY.value}' and "
            f"'{ComparisonKey.VALUE.value}'."
            f" Given: {rule.keys()}"
        )
    column = rule[ComparisonKey.KEY.value]
    operator = rule.get(ComparisonKey.OPERATOR.value, DEFAULT_COMPARISON_OPERATOR.value)
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

    return _build_series_condition(
        column=column,
        operator=operator,
        const_value=const_value,
        merge_pattern=merge_pattern,
    )


def _build_series_condition(
    column: str,
    operator: str,
    const_value: object,
    merge_pattern: str,
) -> PdEvalFn:
    """Build a Series condition function for a single comparison.

    Args:
        column (str): The column to evaluate.
        operator (str): The operator to use.
        const_value (object): The value to compare against.
        merge_pattern (str): The merge pattern to use for regex operators.

    Returns:
        PdEvalFn: A function evaluating the comparison against a DataFrame.
    """
    try:
        operator_enum = ComparisonOperator(operator)
    except ValueError:
        raise ValueError(f"Unsupported operation: {operator}")

    if operator_enum in ComparisonOperatorRegexOnly:
        pattern = (
            "|".join(const_value) if isinstance(const_value, list) else const_value
        )
        # Inline `(?i)` rather than `flags=re.IGNORECASE`: passing `flags` forces
        # pandas onto the slow per-element Python `re` path even for
        # `string[pyarrow]` columns. With the flag inline, pyarrow-backed columns
        # use the vectorised RE2 engine while object columns behave the same. Note
        # RE2 rejects look-around and backreferences.
        pattern = "(?i)" + merge_pattern.format(pattern=pattern)
        try:
            re.compile(pattern)
        except re.error:
            raise ValueError(f"Invalid regex pattern: `{pattern}`")
        negate = operator_enum == ComparisonOperator.REGEX_NOT_MATCH

        def regex_condition(df: pd.DataFrame) -> pd.Series:
            matches = _get_column(df, column).str.contains(pattern, regex=True)
            # Missing values yield <NA>/NaN; treat them as "no match" and coerce
            # to a plain bool Series so nullable (pyarrow) results don't leak NA
            # into the and/or masks or the final `to_numpy(dtype=bool)`.
            matches = matches.fillna(False).astype(bool)
            return ~matches if negate else matches

        return regex_condition

    if operator_enum in ComparisonOperatorNumericOnly:
        if not isinstance(const_value, (int, float)):
            raise ValueError(
                f"Invalid value for number comparison: "
                f"`{const_value}` ({type(const_value)})"
            )
        compare = _NUMERIC_OPERATORS[operator_enum]

        def numeric_condition(df: pd.DataFrame) -> pd.Series:
            return compare(_get_column(df, column), const_value)

        return numeric_condition

    if operator_enum in ComparisonOperatorListOnly:
        if not isinstance(const_value, (list, tuple, set)):
            raise ValueError(
                f"Value for operator '{operator}' must be a list."
                f" Given: {type(const_value)}"
            )
        negate = operator_enum == ComparisonOperator.NOT_IN

        def membership_condition(df: pd.DataFrame) -> pd.Series:
            mask = _get_column(df, column).isin(const_value)
            return ~mask if negate else mask

        return membership_condition

    # =, !=, is, is_not; `is` has no vectorized equivalent, so it compares equal
    negate = operator_enum in (
        ComparisonOperator.NOT_EQUAL,
        ComparisonOperator.IS_NOT,
    )
    if isinstance(const_value, (list, tuple, set)):

        def equality_condition(df: pd.DataFrame) -> pd.Series:
            mask = _get_column(df, column).isin(const_value)
            return ~mask if negate else mask

    else:

        def equality_condition(df: pd.DataFrame) -> pd.Series:
            series = _get_column(df, column)
            return series != const_value if negate else series == const_value

    return equality_condition


def _get_column(df: pd.DataFrame, column: str) -> pd.Series:
    """Return the column, raising if the DataFrame does not have it.

    Args:
        df (pd.DataFrame): The DataFrame to read from.
        column (str): The column name.

    Returns:
        pd.Series: The column values.
    """
    if column not in df.columns:
        raise ValueError(f"Column '{column}' not found in DataFrame.")
    return df[column]


def _evaluate_criteria(
    df: pd.DataFrame, rule: Mapping[str, list | dict], merge_pattern: str
) -> pd.Series:
    """Evaluate the criteria based on the rule (compatibility wrapper).

    Args:
        df (pd.DataFrame): The DataFrame to evaluate.
        rule (Mapping[str, Union[list, dict]]): The rule to evaluate.
        merge_pattern (str): The pattern to merge the regex patterns.

    Returns:
        pd.Series: The Series condition.
    """
    if df.empty:
        return pd.Series([False] * len(df), index=df.index)
    return _build_criteria_function(rule, merge_pattern=merge_pattern)(df)


def _make_series_condition(
    df: pd.DataFrame,
    column: str,
    operator: str,
    const_value: str | list | bool,
    merge_pattern: str,
) -> pd.Series:
    """Create and apply a Series condition (compatibility wrapper).

    Args:
        df (pd.DataFrame): The DataFrame to evaluate.
        column (str): The column to evaluate.
        operator (str): The operator to use.
        const_value (Union[str, list, bool]): The value to compare against.
        merge_pattern (str): The merge pattern to use.

    Returns:
        pd.Series: The Series condition.
    """
    return _build_series_condition(
        column=column,
        operator=operator,
        const_value=const_value,
        merge_pattern=merge_pattern,
    )(df)
