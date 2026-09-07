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
from taghound.exceptions import InvalidRuleError, InvalidRuleTypeError
from taghound.re2 import check_re2_safe

_NUMERIC_OPERATORS: Mapping[ComparisonOperator, Callable] = {
    ComparisonOperator.GREATER_THAN: op.gt,
    ComparisonOperator.GREATER_THAN_OR_EQUAL: op.ge,
    ComparisonOperator.LESS_THAN: op.lt,
    ComparisonOperator.LESS_THAN_OR_EQUAL: op.le,
}


def create_vector_function(rule: Mapping[str, Any], merge_pattern: str) -> PdEvalFn:
    """Build the function that evaluates `rule` against a DataFrame.

    The rule tree is parsed and validated once here; the returned function only
    applies the prebuilt column operations to the frame it is given.
    """
    if not isinstance(rule, dict):
        raise InvalidRuleTypeError(f"Rule must be a dictionary. Given: {type(rule)}")

    if RuleKey.ROOT_OR.value not in rule and RuleKey.ROOT_AND.value not in rule:
        raise InvalidRuleError(
            f"Rule must contain either '{RuleKey.ROOT_OR.value}' or "
            f"'{RuleKey.ROOT_AND.value}'."
        )

    criteria_fn = _build_criteria_function(rule, merge_pattern=merge_pattern)

    def vector_mask(df: pd.DataFrame) -> pd.Series:
        # Guards the frame with no columns at all (`pd.DataFrame([])`), which would
        # otherwise fail `_get_column`. A typed but rowless frame needs no guard.
        if df.empty:
            return pd.Series(False, index=df.index, dtype=bool)
        return criteria_fn(df)

    return vector_mask


def _build_criteria_function(rule: Mapping[str, Any], merge_pattern: str) -> PdEvalFn:
    """Recursively build the evaluation function for one node of a rule tree.

    A node is either a logical `and`/`or` over children, or a `key`/`op`/`value`
    comparison.
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
        raise InvalidRuleError(
            f"Criteria must contain '{ComparisonKey.KEY.value}' and "
            f"'{ComparisonKey.VALUE.value}'."
            f" Given: {rule.keys()}"
        )
    column = rule[ComparisonKey.KEY.value]
    operator = rule.get(ComparisonKey.OPERATOR.value, DEFAULT_COMPARISON_OPERATOR.value)
    const_value = rule[ComparisonKey.VALUE.value]
    if not isinstance(column, str):
        raise InvalidRuleError(
            f"Value for key '{ComparisonKey.KEY.value}' must be a string. "
            f"Given: {type(column)}"
        )
    if not isinstance(operator, str):
        raise InvalidRuleError(
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
    """Build the function evaluating a single comparison against a DataFrame."""
    try:
        operator_enum = ComparisonOperator(operator)
    except ValueError:
        raise InvalidRuleError(f"Unsupported operation: {operator}")

    if operator_enum in ComparisonOperatorRegexOnly:
        pattern = (
            "|".join(const_value) if isinstance(const_value, list) else const_value
        )
        # Inline `(?i)` rather than `flags=re.IGNORECASE`: passing `flags` forces
        # pandas onto the slow per-element Python `re` path even for
        # `string[pyarrow]` columns. With the flag inline, pyarrow-backed columns
        # use the vectorised RE2 engine while object columns behave the same. Note
        # RE2 rejects look-around and backreferences.
        # Checked before the merge wrapper goes on: the wrapper is the caller's
        # explicit choice and may deliberately use look-around for object columns,
        # whereas the pattern is what the rule's author wrote.
        check_re2_safe(pattern)
        pattern = "(?i)" + merge_pattern.format(pattern=pattern)
        try:
            re.compile(pattern)
        except re.error:
            raise InvalidRuleError(f"Invalid regex pattern: `{pattern}`")
        negate = operator_enum == ComparisonOperator.REGEX_NOT_MATCH

        def regex_condition(df: pd.DataFrame) -> pd.Series:
            matches = _as_bool_mask(
                _get_column(df, column).str.contains(pattern, regex=True)
            )
            return ~matches if negate else matches

        return regex_condition

    if operator_enum in ComparisonOperatorNumericOnly:
        if not isinstance(const_value, (int, float)):
            raise InvalidRuleError(
                f"Invalid value for number comparison: "
                f"`{const_value}` ({type(const_value)})"
            )
        compare = _NUMERIC_OPERATORS[operator_enum]

        def numeric_condition(df: pd.DataFrame) -> pd.Series:
            return _as_bool_mask(compare(_get_column(df, column), const_value))

        return numeric_condition

    if operator_enum in ComparisonOperatorListOnly:
        if not isinstance(const_value, (list, tuple, set)):
            raise InvalidRuleError(
                f"Value for operator '{operator}' must be a list."
                f" Given: {type(const_value)}"
            )
        negate = operator_enum == ComparisonOperator.NOT_IN

        def membership_condition(df: pd.DataFrame) -> pd.Series:
            mask = _as_bool_mask(_get_column(df, column).isin(const_value))
            return ~mask if negate else mask

        return membership_condition

    # =, !=, is, is_not; `is` has no vectorized equivalent, so it compares equal
    negate = operator_enum in (
        ComparisonOperator.NOT_EQUAL,
        ComparisonOperator.IS_NOT,
    )
    if isinstance(const_value, (list, tuple, set)):

        def equality_condition(df: pd.DataFrame) -> pd.Series:
            mask = _as_bool_mask(_get_column(df, column).isin(const_value))
            return ~mask if negate else mask

    else:

        def equality_condition(df: pd.DataFrame) -> pd.Series:
            # Negating the positive test rather than comparing with `!=` directly:
            # a missing cell resolves to "did not match" first, so `!=` against it
            # is True, the same way `!~` already treats one.
            mask = _as_bool_mask(_get_column(df, column) == const_value)
            return ~mask if negate else mask

    return equality_condition


def _as_bool_mask(mask: pd.Series) -> pd.Series:
    """Resolve a comparison result to a plain bool Series, nulls counting as no match.

    Every branch funnels through here so a nullable dtype cannot leak `<NA>` into
    the and/or masks or into the final `to_numpy(dtype=bool)`, both of which
    refuse it. Casting to `boolean` before filling stops pandas guessing at the
    result type of an object column, which it warns about and will change.
    """
    return mask.astype("boolean").fillna(False).astype(bool)


def _get_column(df: pd.DataFrame, column: str) -> pd.Series:
    """Return the column, raising if the DataFrame does not have it."""
    if column not in df.columns:
        raise InvalidRuleError(f"Column '{column}' not found in DataFrame.")
    return df[column]
