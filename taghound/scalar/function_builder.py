"""Build per-rule check functions that evaluate one data mapping at a time."""

from __future__ import annotations

import re
from typing import Any
from collections.abc import Callable, Mapping, Sequence

from taghound.constants import (
    DEFAULT_COMPARISON_OPERATOR,
    ComparisonKey,
    ComparisonOperator,
    ComparisonOperatorListOnly,
    ComparisonOperatorNumericOnly,
    ComparisonOperatorRegexOnly,
    LogicalOperator,
    PyEvalFn,
    RuleKey,
)
from taghound.exceptions import (
    InvalidOperatorError,
    InvalidRuleError,
    InvalidRuleTypeError,
    MissingRootConditionError,
)
from taghound.scalar.operators import ComparisonOperatorMap, LogicalOperatorMap


def create_scalar_function(data: Mapping[str, Any], merge_pattern: str) -> PyEvalFn:
    """Build the function that evaluates a rule against one mapping.

    The rule tree is parsed and every regex compiled once here; the returned
    function only applies them.
    """
    if RuleKey.ROOT_AND.value in data:
        condition_fn = _create_logic_function(
            {LogicalOperator.AND.value: data[RuleKey.ROOT_AND.value]},
            merge_pattern=merge_pattern,
        )
    elif RuleKey.ROOT_OR.value in data:
        condition_fn = _create_logic_function(
            {LogicalOperator.OR.value: data[RuleKey.ROOT_OR.value]},
            merge_pattern=merge_pattern,
        )
    else:
        raise MissingRootConditionError(
            "Root condition must have either `and` or `or`."
        )
    return condition_fn


def _create_logic_function(
    params: Mapping[str, Sequence[Mapping[str, Any]]], merge_pattern: str
) -> Callable:
    """Build the function combining a node's child conditions with `and` or `or`."""
    try:
        logical_operator = LogicalOperator(next(iter(params.keys())))
    except (StopIteration, ValueError):
        raise InvalidOperatorError(
            f"Invalid operator. Must be one of {LogicalOperator.__members__}"
        )
    logical_fn = LogicalOperatorMap[logical_operator]

    condition_fns = _parse_conditions(
        params[logical_operator.value], merge_pattern=merge_pattern
    )

    def logic_function(data: Mapping[str, Any]) -> bool:
        return logical_fn(fn(data) for fn in condition_fns)

    return logic_function


def _parse_conditions(
    conditions: Sequence[Mapping], merge_pattern: str
) -> list[Callable[[Mapping[str, Any]], bool]]:
    """Build one evaluation function per condition, recursing into nested and/or."""
    if not isinstance(conditions, Sequence):
        raise InvalidRuleTypeError(f"Must be list. Given: `{type(conditions)}`")

    comparison_fns = []
    for condition in conditions:
        if not isinstance(condition, Mapping):
            raise InvalidRuleTypeError(
                f"Expected a Mapping. Given: `{type(condition)}`"
            )
        if not condition:
            raise InvalidRuleError("Empty Mapping found in logical conditions.")

        if len(condition.keys()) == 1:
            conditions_fn = _create_logic_function(
                condition, merge_pattern=merge_pattern
            )
            comparison_fns.append(conditions_fn)
        else:
            fn = _make_comparison_condition_function(
                key=condition[ComparisonKey.KEY.value],
                value=condition[ComparisonKey.VALUE.value],
                operator=condition.get(
                    ComparisonKey.OPERATOR.value, DEFAULT_COMPARISON_OPERATOR.value
                ),
                merge_pattern=merge_pattern,
            )
            comparison_fns.append(fn)
    return comparison_fns


def _make_comparison_condition_function(
    key: str, value: object, operator: str, merge_pattern: str
) -> Callable[[Any], bool]:
    """Build the function evaluating a single `key`/`op`/`value` comparison."""
    try:
        operator_enum = ComparisonOperator(operator)
    except ValueError:
        raise InvalidRuleError(f"Unsupported operation: {operator}")

    value, operator_enum = _validate_and_normalize_comparison_condition_data(
        value, operator_enum, merge_pattern
    )

    def comparison_condition_function(data: Mapping[str, Any]) -> bool:
        return ComparisonOperatorMap[operator_enum](data, key, value)

    return comparison_condition_function


def _validate_and_normalize_comparison_condition_data(
    value: object,
    operator_enum: ComparisonOperator,
    merge_pattern: str,
) -> tuple[Any, ComparisonOperator]:
    """Check the value suits the operator, and put it in the form the operator wants.

    Regex values come back compiled, list values as a set, and a bool compared with
    `=` or `!=` switches the operator to `is`/`is_not`.
    """
    if operator_enum in ComparisonOperatorRegexOnly:
        if isinstance(value, list):
            pattern_str = "|".join(value)
        elif isinstance(value, str):
            pattern_str = value
        else:
            raise InvalidRuleError(
                f"Invalid value for regex match: `{value}` ({type(value)}"
            )

        try:
            value = re.compile(merge_pattern.format(pattern=pattern_str), re.IGNORECASE)
        except re.error:
            raise InvalidRuleError(f"Invalid regex pattern: `{pattern_str}`")

    elif operator_enum in ComparisonOperatorNumericOnly:
        if not isinstance(value, (int, float)):
            raise InvalidRuleError(
                f"Invalid value for number comparison: `{value}` ({type(value)})"
            )

    elif operator_enum in ComparisonOperatorListOnly:
        if not isinstance(value, (list, tuple, set)):
            raise InvalidRuleError(
                f"Invalid value for list comparison: `{value}` ({type(value)}"
            )
        value = set(value)

    elif isinstance(value, bool):
        if operator_enum == ComparisonOperator.EQUAL:
            operator_enum = ComparisonOperator.IS
        elif operator_enum == ComparisonOperator.NOT_EQUAL:
            operator_enum = ComparisonOperator.IS_NOT

    return value, operator_enum
