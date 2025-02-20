from __future__ import annotations

import re
from typing import Any, Callable, Mapping, Sequence

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
from taghound.exceptions import InvalidOperatorError, MissingRootConditionError
from taghound.scalar.operators import ComparisonOperatorMap, LogicalOperatorMap


def create_scalar_function(data: Mapping[str, Any], merge_pattern: str) -> PyEvalFn:
    """Create a function that will evaluate the scalar condition.

    Args:
        data (Mapping): The data to create the scalar function.
        merge_pattern (str): The pattern to merge the regex patterns.

    Returns:
        PyEvalFn: The function that will evaluate the scalar condition.
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
    """Create a function that will evaluate the logical operation (OR/AND).

    Args:
        params (Mapping): The parameters for the logical operation.
        merge_pattern (str): The pattern to merge the regex patterns.

    Returns:
        Callable: The function that will evaluate the logical operation.

    Raises:
        InvalidOperatorError: If the operator is not valid.

    Example:
        ```python
        params = {
            "and": [
                {"key": "language", "op": "=", "value": "python"},
                {"key": "year", "op": ">", "value": 1990},
            ]
        }
        fn = _make_logic_operation_function(params)
        ```
    """

    try:
        logical_operator = LogicalOperator(next(iter(params.keys())))
    except StopIteration:
        raise InvalidOperatorError(
            f"Invalid operator. Must be one of {LogicalOperator.__members__}"
        )
    except ValueError:
        raise InvalidOperatorError(
            f"Invalid operator. Must be one of {LogicalOperator.__members__}"
        )
    logical_fn = LogicalOperatorMap[LogicalOperator(next(iter(params.keys())))]

    def logic_function(data: Mapping[str, Any]) -> bool:
        return logical_fn(
            fn(data)
            for fn in _parse_conditions(
                params[logical_operator.value], merge_pattern=merge_pattern
            )
        )

    return logic_function


def _parse_conditions(
    conditions: Sequence[Mapping], merge_pattern: str
) -> list[Callable[[Mapping[str, Any]], bool]]:
    """Create a list of functions that will evaluate the conditions.

    Args:
        conditions (list[Mapping]): The list of conditions to be evaluated.
        merge_pattern (str): The pattern to merge the regex patterns.

    Returns:
        list[Callable]: The list of functions that will evaluate the conditions.

    Example:
        ```python
        elems = [
            {"key": "language", "op": "=", "value": "python"},
            {"key": "year", "op": ">", "value": 1990},
        ]
        fns = _make_condition_functions(elems)
        ```

    """
    if not isinstance(conditions, Sequence):
        raise TypeError(f"Must be list. Given: `{type(conditions)}`")

    comparison_fns = []
    for condition in conditions:
        if not isinstance(condition, Mapping):
            raise TypeError(f"Expected a Mapping. Given: `{type(condition)}`")
        if not condition:
            raise ValueError("Empty Mapping found in logical conditions.")

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
    key: str, value: Any, operator: str, merge_pattern: str
) -> Callable[[Any], bool]:
    """Create a function that will evaluate the comparison condition.

    Args:
        key (str): The key in the Mapping to be compared.
        value (Any): The value to be compared.
        operator (str): The operator used in the condition e.g `=`, `>`, ...
        merge_pattern (str): The pattern to merge the regex patterns.

    Returns:
        Callable: The function that will evaluate the comparison condition.
    """
    operator_enum = ComparisonOperator(operator)

    value, operator_enum = _validate_and_normalize_comparison_condition_data(
        value, operator_enum, merge_pattern
    )

    def comparison_condition_function(data: Mapping[str, Any]) -> bool:
        return ComparisonOperatorMap[operator_enum](data, key, value)

    return comparison_condition_function


def _validate_and_normalize_comparison_condition_data(
    value: Any,
    operator_enum: ComparisonOperator,
    merge_pattern: str,
) -> tuple[Any, ComparisonOperator]:
    """Validate and normalize the value based on the operator.

    Args:
        value (Any): The value to be validated
        operator_enum (ComparisonOperator): The operator used in the condition
        merge_pattern (str): The pattern to merge the regex patterns

    Returns:
        tuple[Any, ComparisonOperator]: The normalized value and operator
    """
    if operator_enum in ComparisonOperatorRegexOnly:
        if isinstance(value, list):
            pattern_str = "|".join(value)
        elif isinstance(value, str):
            pattern_str = value
        else:
            raise ValueError(f"Invalid value for regex match: `{value}` ({type(value)}")

        try:
            value = re.compile(merge_pattern.format(pattern=pattern_str), re.IGNORECASE)
        except re.error:
            raise ValueError(f"Invalid regex pattern: `{pattern_str}`")

    elif operator_enum in ComparisonOperatorNumericOnly:
        if not isinstance(value, (int, float)):
            raise ValueError(
                f"Invalid value for number comparison: `{value}` ({type(value)})"
            )

    elif operator_enum in ComparisonOperatorListOnly:
        if not isinstance(value, (list, tuple, set)):
            raise ValueError(
                f"Invalid value for list comparison: `{value}` ({type(value)}"
            )
        value = set(value)

    elif isinstance(value, bool):
        if operator_enum == ComparisonOperator.EQUAL:
            operator_enum = ComparisonOperator.IS
        elif operator_enum == ComparisonOperator.NOT_EQUAL:
            operator_enum = ComparisonOperator.IS_NOT

    return value, operator_enum
