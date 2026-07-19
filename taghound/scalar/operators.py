"""Comparison and logical operator implementations for the scalar path."""

from __future__ import annotations

import re
from typing import Any
from collections.abc import Callable, Mapping

from taghound.constants import ComparisonOperator, LogicalOperator

LogicalOperatorMap: dict[LogicalOperator, Callable] = {
    LogicalOperator.AND: all,
    LogicalOperator.OR: any,
}


def eval_equal(data: Mapping, key: str, value: bool | str | int | float) -> bool:
    """Return whether `data[key] == value`."""
    return data[key] == value


def eval_not_equal(data: Mapping, key: str, value: bool | str | int | float) -> bool:
    """Return whether `data[key] != value`."""
    return data[key] != value


def eval_greater_than(data: Mapping, key: str, value: int | float) -> bool:
    """Return whether `data[key] > value`."""
    return data[key] > value


def eval_greater_than_or_equal(data: Mapping, key: str, value: int | float) -> bool:
    """Return whether `data[key] >= value`."""
    return data[key] >= value


def eval_less_than(data: Mapping, key: str, value: int | float) -> bool:
    """Return whether `data[key] < value`."""
    return data[key] < value


def eval_less_than_or_equal(data: Mapping, key: str, value: int | float) -> bool:
    """Return whether `data[key] <= value`."""
    return data[key] <= value


def eval_in(data: Mapping, key: str, value: set[bool | str | int | float]) -> bool:
    """Return whether `data[key]` is a member of `value`."""
    return data[key] in value


def eval_not_in(data: Mapping, key: str, value: set[bool | str | int | float]) -> bool:
    """Return whether `data[key]` is not a member of `value`."""
    return data[key] not in value


def eval_is(data: Mapping, key: str, value: bool) -> bool:
    """Return whether `data[key] is value`."""
    return data[key] is value


def eval_is_not(data: Mapping, key: str, value: bool) -> bool:
    """Return whether `data[key] is not value`."""
    return data[key] is not value


def eval_regex_match(data: Mapping, key: str, value: re.Pattern[str]) -> bool:
    """Return whether the pattern matches anywhere in `data[key]`."""
    return value.search(data[key]) is not None


def eval_regex_not_match(data: Mapping, key: str, value: re.Pattern[str]) -> bool:
    """Return whether the pattern matches nowhere in `data[key]`."""
    return not value.search(data[key])


ComparisonOperatorFn = Callable[[Mapping, str, Any], bool]
ComparisonOperatorMap: Mapping[ComparisonOperator, ComparisonOperatorFn] = {
    ComparisonOperator.EQUAL: eval_equal,
    ComparisonOperator.NOT_EQUAL: eval_not_equal,
    ComparisonOperator.GREATER_THAN: eval_greater_than,
    ComparisonOperator.GREATER_THAN_OR_EQUAL: eval_greater_than_or_equal,
    ComparisonOperator.LESS_THAN: eval_less_than,
    ComparisonOperator.LESS_THAN_OR_EQUAL: eval_less_than_or_equal,
    ComparisonOperator.IN: eval_in,
    ComparisonOperator.NOT_IN: eval_not_in,
    ComparisonOperator.IS: eval_is,
    ComparisonOperator.IS_NOT: eval_is_not,
    ComparisonOperator.REGEX_MATCH: eval_regex_match,
    ComparisonOperator.REGEX_NOT_MATCH: eval_regex_not_match,
}
