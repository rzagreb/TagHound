"""Serialization of raw rule dicts into TagRule objects."""

from __future__ import annotations

from typing import Any
from collections.abc import Mapping, Sequence

from taghound.constants import (
    DEFAULT_REGEX_MERGE_PATTERN,
    DEFAULT_WEIGHT,
    ComparisonKey,
    LogicalOperator,
    RuleKey,
)
from taghound.models import TagRule
from taghound.exceptions import MissingRootConditionError, RuleAndOrTogetherError
from taghound.scalar.function_builder import create_scalar_function
from taghound.vector.function_builder import create_vector_function


def to_tag_rule(
    rule_data: Mapping[str, Any],
    add_scalar_func: bool = True,
    add_vector_fn: bool = True,
    merge_pattern: str = DEFAULT_REGEX_MERGE_PATTERN,
) -> TagRule:
    """Create a TagRule object from the given data.

    Args:
        rule_data (Mapping[str, Any]): The rule definition: `id`, optional `label`,
            `weight` and `info`, plus a root `and` or `or` condition list.
        add_scalar_func (bool): Whether to build the scalar check function.
        add_vector_fn (bool): Whether to build the vector check function.
        merge_pattern (str): The pattern used to merge regex patterns.

    Returns:
        TagRule: The TagRule object created from the data.

    Example:
        >>> rule = to_tag_rule(
        ...     {
        ...         "id": "python",
        ...         "weight": 10,
        ...         "and": [
        ...             {"key": "language", "op": "=", "value": "python"},
        ...             {"key": "year", "op": ">", "value": 1990},
        ...         ],
        ...     }
        ... )
        >>> rule.id
        'python'
        >>> rule.weight
        10.0
        >>> sorted(rule.required_fields)
        ['language', 'year']
    """
    tag_name = rule_data[RuleKey.ID.value]
    weight = rule_data.get(RuleKey.WEIGHT.value, DEFAULT_WEIGHT)
    label = rule_data.get(RuleKey.LABEL.value, tag_name)
    info = rule_data.get(RuleKey.INFO.value, None)

    # Validate
    if not isinstance(tag_name, str):
        raise ValueError(f"tag_name must be a string. Given: `{type(tag_name)}`")

    if isinstance(weight, int):
        weight = float(weight)
    elif not isinstance(weight, float):
        raise ValueError(f"weight must be a integer or float. Given: {type(weight)}")
    if RuleKey.ROOT_AND.value in rule_data and RuleKey.ROOT_OR.value in rule_data:
        raise RuleAndOrTogetherError(
            "Cannot have both `and` and `or` in the same root condition."
        )

    try:
        required_fields = (
            get_required_keys_from_conditions(rule_data[RuleKey.ROOT_AND.value])
            if RuleKey.ROOT_AND.value in rule_data
            else get_required_keys_from_conditions(rule_data[RuleKey.ROOT_OR.value])
        )
    except KeyError:
        raise MissingRootConditionError()

    tag = TagRule(
        id=tag_name,
        label=label,
        weight=weight,
        info=info,
        required_fields=required_fields,
        scalar_check=(
            create_scalar_function(rule_data, merge_pattern=merge_pattern)
            if add_scalar_func
            else None
        ),
        vector_check=(
            create_vector_function(rule_data, merge_pattern=merge_pattern)
            if add_vector_fn
            else None
        ),
        data=rule_data,
    )
    return tag


def get_required_keys_from_conditions(
    conditions: Sequence[Mapping[str, Any]],
) -> set[str]:
    """Collect the data keys referenced by the conditions, recursing into nested and/or.

    Args:
        conditions (Sequence[Mapping[str, Any]]): The conditions to scan.

    Returns:
        set[str]: The keys the conditions compare against.
    """
    keys_set = set()
    for condition in conditions:
        if not isinstance(condition, Mapping):
            raise TypeError(
                f"Expected a Mapping. Given: `{type(condition)}`. Value: `{condition}`"
            )
        if ComparisonKey.KEY.value in condition:
            keys_set.add(condition[ComparisonKey.KEY.value])
            continue
        for logical_operator in LogicalOperator:
            nested = condition.get(logical_operator.value)
            if isinstance(nested, Sequence) and not isinstance(nested, str):
                keys_set.update(get_required_keys_from_conditions(nested))
    return keys_set
