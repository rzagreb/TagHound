from __future__ import annotations

from typing import Any, Mapping, Sequence

from taghound.constants import DEFAULT_WEIGHT, ComparisonKey, RuleKey
from taghound.models import TagRule
from taghound.exceptions import MissingRootConditionError, RuleAndOrTogetherError
from taghound.scalar.function_builder import create_scalar_function
from taghound.vector.function_builder import create_vector_function


def to_tag_rule(
    rule_data: Mapping[str, Any],
    add_scalar_func: bool = True,
    add_vector_fn: bool = True,
) -> TagRule:
    """Create a TagRule object from the given data.

    Args:
        data (Mapping[str, Any]): The data to create the TagRule object .

    Returns:
        TagRule: The TagRule object created from the data.

    Example:
        ```python
        data = {
            "tag": "python",
            "weight": 10,
            "and": [
                {"key": "language", "op": "=", "value": "python"},
                {"key": "year", "op": ">", "value": 1990},
            ]
        }
        rule = create_tag_assignment_rule(data)
        ```
    """
    tag_name = rule_data[RuleKey.ID.value]
    weight = rule_data.get(RuleKey.WEIGHT.value, DEFAULT_WEIGHT)
    label = rule_data.get(RuleKey.LABEL.value, RuleKey.ID.value)

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
        required_fields=required_fields,
        scalar_check=create_scalar_function(rule_data) if add_scalar_func else None,
        vector_check=create_vector_function(rule_data) if add_vector_fn else None,
        data=rule_data,
    )
    return tag


def get_required_keys_from_conditions(
    conditions: Sequence[Mapping[str, Any]],
) -> set[str]:
    keys_set = set()
    for condition in conditions:
        if not isinstance(condition, Mapping):
            raise TypeError(
                f"Expected a Mapping. Given: `{type(condition)}`. Value: `{condition}`"
            )
        for key, value in condition.items():
            if key == ComparisonKey.KEY.value:
                keys_set.add(value)
                break
            elif isinstance(value, list):
                keys_set.update(get_required_keys_from_conditions(value))
            elif isinstance(value, Mapping):
                keys_set.update(get_required_keys_from_conditions([value]))
    return keys_set
