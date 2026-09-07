"""Serialization of raw rule dicts into TagRule objects."""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from typing import Any

import yaml

from taghound.constants import (
    DEFAULT_REGEX_MERGE_PATTERN,
    DEFAULT_WEIGHT,
    RE2_SAFE_REGEX_MERGE_PATTERN,
    ComparisonKey,
    LogicalOperator,
    RuleKey,
)
from taghound.exceptions import (
    InvalidRuleError,
    InvalidRuleTypeError,
    MissingRootConditionError,
    RuleAndOrTogetherError,
)
from taghound.models import TagRule
from taghound.scalar.function_builder import create_scalar_function
from taghound.vector.function_builder import create_vector_function


def to_tag_rule(
    rule_data: Mapping[str, Any],
    add_scalar_func: bool = True,
    add_vector_fn: bool = True,
    merge_pattern: str = DEFAULT_REGEX_MERGE_PATTERN,
    vector_merge_pattern: str = RE2_SAFE_REGEX_MERGE_PATTERN,
) -> TagRule:
    """Create a TagRule object from the given data.

    The two paths take separate merge patterns because only the vectorized one is
    engine-constrained: it runs on RE2, which rejects look-around. Overriding
    `merge_pattern` alone leaves the vector path RE2-safe.

    `rule_data` holds `id`, plus optional `label`, `weight` and `info`, plus a root
    `and` or `or` condition list.

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
    try:
        tag_name = rule_data[RuleKey.ID.value]
    except KeyError as error:
        raise InvalidRuleError(
            f"A rule needs an `{RuleKey.ID.value}`; it is what the rule is keyed on."
        ) from error
    weight = rule_data.get(RuleKey.WEIGHT.value, DEFAULT_WEIGHT)
    label = rule_data.get(RuleKey.LABEL.value, tag_name)
    info = rule_data.get(RuleKey.INFO.value, None)

    # Validate
    if not isinstance(tag_name, str):
        raise InvalidRuleError(f"tag_name must be a string. Given: `{type(tag_name)}`")

    if isinstance(weight, int):
        weight = float(weight)
    elif not isinstance(weight, float):
        raise InvalidRuleError(
            f"weight must be a integer or float. Given: {type(weight)}"
        )
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
    except KeyError as error:
        raise MissingRootConditionError(
            "Root condition must have either `and` or `or`."
        ) from error

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
            create_vector_function(rule_data, merge_pattern=vector_merge_pattern)
            if add_vector_fn
            else None
        ),
        data=rule_data,
    )
    return tag


def get_required_keys_from_conditions(
    conditions: Sequence[Mapping[str, Any]],
) -> set[str]:
    """Collect the data keys referenced by the conditions, recursing into nested and/or."""
    keys_set = set()
    for condition in conditions:
        if not isinstance(condition, Mapping):
            raise InvalidRuleTypeError(
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


# --- Writing rules back out --------------------------------------------------

ROOT_KEYS = (RuleKey.ROOT_AND.value, RuleKey.ROOT_OR.value)
"""The keys holding a rule's condition tree; everything else is metadata."""

CONDITION_FIELD_ORDER = (
    ComparisonKey.KEY.value,
    ComparisonKey.OPERATOR.value,
    ComparisonKey.VALUE.value,
)
"""Reading order for a condition: what it looks at, how, and against what."""

NO_FOLDING = 10**6
"""A line width PyYAML will never reach, so it never wraps a pattern."""


class _QuotedStr(str):
    """A string the dumper always quotes, however innocuous it looks."""


class _IndentedDumper(yaml.SafeDumper):
    """Dumper that indents list items under the key they belong to.

    PyYAML writes block sequences *indentless*, putting the `-` in the parent
    key's column. That is valid YAML, but it reads as though the items are not
    inside anything.
    """

    def increase_indent(self, flow: bool = False, indentless: bool = False) -> None:
        """Indent block sequences like everything else."""
        return super().increase_indent(flow, indentless=False)


_IndentedDumper.add_representer(
    _QuotedStr,
    lambda dumper, value: dumper.represent_scalar(
        "tag:yaml.org,2002:str", value, style="'"
    ),
)


def _ordered_conditions(node: object) -> object:
    """Rewrite a condition tree in reading order, with every string quoted.

    Quoting matters as much as the ordering. PyYAML only quotes a scalar when the
    syntax forces it, so a dumped rule comes back with some patterns quoted and
    some bare, and no way to tell at a glance which bare ones are one stray
    character away from meaning something else. Field names are dict keys and
    never pass through here, so they stay bare.

    Anything that is not a list, a dict or a string is passed through as-is.
    """
    if isinstance(node, list):
        return [_ordered_conditions(item) for item in node]
    if isinstance(node, str):
        return _QuotedStr(node)
    if not isinstance(node, dict):
        return node

    known = [key for key in CONDITION_FIELD_ORDER if key in node]
    rest = [key for key in node if key not in CONDITION_FIELD_ORDER]
    return {key: _ordered_conditions(node[key]) for key in known + rest}


def _dump(data: object) -> str:
    """Render `data` with the settings a rules file needs."""
    return yaml.dump(
        data,
        Dumper=_IndentedDumper,
        sort_keys=False,
        allow_unicode=True,
        indent=2,
        # One pattern, one line. PyYAML otherwise folds a long scalar at a space,
        # and for a regex that means an edit near the fold silently changes it.
        width=NO_FOLDING,
    )


def dump_conditions(rule: Mapping[str, Any]) -> str:
    """Render just a rule's `and`/`or` tree as YAML, for an editor box.

    Returns an empty string when the rule has no conditions, leaving the caller
    to supply whatever starting point its editor wants.

    Example:
        >>> print(dump_conditions({"id": "r", "or": [{"value": "x", "key": "t"}]}))
        or:
          - key: 't'
            value: 'x'
        <BLANKLINE>
    """
    conditions = {
        key: _ordered_conditions(rule[key]) for key in ROOT_KEYS if key in rule
    }
    return _dump(conditions) if conditions else ""


def dump_rules(rules: Sequence[Mapping[str, Any]]) -> str:
    """Render rules as the YAML that `TagHound.rules_from_string` reads back.

    Rules are dumped one at a time and joined with a blank line: PyYAML has no
    way to space out the entries of a single list, and a few thousand lines of
    rules with nothing between them is unreadable.

    Only the condition tree is quoted and reordered; metadata scalars stay bare,
    so `id: food/coffee` reads as itself.

    Example:
        >>> print(dump_rules([{"id": "r", "weight": 3, "or": [{"key": "t", "value": "x"}]}]))
        - id: r
          weight: 3
          or:
            - key: 't'
              value: 'x'
        <BLANKLINE>
    """
    return "\n".join(_dump([_prepared_rule(rule)]) for rule in rules)


def _prepared_rule(rule: Mapping[str, Any]) -> dict[str, Any]:
    """Quote and reorder the condition tree, leaving metadata scalars alone."""
    return {
        key: _ordered_conditions(value) if key in ROOT_KEYS else value
        for key, value in rule.items()
    }
