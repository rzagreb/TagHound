"""Writing rules back out as YAML, and reading what was written."""

import pytest
import yaml

from taghound import TagHound
from taghound.exceptions import DuplicateRuleIdError
from taghound.serializers import dump_conditions, dump_rules

RULES = [
    {
        "id": "food/coffee",
        "label": "Coffee",
        "info": "",
        "weight": 3,
        "or": [
            {"key": "merchant", "op": "~", "value": ["blue bottle", "C\\+\\+ cafe"]},
            {"and": [{"key": "amount", "op": ">", "value": 3}]},
        ],
    },
    {
        "id": "alerts/big",
        "label": "Big spend",
        "info": "Over 100",
        "weight": -8,
        "or": [{"key": "amount", "op": ">", "value": 100}],
    },
]


def test_dumped_rules_load_back_unchanged():
    """The round trip is the contract: what comes out must go back in."""
    assert yaml.safe_load(dump_rules(RULES)) == RULES


def test_dumped_rules_rebuild_a_working_taghound():
    th = TagHound.rules_from_string(dump_rules(RULES))
    assert th.find_all_tags({"merchant": "Blue Bottle", "amount": 5.75}) == (
        "food/coffee",
    )


def test_every_condition_scalar_is_quoted():
    """A bare regex is one stray character from meaning something else in YAML."""
    conditions = [
        line for line in dump_rules(RULES).splitlines() if line.lstrip().startswith("-")
    ]
    patterns = [line for line in conditions if "blue bottle" in line]
    # Depth: the rule's `-`, then `or:`, its condition, and the value list.
    assert patterns == ["        - 'blue bottle'"]


def test_metadata_scalars_stay_bare():
    assert "id: food/coffee" in dump_rules(RULES)


def test_list_items_are_indented_under_their_key():
    """PyYAML writes sequences indentless, which reads as though they escaped."""
    assert "\n  or:\n    - key: 'merchant'" in dump_rules(RULES)


def test_long_patterns_are_never_folded():
    """A fold inside a regex means an edit near it silently changes the pattern."""
    long_pattern = "a" * 400
    rule = {"id": "r", "or": [{"key": "text", "op": "~", "value": long_pattern}]}
    assert f"'{long_pattern}'" in dump_rules([rule])


def test_conditions_are_written_in_reading_order():
    """Stored rules come back in whatever order they were written in."""
    rule = {"id": "r", "or": [{"value": "x", "op": "~", "key": "text"}]}
    assert (
        dump_conditions(rule) == "or:\n  - key: 'text'\n    op: '~'\n    value: 'x'\n"
    )


def test_a_rule_without_conditions_dumps_to_nothing():
    assert dump_conditions({"id": "r", "weight": 1}) == ""


def test_duplicate_ids_are_refused_on_load():
    """Left alone they collapse into one weight map entry and two tag columns."""
    with pytest.raises(DuplicateRuleIdError, match="food/coffee"):
        TagHound.rules_from_string(dump_rules([RULES[0], RULES[0]]))
