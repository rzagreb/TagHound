"""Tests for the TagHound class: tag search and rule loading."""

# ruff: noqa: D103, ANN201 -- per-test docstrings are noise; test names carry the intent.

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any

import pytest
import yaml

from taghound.constants import ComparisonKey, LogicalOperator, RuleKey
from taghound.models import TagRule
from taghound.taghound import TagHound

RULE_ID = "spend/recurring_review"


def sample_rule_data() -> list[dict[str, Any]]:
    """Build the canonical nested rule definition used by the loading tests."""
    return [
        {
            RuleKey.ID.value: RULE_ID,
            RuleKey.LABEL.value: "Recurring Review",
            RuleKey.WEIGHT.value: -150,
            LogicalOperator.OR.value: [
                {
                    LogicalOperator.AND.value: [
                        {
                            ComparisonKey.KEY.value: "memo",
                            ComparisonKey.OPERATOR.value: "~",
                            ComparisonKey.VALUE.value: "(annual renewal|auto-renew)",
                        }
                    ]
                },
                {
                    LogicalOperator.AND.value: [
                        {
                            ComparisonKey.KEY.value: "memo",
                            ComparisonKey.OPERATOR.value: "~",
                            ComparisonKey.VALUE.value: "(annual renewal|auto-renew)",
                        },
                        {
                            ComparisonKey.KEY.value: "merchant",
                            ComparisonKey.OPERATOR.value: "!~",
                            ComparisonKey.VALUE.value: [
                                "grocer",
                                "transit",
                                "pharmacy",
                                "parking",
                            ],
                        },
                        {
                            ComparisonKey.KEY.value: "city",
                            ComparisonKey.OPERATOR.value: "in",
                            ComparisonKey.VALUE.value: ["Portland", "Seattle"],
                        },
                        {
                            LogicalOperator.OR.value: [
                                {
                                    ComparisonKey.KEY.value: "recurring",
                                    ComparisonKey.OPERATOR.value: "=",
                                    ComparisonKey.VALUE.value: "false",
                                },
                                {
                                    LogicalOperator.AND.value: [
                                        {
                                            ComparisonKey.KEY.value: "recurring",
                                            ComparisonKey.OPERATOR.value: "=",
                                            ComparisonKey.VALUE.value: "true",
                                        },
                                        {
                                            ComparisonKey.KEY.value: "plan",
                                            ComparisonKey.OPERATOR.value: "=",
                                            ComparisonKey.VALUE.value: "Annual",
                                        },
                                    ]
                                },
                            ]
                        },
                    ]
                },
            ],
        }
    ]


@pytest.fixture
def rules() -> list[TagRule]:
    """The sample rule as a hand-built TagRule with an equivalent scalar check."""
    return [
        TagRule(
            id=RULE_ID,
            label="Recurring Review",
            weight=-150,
            required_fields={
                "memo",
                "merchant",
                "city",
                "recurring",
                "plan",
            },
            scalar_check=lambda d: (
                bool(re.search("(annual renewal|auto-renew)", d["memo"]))
                and not bool(
                    re.search("(grocer|transit|pharmacy|parking)", d["merchant"])
                )
                and d["city"] in ["Portland", "Seattle"]
                and (
                    d["recurring"] == "false"
                    or (d["recurring"] == "true" and d["plan"] == "Annual")
                )
            ),
        )
    ]


@pytest.fixture
def th(rules: list[TagRule]) -> TagHound:
    """A TagHound built from the sample rule."""
    return TagHound(rules=rules)


def test_find_all_tags(th: TagHound):
    data = {
        "memo": "annual renewal",
        "merchant": "blue bottle",
        "city": "Portland",
        "recurring": "true",
        "plan": "Annual",
    }
    assert th.find_all_tags(data) == (RULE_ID,)

    data = {
        "memo": "auto-renew",
        "merchant": "corner grocer",
        "city": "Seattle",
        "recurring": "false",
        "plan": "Annual",
    }
    assert th.find_all_tags(data) == ()  # merchant is on the excluded list

    data = {
        "memo": "auto-renew",
        "merchant": "blue bottle",
        "city": "Austin",
        "recurring": "false",
        "plan": "Annual",
    }
    assert th.find_all_tags(data) == ()  # city is not Portland or Seattle


def test_rules_from_yaml(tmp_path: Path, rules: list[TagRule]):
    path = tmp_path / "rules.yml"
    path.write_text(yaml.safe_dump(sample_rule_data()))

    taghound = TagHound.rules_from_yaml(str(path))

    assert len(taghound.rules) == len(rules)
    assert taghound.rules[0].id == rules[0].id
    assert taghound.rules[0].weight == rules[0].weight


def test_rules_from_json(tmp_path: Path, rules: list[TagRule]):
    path = tmp_path / "rules.json"
    path.write_text(json.dumps(sample_rule_data()))

    taghound = TagHound.rules_from_json(str(path))

    assert len(taghound.rules) == len(rules)
    assert taghound.rules[0].id == rules[0].id
    assert taghound.rules[0].weight == rules[0].weight


def test_rules_from_yaml_invalid_version(tmp_path: Path):
    path = tmp_path / "rules.yml"
    path.write_text(yaml.safe_dump(sample_rule_data()))

    with pytest.raises(ValueError):
        TagHound.rules_from_yaml(str(path), version="2")


def test_rules_from_json_invalid_version(tmp_path: Path):
    path = tmp_path / "rules.json"
    path.write_text(json.dumps(sample_rule_data()))

    with pytest.raises(ValueError):
        TagHound.rules_from_json(str(path), version="2")


def test_loading_rule_with_comments(tmp_path: Path):
    rule_data = sample_rule_data()
    rule_data[0][RuleKey.INFO.value] = "This is a comment"
    path = tmp_path / "rules.yml"
    path.write_text(yaml.safe_dump(rule_data))

    taghound = TagHound.rules_from_yaml(str(path))

    assert taghound.rules[0].info == "This is a comment"
