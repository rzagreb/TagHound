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

RULE_ID = "sector_group/advanced_filtering2"


def sample_rule_data() -> list[dict[str, Any]]:
    """Build the canonical nested rule definition used by the loading tests."""
    return [
        {
            RuleKey.ID.value: RULE_ID,
            RuleKey.LABEL.value: "Advanced Filtering 2",
            RuleKey.WEIGHT.value: -150,
            LogicalOperator.OR.value: [
                {
                    LogicalOperator.AND.value: [
                        {
                            ComparisonKey.KEY.value: "description",
                            ComparisonKey.OPERATOR.value: "~",
                            ComparisonKey.VALUE.value: "(government contract|public sector)",
                        }
                    ]
                },
                {
                    LogicalOperator.AND.value: [
                        {
                            ComparisonKey.KEY.value: "description",
                            ComparisonKey.OPERATOR.value: "~",
                            ComparisonKey.VALUE.value: "(government contract|public sector)",
                        },
                        {
                            ComparisonKey.KEY.value: "title",
                            ComparisonKey.OPERATOR.value: "!~",
                            ComparisonKey.VALUE.value: [
                                "data",
                                "analyst",
                                "intern",
                                "junior",
                            ],
                        },
                        {
                            ComparisonKey.KEY.value: "location",
                            ComparisonKey.OPERATOR.value: "in",
                            ComparisonKey.VALUE.value: ["Washington", "New York"],
                        },
                        {
                            LogicalOperator.OR.value: [
                                {
                                    ComparisonKey.KEY.value: "clearance_required",
                                    ComparisonKey.OPERATOR.value: "=",
                                    ComparisonKey.VALUE.value: "false",
                                },
                                {
                                    LogicalOperator.AND.value: [
                                        {
                                            ComparisonKey.KEY.value: "clearance_required",
                                            ComparisonKey.OPERATOR.value: "=",
                                            ComparisonKey.VALUE.value: "true",
                                        },
                                        {
                                            ComparisonKey.KEY.value: "clearance_level",
                                            ComparisonKey.OPERATOR.value: "=",
                                            ComparisonKey.VALUE.value: "Top Secret",
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
            label="Advanced Filtering 2",
            weight=-150,
            required_fields={
                "description",
                "title",
                "location",
                "clearance_required",
                "clearance_level",
            },
            scalar_check=lambda d: (
                bool(re.search("(government contract|public sector)", d["description"]))
                and not bool(re.search("(data|analyst|intern|junior)", d["title"]))
                and d["location"] in ["Washington", "New York"]
                and (
                    d["clearance_required"] == "false"
                    or (
                        d["clearance_required"] == "true"
                        and d["clearance_level"] == "Top Secret"
                    )
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
        "description": "government contract",
        "title": "senior developer",
        "location": "Washington",
        "clearance_required": "true",
        "clearance_level": "Top Secret",
    }
    assert th.find_all_tags(data) == (RULE_ID,)

    data = {
        "description": "public sector",
        "title": "junior developer",
        "location": "New York",
        "clearance_required": "false",
        "clearance_level": "Top Secret",
    }
    assert th.find_all_tags(data) == ()  # title contains "junior"

    data = {
        "description": "public sector",
        "title": "senior developer",
        "location": "Chicago",
        "clearance_required": "false",
        "clearance_level": "Top Secret",
    }
    assert th.find_all_tags(data) == ()  # location is not Washington or New York


def test_find_all_tags_multithread(th: TagHound):
    data = {
        "description": "government contract",
        "title": "senior developer",
        "location": "Washington",
        "clearance_required": "true",
        "clearance_level": "Top Secret",
    }
    assert th.find_all_tags(data, multi_threaded=True) == (RULE_ID,)

    data = {
        "description": "public sector",
        "title": "junior developer",
        "location": "New York",
        "clearance_required": "false",
        "clearance_level": "Top Secret",
    }
    assert th.find_all_tags(data, multi_threaded=True) == ()  # title contains "junior"


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
