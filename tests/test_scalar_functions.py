"""Tests for the scalar function builder and rule serialization."""

# ruff: noqa: D103, ANN201 -- per-test docstrings are noise; test names carry the intent.

from __future__ import annotations

import pytest

from taghound.constants import (
    DEFAULT_REGEX_MERGE_PATTERN,
    ComparisonKey,
    ComparisonOperator,
    LogicalOperator,
    RuleKey,
)
from taghound.exceptions import (
    InvalidOperatorError,
    MissingRootConditionError,
    RuleAndOrTogetherError,
)
from taghound.scalar.function_builder import (
    _create_logic_function,
    _make_comparison_condition_function,
    _parse_conditions,
    _validate_and_normalize_comparison_condition_data,
)
from taghound.serializers import get_required_keys_from_conditions, to_tag_rule

# --- to_tag_rule ---


def test_to_tag_rule_valid_and():
    data = {
        RuleKey.ID.value: "python",
        RuleKey.WEIGHT.value: 10,
        RuleKey.ROOT_AND.value: [
            {
                ComparisonKey.KEY.value: "language",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "python",
            },
            {
                ComparisonKey.KEY.value: "year",
                ComparisonKey.OPERATOR.value: ComparisonOperator.GREATER_THAN.value,
                ComparisonKey.VALUE.value: 1990,
            },
        ],
    }
    rule = to_tag_rule(data)
    assert rule.id == "python"
    assert rule.weight == 10.0
    assert "language" in rule.required_fields
    assert "year" in rule.required_fields


def test_to_tag_rule_valid_or():
    data = {
        RuleKey.ID.value: "python",
        RuleKey.WEIGHT.value: 10,
        RuleKey.ROOT_OR.value: [
            {
                ComparisonKey.KEY.value: "language",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "python",
            },
            {
                ComparisonKey.KEY.value: "year",
                ComparisonKey.OPERATOR.value: ComparisonOperator.GREATER_THAN.value,
                ComparisonKey.VALUE.value: 1990,
            },
        ],
    }
    rule = to_tag_rule(data)
    assert rule.id == "python"
    assert rule.weight == 10.0
    assert "language" in rule.required_fields
    assert "year" in rule.required_fields


def test_to_tag_rule_invalid_tag():
    data = {
        RuleKey.ID.value: 123,
        RuleKey.WEIGHT.value: 10,
        RuleKey.ROOT_AND.value: [
            {
                ComparisonKey.KEY.value: "language",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "python",
            },
        ],
    }
    with pytest.raises(ValueError):
        to_tag_rule(data)


def test_to_tag_rule_invalid_weight():
    data = {
        RuleKey.ID.value: "python",
        RuleKey.WEIGHT.value: "heavy",
        RuleKey.ROOT_AND.value: [
            {
                ComparisonKey.KEY.value: "language",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "python",
            },
        ],
    }
    with pytest.raises(ValueError):
        to_tag_rule(data)


def test_to_tag_rule_and_or_together():
    data = {
        RuleKey.ID.value: "python",
        RuleKey.WEIGHT.value: 10,
        RuleKey.ROOT_AND.value: [
            {
                ComparisonKey.KEY.value: "language",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "python",
            },
        ],
        RuleKey.ROOT_OR.value: [
            {
                ComparisonKey.KEY.value: "year",
                ComparisonKey.OPERATOR.value: ComparisonOperator.GREATER_THAN.value,
                ComparisonKey.VALUE.value: 1990,
            },
        ],
    }
    with pytest.raises(RuleAndOrTogetherError):
        to_tag_rule(data)


def test_to_tag_rule_invalid_regex_fails_at_load():
    data = {
        RuleKey.ID.value: "bad_regex",
        RuleKey.ROOT_AND.value: [
            {
                ComparisonKey.KEY.value: "text",
                ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
                ComparisonKey.VALUE.value: "([unclosed",
            }
        ],
    }
    with pytest.raises(ValueError):
        to_tag_rule(data)


def test_to_tag_rule_nested_invalid_operator_fails_at_load():
    data = {
        RuleKey.ID.value: "bad_nested_op",
        RuleKey.ROOT_AND.value: [
            {
                "invalid_op": [
                    {
                        ComparisonKey.KEY.value: "language",
                        ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                        ComparisonKey.VALUE.value: "python",
                    }
                ]
            }
        ],
    }
    with pytest.raises(InvalidOperatorError):
        to_tag_rule(data)


def test_to_tag_rule_missing_root_condition():
    data = {
        RuleKey.ID.value: "python",
        RuleKey.WEIGHT.value: 10,
    }
    with pytest.raises(MissingRootConditionError):
        to_tag_rule(data)


# --- _create_logic_function ---


def test_create_logic_function_and():
    params = {
        LogicalOperator.AND.value: [
            {
                ComparisonKey.KEY.value: "language",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "python",
            },
            {
                ComparisonKey.KEY.value: "year",
                ComparisonKey.OPERATOR.value: ComparisonOperator.GREATER_THAN.value,
                ComparisonKey.VALUE.value: 1990,
            },
        ]
    }
    condition_fn = _create_logic_function(
        params, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN
    )
    assert callable(condition_fn)


def test_create_logic_function_or():
    params = {
        LogicalOperator.OR.value: [
            {
                ComparisonKey.KEY.value: "language",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "python",
            },
            {
                ComparisonKey.KEY.value: "year",
                ComparisonKey.OPERATOR.value: ComparisonOperator.GREATER_THAN.value,
                ComparisonKey.VALUE.value: 1990,
            },
        ]
    }
    condition_fn = _create_logic_function(
        params, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN
    )
    assert callable(condition_fn)


def test_create_logic_function_invalid_operator():
    params = {
        "invalid_op": [
            {
                ComparisonKey.KEY.value: "language",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "python",
            },
        ]
    }
    with pytest.raises(InvalidOperatorError):
        _create_logic_function(params, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)  # type: ignore


# --- _parse_conditions ---


def test_parse_conditions_valid():
    elems = [
        {
            ComparisonKey.KEY.value: "language",
            ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
            ComparisonKey.VALUE.value: "python",
        },
        {
            ComparisonKey.KEY.value: "year",
            ComparisonKey.OPERATOR.value: ComparisonOperator.GREATER_THAN.value,
            ComparisonKey.VALUE.value: 1990,
        },
    ]
    condition_fns = _parse_conditions(elems, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)
    assert len(condition_fns) == 2
    for fn in condition_fns:
        assert callable(fn)


def test_parse_conditions_invalid_not_list():
    elems = {
        ComparisonKey.KEY.value: "language",
        ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
        ComparisonKey.VALUE.value: "python",
    }
    with pytest.raises(TypeError):
        _parse_conditions(elems, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)  # type: ignore


def test_parse_conditions_empty_dict():
    elems = [{}]
    with pytest.raises(ValueError):
        _parse_conditions(elems, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)  # type: ignore


# --- _make_comparison_condition_function ---


def test_comparison_condition_equal():
    fn = _make_comparison_condition_function(
        "language",
        "python",
        ComparisonOperator.EQUAL.value,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(fn)
    assert fn({"language": "python"})
    assert not fn({"language": "java"})


def test_comparison_condition_not_equal():
    fn = _make_comparison_condition_function(
        "language",
        "python",
        ComparisonOperator.NOT_EQUAL.value,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(fn)
    assert not fn({"language": "python"})
    assert fn({"language": "java"})


def test_comparison_condition_greater_than():
    fn = _make_comparison_condition_function(
        "year",
        1990,
        ComparisonOperator.GREATER_THAN.value,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(fn)
    assert fn({"year": 2000})
    assert not fn({"year": 1980})


def test_comparison_condition_greater_than_or_equal():
    fn = _make_comparison_condition_function(
        "year",
        1990,
        ComparisonOperator.GREATER_THAN_OR_EQUAL.value,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(fn)
    assert fn({"year": 2000})
    assert fn({"year": 1990})
    assert not fn({"year": 1980})


def test_comparison_condition_less_than():
    fn = _make_comparison_condition_function(
        "year",
        2000,
        ComparisonOperator.LESS_THAN.value,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(fn)
    assert fn({"year": 1990})
    assert not fn({"year": 2000})


def test_comparison_condition_less_than_or_equal():
    fn = _make_comparison_condition_function(
        "year",
        2000,
        ComparisonOperator.LESS_THAN_OR_EQUAL.value,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(fn)
    assert fn({"year": 1990})
    assert fn({"year": 2000})
    assert not fn({"year": 2010})


def test_comparison_condition_in():
    fn = _make_comparison_condition_function(
        "language",
        ["python", "java"],
        ComparisonOperator.IN.value,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(fn)
    assert fn({"language": "python"})
    assert not fn({"language": "c++"})


def test_comparison_condition_not_in():
    fn = _make_comparison_condition_function(
        "language",
        ["python", "java"],
        ComparisonOperator.NOT_IN.value,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(fn)
    assert not fn({"language": "python"})
    assert fn({"language": "c++"})


def test_comparison_condition_is():
    fn = _make_comparison_condition_function(
        "is_active",
        True,
        ComparisonOperator.IS.value,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(fn)
    assert fn({"is_active": True})
    assert not fn({"is_active": False})


def test_comparison_condition_is_not():
    fn = _make_comparison_condition_function(
        "is_active",
        True,
        ComparisonOperator.IS_NOT.value,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(fn)
    assert not fn({"is_active": True})
    assert fn({"is_active": False})


def test_comparison_condition_regex_match():
    fn = _make_comparison_condition_function(
        "text",
        "python",
        ComparisonOperator.REGEX_MATCH.value,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(fn)
    assert fn({"text": "I love python"})
    assert not fn({"text": "I love java"})


def test_comparison_condition_regex_not_match():
    fn = _make_comparison_condition_function(
        "text",
        "python",
        ComparisonOperator.REGEX_NOT_MATCH.value,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(fn)
    assert not fn({"text": "I love python"})
    assert fn({"text": "I love java"})


# --- _validate_and_normalize_comparison_condition_data ---


def test_validate_and_normalize_regex():
    value, operator = _validate_and_normalize_comparison_condition_data(
        "python",
        ComparisonOperator.REGEX_MATCH,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(value.match)
    assert operator == ComparisonOperator.REGEX_MATCH


def test_validate_and_normalize_regex_list():
    value, operator = _validate_and_normalize_comparison_condition_data(
        ["python", "java"],
        ComparisonOperator.REGEX_MATCH,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert callable(value.match)
    assert operator == ComparisonOperator.REGEX_MATCH


def test_validate_and_normalize_numeric():
    value, operator = _validate_and_normalize_comparison_condition_data(
        100,
        ComparisonOperator.GREATER_THAN,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert value == 100
    assert operator == ComparisonOperator.GREATER_THAN


def test_validate_and_normalize_numeric_invalid():
    with pytest.raises(ValueError):
        _validate_and_normalize_comparison_condition_data(
            "100",
            ComparisonOperator.GREATER_THAN,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )


def test_validate_and_normalize_string():
    value, operator = _validate_and_normalize_comparison_condition_data(
        "python",
        ComparisonOperator.EQUAL,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert value == "python"
    assert operator == ComparisonOperator.EQUAL


def test_validate_and_normalize_list():
    value, operator = _validate_and_normalize_comparison_condition_data(
        ["python", "java"],
        ComparisonOperator.IN,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert value == {"python", "java"}
    assert operator == ComparisonOperator.IN


def test_validate_and_normalize_list_invalid():
    with pytest.raises(ValueError):
        _validate_and_normalize_comparison_condition_data(
            "python",
            ComparisonOperator.IN,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )


def test_validate_and_normalize_boolean_equal():
    value, operator = _validate_and_normalize_comparison_condition_data(
        True, ComparisonOperator.EQUAL, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN
    )
    assert value is True
    assert operator == ComparisonOperator.IS


def test_validate_and_normalize_boolean_not_equal():
    value, operator = _validate_and_normalize_comparison_condition_data(
        True,
        ComparisonOperator.NOT_EQUAL,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    assert value is True
    assert operator == ComparisonOperator.IS_NOT


# --- get_required_keys_from_conditions ---


def test_get_required_keys_single():
    conditions = [
        {
            ComparisonKey.KEY.value: "language",
            ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
            ComparisonKey.VALUE.value: "python",
        }
    ]
    keys = get_required_keys_from_conditions(conditions)
    assert "language" in keys


def test_get_required_keys_nested():
    conditions = [
        {
            ComparisonKey.KEY.value: "language",
            ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
            ComparisonKey.VALUE.value: "python",
        },
        {
            LogicalOperator.OR.value: [
                {
                    ComparisonKey.KEY.value: "year",
                    ComparisonKey.OPERATOR.value: ComparisonOperator.GREATER_THAN.value,
                    ComparisonKey.VALUE.value: 1990,
                },
                {
                    ComparisonKey.KEY.value: "experience",
                    ComparisonKey.OPERATOR.value: ComparisonOperator.GREATER_THAN_OR_EQUAL.value,
                    ComparisonKey.VALUE.value: 5,
                },
            ]
        },
    ]
    keys = get_required_keys_from_conditions(conditions)
    assert "language" in keys
    assert "year" in keys
    assert "experience" in keys


def test_get_required_keys_key_not_first():
    conditions = [
        {
            ComparisonKey.OPERATOR.value: ComparisonOperator.IN.value,
            ComparisonKey.VALUE.value: ["Washington", "New York"],
            ComparisonKey.KEY.value: "location",
        }
    ]
    keys = get_required_keys_from_conditions(conditions)
    assert keys == {"location"}


def test_get_required_keys_ignores_mappings_in_value():
    conditions = [
        {
            ComparisonKey.VALUE.value: [{ComparisonKey.KEY.value: "oops"}],
            ComparisonKey.OPERATOR.value: ComparisonOperator.IN.value,
            ComparisonKey.KEY.value: "location",
        }
    ]
    keys = get_required_keys_from_conditions(conditions)
    assert keys == {"location"}
