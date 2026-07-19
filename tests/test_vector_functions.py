"""Tests for the vector (DataFrame) function builder."""

# ruff: noqa: D103, ANN201 -- per-test docstrings are noise; test names carry the intent.

from __future__ import annotations

import pandas as pd
import pytest

from taghound.constants import (
    DEFAULT_REGEX_MERGE_PATTERN,
    ComparisonKey,
    ComparisonOperator,
    LogicalOperator,
    RuleKey,
)
from taghound.vector.function_builder import (
    _evaluate_criteria,
    _make_series_condition,
    create_vector_function,
)


@pytest.fixture
def df() -> pd.DataFrame:
    """Sample records covering regex, membership, and boolean conditions."""
    return pd.DataFrame(
        [
            {
                "description": "working on government contract",
                "title": "data scientist",
                "location": "Washington",
                "clearance_required": False,
                "clearance_level": "",
            },
            {
                "description": "private sector project",
                "title": "senior analyst",
                "location": "San Francisco",
                "clearance_required": True,
                "clearance_level": "Top Secret",
            },
            {
                "description": "public sector analysis",
                "title": "intern",
                "location": "New York",
                "clearance_required": True,
                "clearance_level": "Confidential",
            },
            {
                "description": "public sector and government contract",
                "title": "junior developer",
                "location": "Washington",
                "clearance_required": False,
                "clearance_level": "",
            },
        ]
    )


def test_create_vector_function_or(df: pd.DataFrame):
    rule = {
        RuleKey.ROOT_OR.value: [
            {
                LogicalOperator.AND.value: [
                    {
                        ComparisonKey.KEY.value: "description",
                        ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
                        ComparisonKey.VALUE.value: "(?:government contract|public sector)",
                    }
                ]
            }
        ]
    }
    func = create_vector_function(rule, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)
    result = func(df)
    expected = pd.Series([True, False, True, True])
    pd.testing.assert_series_equal(result, expected)


def test_create_vector_function_and(df: pd.DataFrame):
    rule = {
        RuleKey.ROOT_AND.value: [
            {
                LogicalOperator.AND.value: [
                    {
                        ComparisonKey.KEY.value: "description",
                        ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
                        ComparisonKey.VALUE.value: "(?:government contract|public sector)",
                    },
                    {
                        ComparisonKey.KEY.value: "location",
                        ComparisonKey.OPERATOR.value: ComparisonOperator.IN.value,
                        ComparisonKey.VALUE.value: ["Washington", "New York"],
                    },
                ]
            }
        ]
    }
    func = create_vector_function(rule, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)
    result = func(df)
    expected = pd.Series([True, False, True, True])
    pd.testing.assert_series_equal(result, expected)


def test_apply_condition_regex_match(df: pd.DataFrame):
    result = _make_series_condition(
        df,
        "description",
        ComparisonOperator.REGEX_MATCH.value,
        "(?:government contract|public sector)",
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([True, False, True, True], name="description")
    pd.testing.assert_series_equal(result, expected)


def test_apply_condition_regex_not_match(df: pd.DataFrame):
    result = _make_series_condition(
        df,
        "description",
        ComparisonOperator.REGEX_NOT_MATCH.value,
        "(?:government contract|public sector)",
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([False, True, False, False], name="description")
    pd.testing.assert_series_equal(result, expected)


def test_apply_condition_equal(df: pd.DataFrame):
    result = _make_series_condition(
        df,
        "clearance_required",
        ComparisonOperator.EQUAL.value,
        False,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([True, False, False, True], name="clearance_required")
    pd.testing.assert_series_equal(result, expected)


def test_apply_condition_not_equal(df: pd.DataFrame):
    result = _make_series_condition(
        df,
        "clearance_required",
        ComparisonOperator.NOT_EQUAL.value,
        False,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([False, True, True, False], name="clearance_required")
    pd.testing.assert_series_equal(result, expected)


def test_apply_condition_in(df: pd.DataFrame):
    result = _make_series_condition(
        df,
        "location",
        ComparisonOperator.IN.value,
        ["Washington", "New York"],
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([True, False, True, True], name="location")
    pd.testing.assert_series_equal(result, expected)


def test_apply_condition_not_in(df: pd.DataFrame):
    result = _make_series_condition(
        df,
        "location",
        ComparisonOperator.NOT_IN.value,
        ["Washington", "New York"],
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([False, True, False, False], name="location")
    pd.testing.assert_series_equal(result, expected)


def test_apply_condition_greater_than():
    frame = pd.DataFrame([{"height": 10}, {"height": 30}])
    result = _make_series_condition(
        frame,
        "height",
        ComparisonOperator.GREATER_THAN.value,
        20,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([False, True], name="height")
    pd.testing.assert_series_equal(result, expected)


def test_evaluate_criteria_missing_op_defaults_to_equal(df: pd.DataFrame):
    rule = {
        LogicalOperator.AND.value: [
            {
                ComparisonKey.KEY.value: "location",
                ComparisonKey.VALUE.value: "Washington",
            }
        ]
    }
    result = _evaluate_criteria(df, rule, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)
    expected = pd.Series([True, False, False, True])
    pd.testing.assert_series_equal(result, expected)


def test_evaluate_criteria_or(df: pd.DataFrame):
    rule = {
        LogicalOperator.OR.value: [
            {
                ComparisonKey.KEY.value: "description",
                ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
                ComparisonKey.VALUE.value: "(?:government contract|public sector)",
            },
            {
                ComparisonKey.KEY.value: "location",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "San Francisco",
            },
        ]
    }
    result = _evaluate_criteria(df, rule, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)
    expected = pd.Series([True, True, True, True])
    pd.testing.assert_series_equal(result, expected)


def test_evaluate_criteria_and(df: pd.DataFrame):
    rule = {
        LogicalOperator.AND.value: [
            {
                ComparisonKey.KEY.value: "description",
                ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
                ComparisonKey.VALUE.value: "(?:government contract|public sector)",
            },
            {
                ComparisonKey.KEY.value: "location",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "Washington",
            },
        ]
    }
    result = _evaluate_criteria(df, rule, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)
    expected = pd.Series([True, False, False, True])
    pd.testing.assert_series_equal(result, expected)


def test_evaluate_criteria_invalid(df: pd.DataFrame):
    rule = {
        ComparisonKey.KEY.value: "description",
        ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
    }
    with pytest.raises(ValueError):
        _evaluate_criteria(df, rule, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)  # type: ignore


def test_create_vector_function_invalid_rule():
    rule = {"invalid_key": []}
    with pytest.raises(ValueError):
        create_vector_function(rule, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)
