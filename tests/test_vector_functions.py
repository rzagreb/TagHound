"""Tests for the vector (DataFrame) function builder."""

# ruff: noqa: D103, ANN201 -- per-test docstrings are noise; test names carry the intent.

from __future__ import annotations

import pandas as pd
import pytest

from taghound.constants import (
    DEFAULT_REGEX_MERGE_PATTERN,
    RE2_SAFE_REGEX_MERGE_PATTERN,
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


@pytest.fixture
def punctuation_df() -> pd.DataFrame:
    """Descriptions whose signal sits on a punctuation boundary.

    The column is `string[pyarrow]` on purpose: that is what routes `str.contains`
    onto RE2, the engine the vector merge pattern has to stay legal in.
    """
    return pd.DataFrame(
        {
            "description": pd.Series(
                [
                    "we use C++ and rust here",
                    "experience with C# and .NET",
                    "great benefits. #LI-Remote",
                    "python developer wanted",
                    "no signal in this one",
                ],
                dtype="string[pyarrow]",
            )
        }
    )


@pytest.mark.parametrize(
    ("pattern", "expected"),
    [
        (r"C\+\+", [True, False, False, False, False]),
        (r"C\#", [False, True, False, False, False]),
        (r"\#LI-Remote", [False, False, True, False, False]),
        (r"\.?net", [False, True, False, False, False]),
    ],
)
def test_re2_safe_merge_pattern_matches_punctuation_edges(
    punctuation_df: pd.DataFrame, pattern: str, expected: list[bool]
):
    condition = {
        ComparisonKey.KEY.value: "description",
        ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
        ComparisonKey.VALUE.value: pattern,
    }
    result = _evaluate_criteria(
        punctuation_df, condition, merge_pattern=RE2_SAFE_REGEX_MERGE_PATTERN
    )
    pd.testing.assert_series_equal(result, pd.Series(expected, name="description"))


@pytest.mark.parametrize("pattern", [r"C\+\+", r"C\#", r"\#LI-Remote", "python"])
def test_re2_safe_merge_pattern_agrees_with_lookaround_default(
    punctuation_df: pd.DataFrame, pattern: str
):
    """The RE2-safe spelling must mean the same thing as the look-around default.

    The default cannot run on the pyarrow path at all, so compare against it on an
    object-dtype frame, where pandas falls back to Python `re`.
    """
    condition = {
        ComparisonKey.KEY.value: "description",
        ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
        ComparisonKey.VALUE.value: pattern,
    }
    object_df = punctuation_df.astype({"description": "object"})
    lookaround = _evaluate_criteria(
        object_df, condition, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN
    )
    re2_safe = _evaluate_criteria(
        punctuation_df, condition, merge_pattern=RE2_SAFE_REGEX_MERGE_PATTERN
    )
    pd.testing.assert_series_equal(re2_safe, lookaround)


def test_word_bounded_pattern_is_not_over_matched(punctuation_df: pd.DataFrame):
    """`python` must not match inside `micropython` under the RE2-safe boundaries."""
    punctuation_df.loc[len(punctuation_df)] = "micropython only shop"
    condition = {
        ComparisonKey.KEY.value: "description",
        ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
        ComparisonKey.VALUE.value: "python",
    }
    result = _evaluate_criteria(
        punctuation_df, condition, merge_pattern=RE2_SAFE_REGEX_MERGE_PATTERN
    )
    expected = pd.Series([False, False, False, True, False, False], name="description")
    pd.testing.assert_series_equal(result, expected)
