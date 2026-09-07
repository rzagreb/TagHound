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
                "memo": "annual renewal processed",
                "merchant": "blue bottle",
                "city": "Portland",
                "recurring": False,
                "plan": "",
            },
            {
                "memo": "one-off purchase",
                "merchant": "corner grocer",
                "city": "Denver",
                "recurring": True,
                "plan": "Annual",
            },
            {
                "memo": "auto-renew notice",
                "merchant": "city transit",
                "city": "Seattle",
                "recurring": True,
                "plan": "Monthly",
            },
            {
                "memo": "annual renewal and auto-renew",
                "merchant": "corner grocer",
                "city": "Portland",
                "recurring": False,
                "plan": "",
            },
        ]
    )


def test_create_vector_function_or(df: pd.DataFrame):
    rule = {
        RuleKey.ROOT_OR.value: [
            {
                LogicalOperator.AND.value: [
                    {
                        ComparisonKey.KEY.value: "memo",
                        ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
                        ComparisonKey.VALUE.value: "(?:annual renewal|auto-renew)",
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
                        ComparisonKey.KEY.value: "memo",
                        ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
                        ComparisonKey.VALUE.value: "(?:annual renewal|auto-renew)",
                    },
                    {
                        ComparisonKey.KEY.value: "city",
                        ComparisonKey.OPERATOR.value: ComparisonOperator.IN.value,
                        ComparisonKey.VALUE.value: ["Portland", "Seattle"],
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
        "memo",
        ComparisonOperator.REGEX_MATCH.value,
        "(?:annual renewal|auto-renew)",
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([True, False, True, True], name="memo")
    pd.testing.assert_series_equal(result, expected)


def test_apply_condition_regex_not_match(df: pd.DataFrame):
    result = _make_series_condition(
        df,
        "memo",
        ComparisonOperator.REGEX_NOT_MATCH.value,
        "(?:annual renewal|auto-renew)",
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([False, True, False, False], name="memo")
    pd.testing.assert_series_equal(result, expected)


def test_apply_condition_equal(df: pd.DataFrame):
    result = _make_series_condition(
        df,
        "recurring",
        ComparisonOperator.EQUAL.value,
        False,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([True, False, False, True], name="recurring")
    pd.testing.assert_series_equal(result, expected)


def test_apply_condition_not_equal(df: pd.DataFrame):
    result = _make_series_condition(
        df,
        "recurring",
        ComparisonOperator.NOT_EQUAL.value,
        False,
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([False, True, True, False], name="recurring")
    pd.testing.assert_series_equal(result, expected)


def test_apply_condition_in(df: pd.DataFrame):
    result = _make_series_condition(
        df,
        "city",
        ComparisonOperator.IN.value,
        ["Portland", "Seattle"],
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([True, False, True, True], name="city")
    pd.testing.assert_series_equal(result, expected)


def test_apply_condition_not_in(df: pd.DataFrame):
    result = _make_series_condition(
        df,
        "city",
        ComparisonOperator.NOT_IN.value,
        ["Portland", "Seattle"],
        merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
    )
    expected = pd.Series([False, True, False, False], name="city")
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
                ComparisonKey.KEY.value: "city",
                ComparisonKey.VALUE.value: "Portland",
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
                ComparisonKey.KEY.value: "memo",
                ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
                ComparisonKey.VALUE.value: "(?:annual renewal|auto-renew)",
            },
            {
                ComparisonKey.KEY.value: "city",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "Denver",
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
                ComparisonKey.KEY.value: "memo",
                ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
                ComparisonKey.VALUE.value: "(?:annual renewal|auto-renew)",
            },
            {
                ComparisonKey.KEY.value: "city",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "Portland",
            },
        ]
    }
    result = _evaluate_criteria(df, rule, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)
    expected = pd.Series([True, False, False, True])
    pd.testing.assert_series_equal(result, expected)


def test_evaluate_criteria_invalid(df: pd.DataFrame):
    rule = {
        ComparisonKey.KEY.value: "memo",
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
            "memo": pd.Series(
                [
                    "C++ toolkit, 3 licences",
                    "C# and .NET bundle renewal",
                    "spring sale. #promo",
                    "python course, annual",
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
        (r"\#promo", [False, False, True, False, False]),
        (r"\.?net", [False, True, False, False, False]),
    ],
)
def test_re2_safe_merge_pattern_matches_punctuation_edges(
    punctuation_df: pd.DataFrame, pattern: str, expected: list[bool]
):
    condition = {
        ComparisonKey.KEY.value: "memo",
        ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
        ComparisonKey.VALUE.value: pattern,
    }
    result = _evaluate_criteria(
        punctuation_df, condition, merge_pattern=RE2_SAFE_REGEX_MERGE_PATTERN
    )
    pd.testing.assert_series_equal(result, pd.Series(expected, name="memo"))


@pytest.mark.parametrize("pattern", [r"C\+\+", r"C\#", r"\#promo", "python"])
def test_re2_safe_merge_pattern_agrees_with_lookaround_default(
    punctuation_df: pd.DataFrame, pattern: str
):
    """The RE2-safe spelling must mean the same thing as the look-around default.

    The default cannot run on the pyarrow path at all, so compare against it on an
    object-dtype frame, where pandas falls back to Python `re`.
    """
    condition = {
        ComparisonKey.KEY.value: "memo",
        ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
        ComparisonKey.VALUE.value: pattern,
    }
    object_df = punctuation_df.astype({"memo": "object"})
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
        ComparisonKey.KEY.value: "memo",
        ComparisonKey.OPERATOR.value: ComparisonOperator.REGEX_MATCH.value,
        ComparisonKey.VALUE.value: "python",
    }
    result = _evaluate_criteria(
        punctuation_df, condition, merge_pattern=RE2_SAFE_REGEX_MERGE_PATTERN
    )
    expected = pd.Series([False, False, False, True, False, False], name="memo")
    pd.testing.assert_series_equal(result, expected)


# --- Missing values ---------------------------------------------------------
#
# The regex branch has resolved nulls to "no match" since 0.3.0; these pin the
# same behaviour on the branches that were left leaking NA into the masks.

NULL_ROW = 1
"""Index of the missing value in the fixtures below."""


@pytest.mark.parametrize("dtype", ["Int64", "float64", "object"])
@pytest.mark.parametrize(
    ("operator", "value", "expected"),
    [
        (">", 100, [False, False, True]),
        (">=", 420, [False, False, True]),
        ("<", 100, [True, False, False]),
        ("in", [420], [False, False, True]),
        ("not_in", [420], [True, True, False]),
        ("=", 420, [False, False, True]),
        ("!=", 420, [True, True, False]),
    ],
)
def test_a_missing_number_is_not_a_match(dtype, operator, value, expected):
    frame = pd.DataFrame({"amount": pd.Series([5, None, 420], dtype=dtype)})
    condition = {
        ComparisonKey.KEY.value: "amount",
        ComparisonKey.OPERATOR.value: operator,
        ComparisonKey.VALUE.value: value,
    }
    result = _evaluate_criteria(
        frame, condition, merge_pattern=RE2_SAFE_REGEX_MERGE_PATTERN
    )
    assert list(result) == expected
    # The and/or masks and the final to_numpy(dtype=bool) both refuse NA.
    assert result.dtype == bool


@pytest.mark.parametrize("dtype", ["string[pyarrow]", "object"])
@pytest.mark.parametrize(
    ("operator", "value", "expected"),
    [
        ("=", "a", [True, False]),
        ("!=", "a", [False, True]),
        ("in", ["a"], [True, False]),
        ("~", "a", [True, False]),
        ("!~", "a", [False, True]),
    ],
)
def test_a_missing_string_is_not_a_match(dtype, operator, value, expected):
    frame = pd.DataFrame({"merchant": pd.Series(["a", None], dtype=dtype)})
    condition = {
        ComparisonKey.KEY.value: "merchant",
        ComparisonKey.OPERATOR.value: operator,
        ComparisonKey.VALUE.value: value,
    }
    result = _evaluate_criteria(
        frame, condition, merge_pattern=RE2_SAFE_REGEX_MERGE_PATTERN
    )
    assert list(result) == expected
    assert result.dtype == bool
