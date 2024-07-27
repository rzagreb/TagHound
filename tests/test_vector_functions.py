import unittest

import context  # type: ignore # noqa: F401
import pandas as pd

from taghound.constants import (
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


class TestVectorFunction(unittest.TestCase):
    def setUp(self):
        self.data = [
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
        self.df = pd.DataFrame(self.data)

    def test_create_vector_function_or(self):
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
        func = create_vector_function(rule)
        result = func(self.df)
        expected = pd.Series([True, False, True, True])
        pd.testing.assert_series_equal(result, expected)

    def test_create_vector_function_and(self):
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
        func = create_vector_function(rule)
        result = func(self.df)
        expected = pd.Series([True, False, True, True])
        pd.testing.assert_series_equal(result, expected)

    def test_apply_condition_regex_match(self):
        result = _make_series_condition(
            self.df,
            "description",
            ComparisonOperator.REGEX_MATCH.value,
            "(?:government contract|public sector)",
        )
        expected = pd.Series([True, False, True, True], name="description")
        pd.testing.assert_series_equal(result, expected)

    def test_apply_condition_regex_not_match(self):
        result = _make_series_condition(
            self.df,
            "description",
            ComparisonOperator.REGEX_NOT_MATCH.value,
            "(?:government contract|public sector)",
        )
        expected = pd.Series([False, True, False, False], name="description")
        pd.testing.assert_series_equal(result, expected)

    def test_apply_condition_equal(self):
        result = _make_series_condition(
            self.df,
            "clearance_required",
            ComparisonOperator.EQUAL.value,
            False,
        )
        expected = pd.Series([True, False, False, True], name="clearance_required")
        pd.testing.assert_series_equal(result, expected)

    def test_apply_condition_not_equal(self):
        result = _make_series_condition(
            self.df,
            "clearance_required",
            ComparisonOperator.NOT_EQUAL.value,
            False,
        )
        expected = pd.Series([False, True, True, False], name="clearance_required")
        pd.testing.assert_series_equal(result, expected)

    def test_apply_condition_in(self):
        result = _make_series_condition(
            self.df, "location", ComparisonOperator.IN.value, ["Washington", "New York"]
        )
        expected = pd.Series([True, False, True, True], name="location")
        pd.testing.assert_series_equal(result, expected)

    def test_apply_condition_not_in(self):
        result = _make_series_condition(
            self.df,
            "location",
            ComparisonOperator.NOT_IN.value,
            ["Washington", "New York"],
        )
        expected = pd.Series([False, True, False, False], name="location")
        pd.testing.assert_series_equal(result, expected)

    def test_evaluate_criteria_or(self):
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
        result = _evaluate_criteria(self.df, rule)
        expected = pd.Series([True, True, True, True])
        pd.testing.assert_series_equal(result, expected)

    def test_evaluate_criteria_and(self):
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
        result = _evaluate_criteria(self.df, rule)
        expected = pd.Series([True, False, False, True])
        pd.testing.assert_series_equal(result, expected)

    def test_evaluate_criteria_invalid(self):
        rule = {
            ComparisonKey.KEY.value: "description",
            ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
        }
        with self.assertRaises(ValueError):
            _evaluate_criteria(self.df, rule) # type: ignore

    def test_create_vector_function_invalid_rule(self):
        rule = {"invalid_key": []}
        with self.assertRaises(ValueError):
            create_vector_function(rule)


if __name__ == "__main__":
    unittest.main()
