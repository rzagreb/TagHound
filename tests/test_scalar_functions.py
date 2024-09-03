from __future__ import annotations

import unittest

import context  # type: ignore # noqa: F401

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


class TestCreateTagAssignmentRule(unittest.TestCase):
    def test_create_tag_assignment_rule_valid_and(self):
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
        self.assertEqual(rule.id, "python")
        self.assertEqual(rule.weight, 10.0)
        self.assertIn("language", rule.required_fields)
        self.assertIn("year", rule.required_fields)

    def test_create_tag_assignment_rule_valid_or(self):
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
        self.assertEqual(rule.id, "python")
        self.assertEqual(rule.weight, 10.0)
        self.assertIn("language", rule.required_fields)
        self.assertIn("year", rule.required_fields)

    def test_create_tag_assignment_rule_invalid_tag(self):
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
        with self.assertRaises(ValueError):
            to_tag_rule(data)

    def test_create_tag_assignment_rule_invalid_weight(self):
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
        with self.assertRaises(ValueError):
            to_tag_rule(data)

    def test_create_tag_assignment_rule_and_or_together(self):
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
        with self.assertRaises(RuleAndOrTogetherError):
            to_tag_rule(data)

    def test_create_tag_assignment_rule_missing_root_condition(self):
        data = {
            RuleKey.ID.value: "python",
            RuleKey.WEIGHT.value: 10,
        }
        with self.assertRaises(MissingRootConditionError):
            to_tag_rule(data)


class TestMakeLogicOperationFunction(unittest.TestCase):
    def test_make_logic_operation_function_and(self):
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
        self.assertTrue(callable(condition_fn))

    def test_make_logic_operation_function_or(self):
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
        self.assertTrue(callable(condition_fn))

    def test_make_logic_operation_function_invalid_operator(self):
        params = {
            "invalid_op": [
                {
                    ComparisonKey.KEY.value: "language",
                    ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                    ComparisonKey.VALUE.value: "python",
                },
            ]
        }
        with self.assertRaises(InvalidOperatorError):
            _create_logic_function(params, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)  # type: ignore


class TestMakeConditionFunctions(unittest.TestCase):
    def test_make_condition_functions_valid(self):
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
        condition_fns = _parse_conditions(
            elems, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN
        )
        self.assertEqual(len(condition_fns), 2)
        for fn in condition_fns:
            self.assertTrue(callable(fn))

    def test_make_condition_functions_invalid_not_list(self):
        elems = {
            ComparisonKey.KEY.value: "language",
            ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
            ComparisonKey.VALUE.value: "python",
        }
        with self.assertRaises(TypeError):
            _parse_conditions(elems, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)  # type: ignore

    def test_make_condition_functions_empty_dict(self):
        elems = [{}]
        with self.assertRaises(ValueError):
            _parse_conditions(elems, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN)  # type: ignore


class TestMakeComparisonConditionFunction(unittest.TestCase):
    def test_make_comparison_condition_function_equal(self):
        fn = _make_comparison_condition_function(
            "language",
            "python",
            ComparisonOperator.EQUAL.value,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(fn))
        data = {"language": "python"}
        self.assertTrue(fn(data))
        data = {"language": "java"}
        self.assertFalse(fn(data))

    def test_make_comparison_condition_function_not_equal(self):
        fn = _make_comparison_condition_function(
            "language",
            "python",
            ComparisonOperator.NOT_EQUAL.value,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(fn))
        data = {"language": "python"}
        self.assertFalse(fn(data))
        data = {"language": "java"}
        self.assertTrue(fn(data))

    def test_make_comparison_condition_function_greater_than(self):
        fn = _make_comparison_condition_function(
            "year",
            1990,
            ComparisonOperator.GREATER_THAN.value,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(fn))
        data = {"year": 2000}
        self.assertTrue(fn(data))
        data = {"year": 1980}
        self.assertFalse(fn(data))

    def test_make_comparison_condition_function_greater_than_or_equal(self):
        fn = _make_comparison_condition_function(
            "year",
            1990,
            ComparisonOperator.GREATER_THAN_OR_EQUAL.value,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(fn))
        data = {"year": 2000}
        self.assertTrue(fn(data))
        data = {"year": 1990}
        self.assertTrue(fn(data))
        data = {"year": 1980}
        self.assertFalse(fn(data))

    def test_make_comparison_condition_function_less_than(self):
        fn = _make_comparison_condition_function(
            "year",
            2000,
            ComparisonOperator.LESS_THAN.value,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(fn))
        data = {"year": 1990}
        self.assertTrue(fn(data))
        data = {"year": 2000}
        self.assertFalse(fn(data))

    def test_make_comparison_condition_function_less_than_or_equal(self):
        fn = _make_comparison_condition_function(
            "year",
            2000,
            ComparisonOperator.LESS_THAN_OR_EQUAL.value,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(fn))
        data = {"year": 1990}
        self.assertTrue(fn(data))
        data = {"year": 2000}
        self.assertTrue(fn(data))
        data = {"year": 2010}
        self.assertFalse(fn(data))

    def test_make_comparison_condition_function_in(self):
        fn = _make_comparison_condition_function(
            "language",
            ["python", "java"],
            ComparisonOperator.IN.value,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(fn))
        data = {"language": "python"}
        self.assertTrue(fn(data))
        data = {"language": "c++"}
        self.assertFalse(fn(data))

    def test_make_comparison_condition_function_not_in(self):
        fn = _make_comparison_condition_function(
            "language",
            ["python", "java"],
            ComparisonOperator.NOT_IN.value,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(fn))
        data = {"language": "python"}
        self.assertFalse(fn(data))
        data = {"language": "c++"}
        self.assertTrue(fn(data))

    def test_make_comparison_condition_function_is(self):
        fn = _make_comparison_condition_function(
            "is_active",
            True,
            ComparisonOperator.IS.value,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(fn))
        data = {"is_active": True}
        self.assertTrue(fn(data))
        data = {"is_active": False}
        self.assertFalse(fn(data))

    def test_make_comparison_condition_function_is_not(self):
        fn = _make_comparison_condition_function(
            "is_active",
            True,
            ComparisonOperator.IS_NOT.value,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(fn))
        data = {"is_active": True}
        self.assertFalse(fn(data))
        data = {"is_active": False}
        self.assertTrue(fn(data))

    def test_make_comparison_condition_function_regex_match(self):
        fn = _make_comparison_condition_function(
            "text",
            "python",
            ComparisonOperator.REGEX_MATCH.value,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(fn))

        data = {"text": "I love python"}
        self.assertTrue(fn(data))

        data = {"text": "I love java"}
        self.assertFalse(fn(data))

    def test_make_comparison_condition_function_regex_not_match(self):
        fn = _make_comparison_condition_function(
            "text",
            "python",
            ComparisonOperator.REGEX_NOT_MATCH.value,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(fn))

        data = {"text": "I love python"}
        self.assertFalse(fn(data))

        data = {"text": "I love java"}
        self.assertTrue(fn(data))


class TestValidateAndNormalizeComparisonConditionData(unittest.TestCase):
    def test_validate_and_normalize_comparison_condition_data_regex(self):
        value, operator = _validate_and_normalize_comparison_condition_data(
            "python",
            ComparisonOperator.REGEX_MATCH,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(value.match))
        self.assertEqual(operator, ComparisonOperator.REGEX_MATCH)

    def test_validate_and_normalize_comparison_condition_data_regex_list(self):
        value, operator = _validate_and_normalize_comparison_condition_data(
            ["python", "java"],
            ComparisonOperator.REGEX_MATCH,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertTrue(callable(value.match))
        self.assertEqual(operator, ComparisonOperator.REGEX_MATCH)

    def test_validate_and_normalize_comparison_condition_data_numeric(self):
        value, operator = _validate_and_normalize_comparison_condition_data(
            100,
            ComparisonOperator.GREATER_THAN,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertEqual(value, 100)
        self.assertEqual(operator, ComparisonOperator.GREATER_THAN)

    def test_validate_and_normalize_comparison_condition_data_numeric_invalid(self):
        with self.assertRaises(ValueError):
            _validate_and_normalize_comparison_condition_data(
                "100",
                ComparisonOperator.GREATER_THAN,
                merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
            )

    def test_validate_and_normalize_comparison_condition_data_string(self):
        value, operator = _validate_and_normalize_comparison_condition_data(
            "python",
            ComparisonOperator.EQUAL,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertEqual(value, "python")
        self.assertEqual(operator, ComparisonOperator.EQUAL)

    def test_validate_and_normalize_comparison_condition_data_list(self):
        value, operator = _validate_and_normalize_comparison_condition_data(
            ["python", "java"],
            ComparisonOperator.IN,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertEqual(value, set(["python", "java"]))
        self.assertEqual(operator, ComparisonOperator.IN)

    def test_validate_and_normalize_comparison_condition_data_list_invalid(self):
        with self.assertRaises(ValueError):
            _validate_and_normalize_comparison_condition_data(
                "python",
                ComparisonOperator.IN,
                merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
            )

    def test_validate_and_normalize_comparison_condition_data_boolean_equal(self):
        value, operator = _validate_and_normalize_comparison_condition_data(
            True, ComparisonOperator.EQUAL, merge_pattern=DEFAULT_REGEX_MERGE_PATTERN
        )
        self.assertEqual(value, True)
        self.assertEqual(operator, ComparisonOperator.IS)

    def test_validate_and_normalize_comparison_condition_data_boolean_not_equal(self):
        value, operator = _validate_and_normalize_comparison_condition_data(
            True,
            ComparisonOperator.NOT_EQUAL,
            merge_pattern=DEFAULT_REGEX_MERGE_PATTERN,
        )
        self.assertEqual(value, True)
        self.assertEqual(operator, ComparisonOperator.IS_NOT)


class TestGetRequiredKeysFromConditions(unittest.TestCase):
    def test_get_required_keys_from_conditions_single(self):
        conditions = [
            {
                ComparisonKey.KEY.value: "language",
                ComparisonKey.OPERATOR.value: ComparisonOperator.EQUAL.value,
                ComparisonKey.VALUE.value: "python",
            }
        ]
        keys = get_required_keys_from_conditions(conditions)  # type: ignore
        self.assertIn("language", keys)

    def test_get_required_keys_from_conditions_nested(self):
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
        self.assertIn("language", keys)
        self.assertIn("year", keys)
        self.assertIn("experience", keys)


if __name__ == "__main__":
    unittest.main()
