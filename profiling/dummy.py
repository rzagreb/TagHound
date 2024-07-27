"""
To run memory profiler:
    1. add @profile decorator to the function you want to profile
    2. Run `mprof run perf_test_v1.py`


NOTE: run_line_profiler will not work with memory_profiler
NOTE: run_line_profiler will not work with multithreading
"""

import random
from typing import Any, Mapping

import context  # type: ignore # noqa: F401

from taghound.constants import ComparisonKey, ComparisonOperator, LogicalOperator, RuleKey
from taghound.models import TagRule
from taghound.serializers import to_tag_rule

TAGS_COUNT = 1_000
DATA_COUNT = 1_000


def generate_random_tags(samples: int) -> list[TagRule]:
    print("Generating random tags")

    def random_logical_operator() -> LogicalOperator:
        return random.choice(list(LogicalOperator))

    def random_comparison_operator(data_key: str) -> ComparisonOperator:
        if data_key == "language":
            return random.choice(
                [
                    ComparisonOperator.EQUAL,
                    ComparisonOperator.NOT_EQUAL,
                    ComparisonOperator.REGEX_MATCH,
                    ComparisonOperator.REGEX_NOT_MATCH,
                ]
            )
        elif data_key == "year":
            return random.choice(
                [
                    ComparisonOperator.EQUAL,
                    ComparisonOperator.NOT_EQUAL,
                    ComparisonOperator.IN,
                    ComparisonOperator.NOT_IN,
                ]
            )
        elif data_key == "version":
            return random.choice(
                [
                    ComparisonOperator.EQUAL,
                    ComparisonOperator.NOT_EQUAL,
                    ComparisonOperator.REGEX_MATCH,
                    ComparisonOperator.REGEX_NOT_MATCH,
                ]
            )
        return ComparisonOperator.EQUAL

    def random_value(data_key: str, operator: ComparisonOperator) -> Any:
        if data_key == "language":
            return ["python", "java", r"c\+\+", "javascript"]
        elif data_key == "year":
            if operator in {ComparisonOperator.IN, ComparisonOperator.NOT_IN}:
                return [random.randint(1990, 2024) for _ in range(random.randint(1, 5))]
            return random.randint(1990, 2024)
        elif data_key == "version":
            return ["3.10", "2.7", "1.8", "11"]
        return "unknown"

    def random_condition() -> dict:
        data_key = random.choice(["language", "year", "version"])
        operator = random_comparison_operator(data_key)
        return {
            ComparisonKey.KEY.value: data_key,
            ComparisonKey.OPERATOR.value: operator.value,
            ComparisonKey.VALUE.value: random_value(data_key, operator),
        }

    tags = []
    for i in range(samples):
        random.seed(i)

        tag_id = f"tag_{i}"
        label = f"label_{i}"
        weight = random.uniform(0.1, 10.0)
        conditions = [random_condition() for _ in range(random.randint(1, 3))]

        data = {
            RuleKey.ID.value: tag_id,
            RuleKey.WEIGHT.value: weight,
            RuleKey.LABEL.value: label,
        }

        if random_logical_operator() == LogicalOperator.AND:
            data[RuleKey.ROOT_AND.value] = conditions
        else:
            data[RuleKey.ROOT_OR.value] = conditions

        tag_rule = to_tag_rule(data)
        tags.append(tag_rule)

    return tags


def generate_random_data(samples: int) -> list[Mapping[str, str]]:
    print("Generating random data")
    data = []
    for i in range(samples):
        random.seed(i)

        data.append(
            {
                "language": random.choice(["python", "java", "c++"]),
                "year": str(random.randint(1980, 2021)),
                "version": f"{random.randint(1, 3)}.{random.randint(0, 10)}",
            }
        )

    return data
