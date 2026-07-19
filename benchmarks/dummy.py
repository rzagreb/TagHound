"""Random rule and data generators for the profiling scripts."""

# ruff: noqa: T201, ANN201, ANN202, ANN401, D103 -- dev profiling helper; prints are its output

import random
from typing import Any
from collections.abc import Mapping

from taghound.constants import (
    ComparisonKey,
    ComparisonOperator,
    LogicalOperator,
    RuleKey,
)
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
