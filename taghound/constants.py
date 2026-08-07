"""Shared enums, type aliases, and defaults for rule definitions."""

from __future__ import annotations

from collections.abc import Callable, Mapping
from enum import Enum
from typing import Any

import pandas as pd

PyEvalFn = Callable[[Mapping[str, Any]], bool]
PdEvalFn = Callable[[pd.DataFrame], pd.Series]


class LogicalOperator(Enum):
    """Logical operators combining condition lists."""

    AND = "and"
    OR = "or"


class ComparisonOperator(Enum):
    """Comparison operators usable in a condition's `op` field."""

    EQUAL = "="
    NOT_EQUAL = "!="
    GREATER_THAN = ">"
    GREATER_THAN_OR_EQUAL = ">="
    LESS_THAN = "<"
    LESS_THAN_OR_EQUAL = "<="
    IS = "is"
    IS_NOT = "is_not"
    IN = "in"
    NOT_IN = "not_in"
    REGEX_MATCH = "~"
    REGEX_NOT_MATCH = "!~"


ComparisonOperatorRegexOnly = {
    ComparisonOperator.REGEX_MATCH,
    ComparisonOperator.REGEX_NOT_MATCH,
}
ComparisonOperatorNumericOnly = {
    ComparisonOperator.GREATER_THAN,
    ComparisonOperator.GREATER_THAN_OR_EQUAL,
    ComparisonOperator.LESS_THAN,
    ComparisonOperator.LESS_THAN_OR_EQUAL,
}
ComparisonOperatorBoolOnly = {
    ComparisonOperator.IS,
    ComparisonOperator.IS_NOT,
}
ComparisonOperatorListOnly = {
    ComparisonOperator.IN,
    ComparisonOperator.NOT_IN,
}


class RuleKey(Enum):
    """Top-level keys of a rule definition."""

    ID = "id"
    LABEL = "label"
    WEIGHT = "weight"
    INFO = "info"
    ROOT_AND = LogicalOperator.AND.value
    ROOT_OR = LogicalOperator.OR.value


class ComparisonKey(Enum):
    """Keys of a single comparison condition."""

    KEY = "key"
    OPERATOR = "op"
    VALUE = "value"


DEFAULT_COMPARISON_OPERATOR = ComparisonOperator.EQUAL
""" Default comparison operator to use when not specified """

DEFAULT_WEIGHT = 0.0
""" Default weight to use when not specified """

# - This is better than \b...\b because it also matches cases like `C++`
# - Limitations: does not work when we need to match `.python` with `python` pattern
DEFAULT_REGEX_MERGE_PATTERN = r"(?<!\w)(?:{pattern})(?!\w)"
""" Pattern used to merge list of patterns into a single regex pattern """

# Same boundaries as DEFAULT_REGEX_MERGE_PATTERN, spelled without look-around so
# RE2 accepts it -- that's the engine pyarrow-backed columns run on. "Not preceded
# by a word char" and "at the start or on a \W" are the same condition, so for the
# boolean `contains` the vector path needs, the two are equivalent. Do NOT fall back
# to \b...\b here: it silently drops every pattern that starts or ends with
# punctuation, so `C++`, `C#` and `#LI-Remote` never match.
RE2_SAFE_REGEX_MERGE_PATTERN = r"(?:^|\W)(?:{pattern})(?:$|\W)"
""" Merge pattern for the vectorized path, where look-around is unavailable """
