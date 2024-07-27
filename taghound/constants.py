from __future__ import annotations

from enum import Enum
from typing import Any, Callable, Mapping

import pandas as pd

PyEvalFn = Callable[[Mapping[str, Any]], bool]
PdEvalFn = Callable[[pd.DataFrame], pd.Series]


class LogicalOperator(Enum):
    AND = "and"
    OR = "or"


class ComparisonOperator(Enum):
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
    ID = "id"
    LABEL = "label"
    WEIGHT = "weight"
    ROOT_AND = LogicalOperator.AND.value
    ROOT_OR = LogicalOperator.OR.value


class ComparisonKey(Enum):
    KEY = "key"
    OPERATOR = "op"
    VALUE = "value"


DEFAULT_COMPARISON_OPERATOR = ComparisonOperator.EQUAL
DEFAULT_WEIGHT = 0.0
