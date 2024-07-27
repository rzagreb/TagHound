from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Mapping, Optional

from taghound.constants import PdEvalFn, PyEvalFn


@dataclass(frozen=True, eq=True)
class TagRule:
    id: str
    """ Unique identifier for the rule. """

    label: str = field(compare=False)
    """ The label for the rule. No uniqueness is enforced. """

    weight: float = field(compare=False)
    """ The weight of the rule. """

    required_fields: set[str] = field(compare=False)
    """ The required fields for the rule. """

    scalar_check: Optional[PyEvalFn] = field(compare=False, default=None)
    """ Scalar function used to check if the rule is satisfied. """

    vector_check: Optional[PdEvalFn] = field(compare=False, default=None)
    """ Vector function used to check if the rule is satisfied. """

    data: Mapping[str, Any] = field(compare=False, default_factory=dict)
    """ The original data used to create the TagRule object. """


@dataclass(frozen=True, eq=True)
class TagFound:
    id: str
    rule: TagRule
