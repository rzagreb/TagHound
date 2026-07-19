"""Data model for tag rules."""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass, field
from typing import Any

from taghound.constants import PdEvalFn, PyEvalFn


@dataclass(frozen=True, eq=True, slots=True)
class TagRule:
    """A single tag rule: identity, metadata, and its prebuilt check functions."""

    id: str
    """ Unique identifier for the rule. """

    label: str = field(compare=False)
    """ The label for the rule. No uniqueness is enforced. """

    weight: float = field(compare=False)
    """ The weight of the rule. """

    required_fields: set[str] = field(compare=False)
    """ The required fields for the rule. """

    info: str | None = field(compare=False, default=None)
    """ Internal comments for the rule. """

    scalar_check: PyEvalFn | None = field(compare=False, default=None)
    """ Scalar function used to check if the rule is satisfied. """

    vector_check: PdEvalFn | None = field(compare=False, default=None)
    """ Vector function used to check if the rule is satisfied. """

    data: Mapping[str, Any] = field(compare=False, default_factory=dict)
    """ The original data used to create the TagRule object. """
