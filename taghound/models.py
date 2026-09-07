"""Data model for tag rules."""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass, field
from typing import Any

from taghound.constants import PdEvalFn, PyEvalFn


@dataclass(frozen=True, eq=True, slots=True)
class TagRule:
    """A single tag rule: identity, metadata, and its prebuilt check functions.

    Equality and hashing are on `id` alone; every other field is `compare=False`,
    so two rules sharing an id are the same rule as far as a set or dict is
    concerned.
    """

    id: str

    label: str = field(compare=False)
    """ Human-readable name. No uniqueness is enforced. """

    weight: float = field(compare=False)

    required_fields: set[str] = field(compare=False)
    """ Every data key the rule's conditions name. """

    info: str | None = field(compare=False, default=None)
    """ Free-text note for whoever maintains the rule; never matched against. """

    scalar_check: PyEvalFn | None = field(compare=False, default=None)
    """ Checks one mapping. None when built with `add_scalar_func=False`. """

    vector_check: PdEvalFn | None = field(compare=False, default=None)
    """ Checks a whole DataFrame. None when built with `add_vector_fn=False`. """

    data: Mapping[str, Any] = field(compare=False, default_factory=dict)
    """ The rule dict this was built from, and what `dump_rules` writes back out. """
