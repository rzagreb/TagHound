"""Exceptions raised while parsing rule definitions."""


class TagHoundError(Exception):
    """Base for every error TagHound raises, so one `except` covers the library."""


class InvalidRuleError(TagHoundError, ValueError):
    """A rule is malformed or a value cannot work with its operator."""

    # Also a ValueError: that is what the rule builders raised before there was a
    # base class, and callers written against it keep working.


class InvalidRuleTypeError(TagHoundError, TypeError):
    """Part of a rule is the wrong Python type."""

    # A TypeError for the same reason InvalidRuleError is a ValueError.


class RE2UnsupportedError(InvalidRuleError):
    """A pattern uses regex syntax the vectorized path's engine rejects."""


class DuplicateRuleIdError(InvalidRuleError):
    """Two rules share an id, which is what a rule is keyed on."""


class InvalidOperatorError(TagHoundError):
    """A logical node uses an operator other than `and`/`or`."""


class RuleAndOrTogetherError(TagHoundError):
    """A rule's root condition has both `and` and `or`."""


class MissingRootConditionError(TagHoundError):
    """A rule's root condition has neither `and` nor `or`."""
