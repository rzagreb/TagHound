"""Exceptions raised while parsing rule definitions."""


class InvalidOperatorError(Exception):
    """A logical node uses an operator other than `and`/`or`."""


class RuleAndOrTogetherError(Exception):
    """A rule's root condition has both `and` and `or`."""


class MissingRootConditionError(Exception):
    """A rule's root condition has neither `and` nor `or`."""
