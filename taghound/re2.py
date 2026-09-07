"""Check a pattern against the regex syntax RE2 supports.

The vectorized path runs on RE2 whenever the column is pyarrow-backed, and RE2
refuses constructs Python's `re` accepts. Nothing else catches that: `re.compile`
takes a look-behind happily, the rule then returns correct tags on an `object`
column and raises inside pyarrow on a `string[pyarrow]` one, so the same rule set
means two different things depending on a dtype the rule author never sees.
"""

from __future__ import annotations

from taghound.exceptions import RE2UnsupportedError

UNSUPPORTED_GROUPS = {
    "(?=": "look-ahead `(?=`",
    "(?!": "negative look-ahead `(?!`",
    "(?<=": "look-behind `(?<=`",
    "(?<!": "negative look-behind `(?<!`",
    "(?>": "atomic group `(?>`",
    "(?P=": "named backreference `(?P=`",
}
"""Group openers RE2 rejects, mapped to how the error names them."""


def check_re2_safe(pattern: str) -> None:
    """Raise unless `pattern` is syntax RE2 accepts.

    Escapes and character classes are walked rather than searched for, so
    `C\\+\\+` and `[(?<!]` read as the literals they are instead of tripping the
    check. Only what RE2 rejects outright is looked for; a pattern passing here
    still has to compile.

    Raises:
        RE2UnsupportedError: Naming the construct, which is the only part of this
            the rule's author can act on.

    Example:
        >>> check_re2_safe(r"C\\+\\+")
        >>> check_re2_safe(r"(?<!micro)python")
        Traceback (most recent call last):
        ...
        taghound.exceptions.RE2UnsupportedError: `(?<!micro)python` uses negative look-behind `(?<!`, which RE2 does not support. The vectorized path runs on RE2 for pyarrow-backed string columns.
    """
    position = 0
    in_class = False
    while position < len(pattern):
        char = pattern[position]

        if char == "\\":
            following = pattern[position + 1 : position + 2]
            # `\1` outside a class is a backreference; inside one it is an octal
            # escape, and `\\1` is a literal backslash followed by a digit.
            if not in_class and following.isdigit() and following != "0":
                _refuse(pattern, f"backreference `\\{following}`")
            position += 2
            continue

        if in_class:
            in_class = char != "]"
            position += 1
            continue

        if char == "[":
            in_class = True
        elif char == "(":
            for prefix, construct in UNSUPPORTED_GROUPS.items():
                if pattern.startswith(prefix, position):
                    _refuse(pattern, construct)
        position += 1


def _refuse(pattern: str, construct: str) -> None:
    """Raise, naming both the pattern and what is wrong with it."""
    raise RE2UnsupportedError(
        f"`{pattern}` uses {construct}, which RE2 does not support. The vectorized "
        "path runs on RE2 for pyarrow-backed string columns."
    )
