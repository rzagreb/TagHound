"""The RE2 compatibility check that guards the vectorized path."""

import pytest

from taghound.exceptions import RE2UnsupportedError
from taghound.re2 import check_re2_safe


@pytest.mark.parametrize(
    ("pattern", "construct"),
    [
        (r"(?=python)dev", "(?="),
        (r"(?!micro)python", "(?!"),
        (r"(?<=micro)python", "(?<="),
        (r"(?<!micro)python", "(?<!"),
        (r"(?>python)", "(?>"),
        (r"(python) \1", r"\1"),
        (r"(?P<lang>py)(?P=lang)", "(?P="),
        (r"go|(?!micro)python", "(?!"),
        (r"[a-z]+(?<!ed)\b", "(?<!"),
    ],
)
def test_constructs_re2_rejects_are_named_in_the_error(pattern, construct):
    """The message has to say which construct, or the author can't fix it."""
    with pytest.raises(RE2UnsupportedError) as error:
        check_re2_safe(pattern)
    assert construct in str(error.value)


@pytest.mark.parametrize(
    "pattern",
    [
        r"python",
        r"C\+\+",
        r"C\#",
        r"\#LI-Remote",
        r"\.?net",
        r"data\w*",
        r"(?i)python",
        r"(?:^|\W)(?:python|go)(?:$|\W)",
        r"(?P<lang>python)",
        r"[\d]{1,3}",
        r"a{2,3}",
        # A literal backslash then a digit is not a backreference.
        r"\\1",
        # Escaped punctuation must not read as a quantifier or a group opener.
        r"\(\?\!not-a-group\)",
        # `(?<` only means look-behind outside a character class.
        r"[(?<!]+",
    ],
)
def test_patterns_re2_accepts_pass(pattern):
    check_re2_safe(pattern)
