# Changelog

All notable changes to this project are documented here.

## [0.6.0] 2026-09-06

### Removed

- **Breaking:** `find_all_tags` no longer takes `multi_threaded` or `thread_exec_params`. Rule checks are CPU-bound Python, so the threads cost more than they saved; drop the argument.
- `ComparisonOperatorBoolOnly` from `taghound.constants`, which nothing read.

### Fixed

- `find_all_tags` now names the problem when rules were loaded with `add_scalar_func=False`, instead of failing with `'NoneType' object is not callable`.
- Vector conditions on an empty DataFrame return a bool Series rather than an object one, matching every other evaluation path.

## [0.5.0] 2026-09-06

### Added

- `taghound.serializers.dump_rules` and `dump_conditions` — write rules back out as the YAML the loaders read. The library could read a rules file but not produce one, so anything storing rules outside a file (a database table, an editor with a download button) had to hand-roll the four things that make regex-bearing YAML safe: quoting every pattern, never folding a long line, indenting sequences under their key, and a fixed `key`/`op`/`value` order.
- `TagHound.rules_from_string` — load rules from YAML text. `rules_from_yaml` takes a path, which an upload does not have.
- `taghound.re2.check_re2_safe` — checks a pattern against the syntax RE2 accepts.
- `TagHoundError`, a base for everything the library raises, so one `except` covers it. `InvalidRuleError` and `InvalidRuleTypeError` also inherit `ValueError` and `TypeError` respectively, so code catching those is unaffected. New `RE2UnsupportedError` and `DuplicateRuleIdError` sit under it.
- README section on text dtypes, RE2 and missing values.

### Fixed

- **Nullable dtypes no longer break the vectorized path.** 0.3.0 resolved nulls to "no match" on the regex branch only; the numeric, membership and equality branches still leaked `<NA>` into the and/or masks. An `Int64` column with a gap raised `ValueError: cannot convert to 'bool'-dtype NumPy array with missing values`, and — worse, because it is the dtype this library's own design steers you toward — `string[pyarrow]` with `=` raised `TypeError: boolean value of NA is ambiguous`. Every branch now resolves a null to "did not match" before any negation, so `!=` and `not_in` treat a missing cell the way `!~` already did. Callers working around this by casting to `float64` can stop.
- Two rules sharing an `id` now raise `DuplicateRuleIdError` at load. They used to load quietly and then disagree about themselves: the weight and label maps kept the last one, while `output_format="columns"` emitted the id twice.

### Changed

- **Breaking:** a pattern using look-ahead, look-behind, a backreference or an atomic group is refused when the rule is built, rather than working on an `object` column and raising inside pyarrow on a `string[pyarrow]` one. Rules relying on this were already a latent break — the same rule set meant two different things depending on a dtype their author never sees. The error names the construct. Only the pattern the rule's author wrote is checked; a `vector_merge_pattern` you pass yourself is left alone, since that is a deliberate choice and may be look-around on purpose for object columns.

## [0.4.0] 2026-08-06

### Added

- `RE2_SAFE_REGEX_MERGE_PATTERN` — the same word boundaries as the default merge pattern, spelled without look-around so RE2 accepts it.
- `to_tag_rule`, `TagHound.rules_from_yaml` and `TagHound.rules_from_json` take a separate `vector_merge_pattern`, defaulting to the RE2-safe constant. Only the vectorised path is engine-constrained, so the scalar path keeps the look-around default.

### Fixed

- The vectorised path no longer forces callers to hand-roll an RE2-safe merge pattern. Callers that worked around 0.3.0 by passing `\b(?:{pattern})\b` were silently dropping every pattern that starts or ends with punctuation — `C++`, and `.NET` could never match. Drop the override to pick up the fix.

## [0.3.0] 2026-07-21

### Changed

- Vector regex conditions use an inline `(?i)` flag instead of `flags=re.IGNORECASE`, so `str.contains` can run on the vectorised RE2 engine for `string[pyarrow]` columns instead of always falling back to per-element Python `re`. **Rule regexes must be RE2-compatible (no look-around or backreferences).**

### Fixed

- Vector regex conditions no longer raise on missing values; a null cell is now treated as "no match" instead of erroring on `~` (regex-not-match) or leaking NA into `and`/`or` masks.

## [0.2.0] - 2026-07-19

### Added

- `py.typed` marker so type checkers pick up the package's type hints

### Changed

- Python 3.11+ is now required; dependency floors raised to `pyyaml>=6.0.2` and `pandas>=2.2.3,<3`
- Rules are validated when loaded, so invalid rules (bad regex, unknown operator) raise at load time instead of on first evaluation
- `find_tags_using_vector` now supports the full documented operator set (`>`, `<`, `>=`, `<=`, `is`, `is_not`) and defaults a missing `op` to `=`, matching `find_all_tags`
- `find_tags_using_vector` raises a clear error when no rules are set or rules were loaded without vector functions, matching `find_all_tags`

### Fixed

- Scalar rule functions re-parsed the whole rule tree (and recompiled every regex) on every evaluation; `find_all_tags` is now roughly 9x faster on large rule sets
- Rules failed to load when a condition listed `key` after `value` (e.g. `{"op": "in", "value": [...], "key": "location"}`), and mappings inside `value` lists polluted the required fields
- Vector evaluation errors no longer print the entire DataFrame to stdout

## [0.1.19] - 2024-09-03

### Added

- Option to use a custom regex merge pattern

## [0.1.18] - 2024-09-02

### Fixed

- Tag rule serializer uses the tag name as the default label instead of the constant "id"

## [0.1.17] - 2024-08-09

### Added

- Support for the `info` field in `TagRule`
- Initial changelog file

### Changed

- `find_tags_using_vector` supports a custom output column name
