# Changelog

All notable changes to this project are documented here.

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
