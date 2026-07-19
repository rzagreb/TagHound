# List available recipes
default:
    @just --list

# Static checks: ruff lint (read-only, fails on findings)
lint:
    uv run ruff check .

# Run the test suite (extra args pass through to pytest)
test *args:
    uv run pytest {{args}}

# Everything CI runs — green here means green CI
check: lint test

# Benchmarks (pytest-benchmark): stats table + run saved to .benchmarks/ for comparison
bench *args:
    uv run --group profiling pytest benchmarks --benchmark-autosave {{args}}

# Compare saved benchmark runs (history in .benchmarks/)
bench-compare:
    uv run --group profiling pytest-benchmark compare

# Line-profile an evaluation path (target = scalar|vector)
profile target="scalar":
    uv run --group profiling python -m benchmarks.profile_{{target}}_methods

# Memory-profile a path with memray and open the allocation flamegraph (target = scalar|vector)
profile-mem target="scalar":
    #!/usr/bin/env bash
    set -euo pipefail
    mkdir -p .profiles
    uv run --group profiling memray run -f -o .profiles/{{target}}.bin -m benchmarks.workload {{target}}
    uv run --group profiling memray flamegraph -f -o .profiles/{{target}}.html .profiles/{{target}}.bin
    open .profiles/{{target}}.html

# Remove build and cache artifacts
clean:
    rm -fr build dist .ruff_cache .pytest_cache .benchmarks .profiles

# Release (bump = patch|minor|major): bump, commit, tag, push; then cut the GitHub Release to publish
[confirm("Bump the version, commit, tag, and push a release?")]
release bump="patch": check
    #!/usr/bin/env bash
    set -euo pipefail
    # Promote CHANGELOG.md [Unreleased] to the new version before running this.
    uv version --bump {{bump}}
    version=$(uv version --short)
    git commit -m "Release v$version" pyproject.toml uv.lock CHANGELOG.md
    git tag -a "v$version" -m "v$version"
    git push --follow-tags
