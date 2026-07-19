"""Shared profiling workload: build random rules/data and run one evaluation path.

Used by the line-profiler scripts and as a profiler-free entry point for memray
(`just profile-mem`).
"""

# ruff: noqa: T201 -- dev profiling script; prints are its output

import sys

from benchmarks.dummy import (
    DATA_COUNT,
    TAGS_COUNT,
    generate_random_data,
    generate_random_tags,
)
from taghound.taghound import TagHound


def run_scalar() -> None:
    """Run the scalar evaluation path over generated rules and data."""
    th = TagHound(generate_random_tags(samples=TAGS_COUNT))
    datas = generate_random_data(samples=DATA_COUNT)

    print("Running scalar methods")
    for data in datas:
        th.find_all_tags(data, multi_threaded=False)


def run_vector() -> None:
    """Run the vector evaluation path over generated rules and data."""
    th = TagHound(generate_random_tags(samples=TAGS_COUNT))
    rows = generate_random_data(samples=DATA_COUNT)

    print("Running vector methods")
    th.find_tags_using_vector(rows)


if __name__ == "__main__":
    target = sys.argv[1] if len(sys.argv) > 1 else "scalar"
    {"scalar": run_scalar, "vector": run_vector}[target]()
