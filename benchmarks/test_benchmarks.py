"""pytest-benchmark suite comparing scalar vs vector tag evaluation.

Run via `just bench`; excluded from the default test run (outside testpaths).
"""

import pytest
from pytest_benchmark.fixture import BenchmarkFixture

from benchmarks.dummy import generate_random_data, generate_random_tags
from taghound.taghound import TagHound

TAGS_COUNT = 500
ROW_COUNTS = [10, 100, 1000]


@pytest.fixture(scope="module")
def hound() -> TagHound:
    """TagHound loaded with generated random rules."""
    return TagHound(generate_random_tags(samples=TAGS_COUNT))


@pytest.fixture(scope="module")
def rows() -> list:
    """Generated random data rows, sliced per benchmark size."""
    return generate_random_data(samples=max(ROW_COUNTS))


@pytest.mark.parametrize("size", ROW_COUNTS)
def test_scalar(
    benchmark: BenchmarkFixture, hound: TagHound, rows: list, size: int
) -> None:
    """Benchmark the scalar path, grouped by row count against the vector path."""
    benchmark.group = f"rows-{size}"
    selected = rows[:size]

    def run() -> None:
        for row in selected:
            hound.find_all_tags(row, multi_threaded=False)

    benchmark(run)


@pytest.mark.parametrize("size", ROW_COUNTS)
def test_vector(
    benchmark: BenchmarkFixture, hound: TagHound, rows: list, size: int
) -> None:
    """Benchmark the vector path, grouped by row count against the scalar path."""
    benchmark.group = f"rows-{size}"
    selected = rows[:size]

    benchmark(hound.find_tags_using_vector, selected)
