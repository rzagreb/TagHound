"""Line-profile the vector evaluation path over generated rules and data."""

# ruff: noqa: T201, ANN201, D103 -- dev profiling script; prints are its output

from line_profiler import LineProfiler

from benchmarks.workload import run_vector
from taghound.taghound import TagHound
from taghound.vector.function_builder import (
    _as_bool_mask,
    _build_criteria_function,
    _build_series_condition,
    _get_column,
    create_vector_function,
)


def run_line_profiler():
    profiler = LineProfiler()
    profiler.add_function(TagHound.find_tags_using_vector)
    # The builders run once per rule at load; the two below run per evaluation and
    # are where the time actually goes.
    profiler.add_function(create_vector_function)
    profiler.add_function(_build_criteria_function)
    profiler.add_function(_build_series_condition)
    profiler.add_function(_as_bool_mask)
    profiler.add_function(_get_column)

    profiler.runcall(run_vector)
    profiler.print_stats()


if __name__ == "__main__":
    run_line_profiler()
