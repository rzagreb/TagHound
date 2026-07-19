"""Line-profile the vector evaluation path over generated rules and data."""

# ruff: noqa: T201, ANN201, D103 -- dev profiling script; prints are its output

from line_profiler import LineProfiler

from benchmarks.workload import run_vector
from taghound.taghound import TagHound
from taghound.vector.function_builder import (
    _evaluate_criteria,
    _make_series_condition,
    create_vector_function,
)


def run_line_profiler():
    profiler = LineProfiler()
    profiler.add_function(TagHound.find_tags_using_vector)
    profiler.add_function(create_vector_function)
    profiler.add_function(_evaluate_criteria)
    profiler.add_function(_make_series_condition)

    profiler.runcall(run_vector)
    profiler.print_stats()


if __name__ == "__main__":
    run_line_profiler()
