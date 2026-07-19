"""Line-profile the scalar evaluation path over generated rules and data.

NOTE: run_line_profiler will not work with multithreading
"""

# ruff: noqa: T201, ANN201, D103 -- dev profiling script; prints are its output

from line_profiler import LineProfiler

from benchmarks.workload import run_scalar
from taghound.scalar.function_builder import ComparisonOperatorMap
from taghound.taghound import TagHound


def run_line_profiler():
    profiler = LineProfiler()
    profiler.add_function(TagHound.find_all_tags)

    for func in ComparisonOperatorMap.values():
        profiler.add_function(func)

    profiler.runcall(run_scalar)
    profiler.print_stats()


if __name__ == "__main__":
    run_line_profiler()
