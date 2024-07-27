"""
To run memory profiler:
    1. add @profile decorator to the function you want to profile
    2. Run `mprof run perf_test_v1.py`


NOTE: run_line_profiler will not work with memory_profiler
NOTE: run_line_profiler will not work with multithreading
"""
import random

import context  # type: ignore # noqa: F401
from line_profiler import LineProfiler

from profiling.dummy import (
    DATA_COUNT,
    TAGS_COUNT,
    generate_random_data,
    generate_random_tags,
)
from taghound.taghound import TagHound
from taghound.scalar.function_builder import ComparisonOperatorMap


# from memory_profiler import profile
# @profile
def case_for_profile():
    random_tags = generate_random_tags(samples=TAGS_COUNT)
    th = TagHound(random_tags)

    datas = generate_random_data(samples=DATA_COUNT)

    print("Running scalar methods")
    for data in datas:
        th.find_all_tags(data, multi_threaded=False)


def run_line_profiler():
    profiler = LineProfiler()
    # profiler.add_function(case_for_profile)
    profiler.add_function(TagHound.find_all_tags)

    for func in ComparisonOperatorMap.values():
        profiler.add_function(func)

    profiler.run("case_for_profile()")
    profiler.print_stats()


if __name__ == "__main__":
    run_line_profiler()
    # case_for_profile()
