import context  # type: ignore # noqa: F401
from line_profiler import LineProfiler

from profiling.dummy import (
    DATA_COUNT,
    TAGS_COUNT,
    generate_random_data,
    generate_random_tags,
)
from taghound.taghound import TagHound
from taghound.vector.function_builder import (
    _evaluate_criteria,
    _make_series_condition,
    create_vector_function,
)


# from memory_profiler import profile
# @profile
def case_for_profile():
    random_tags = generate_random_tags(samples=TAGS_COUNT)
    th = TagHound(random_tags)

    rows = generate_random_data(samples=DATA_COUNT)

    print("Running vector methods")
    th.find_tags_using_vector(rows)


def run_line_profiler():
    profiler = LineProfiler()
    # profiler.add_function(case_for_profile)
    profiler.add_function(TagHound.find_tags_using_vector)
    profiler.add_function(create_vector_function)
    profiler.add_function(_evaluate_criteria)
    profiler.add_function(_make_series_condition)

    profiler.run("case_for_profile()")
    profiler.print_stats()


if __name__ == "__main__":
    run_line_profiler()
    # case_for_profile()
