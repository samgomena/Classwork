
import argparse
import numpy as np
from time import perf_counter
from functools import wraps

from selection_sort import selection_sort
from merge_sort import merge_sort

parser = argparse.ArgumentParser(description='Benchmark sorting algorithms.')

parser.add_argument("-m", "--max-time", 
                    type=int,
                    default=5,
                    dest="max_time",
                    action="store",
                    help="Maximum number of seconds to let each sort algo run for")

args = parser.parse_args()

# Deprecate this?
def time_it(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = perf_counter()
        func_value = func(*args, **kwargs)
        delta = perf_counter() - start
        print(f"{func.__name__} took {delta}s to run")
        return func_value
    return wrapper


def gen_rand_arr(size=1_000, min=1, max=1000):
    return list(np.random.randint(min, max, size))

def test_em(func, arr):
    start = perf_counter()
    func(arr)
    return perf_counter() - start

def time_test_it(func, min_arr_size=1_000, increase_arr_size_by=5_000, max_time_allowed=120):
    # Update `arr_size` as our test progresses
    arr_size = min_arr_size
    
    rand_arr = gen_rand_arr(arr_size)
    time = test_em(func, rand_arr)
    print(f"{func.__name__}([{arr_size:,}]) took {time}s")

    while time < max_time_allowed:
        arr_size += increase_arr_size_by
        rand_arr = gen_rand_arr(arr_size)
        time = test_em(func, rand_arr)
        print(f"{func.__name__}([{arr_size:,}]) took {time}s")

def main():
    max_time = 6
    time_test_it(merge_sort, max_time_allowed=args.max_time)
    time_test_it(selection_sort, max_time_allowed=args.max_time)

if __name__ == "__main__":
    main()