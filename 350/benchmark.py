
import argparse
import numpy as np
from time import perf_counter
from functools import wraps

from selection_sort import selection_sort
from merge_sort import merge_sort
from radix_sort import radix_sort

parser = argparse.ArgumentParser(description='Benchmark sorting algorithms.')

parser.add_argument("-m", "--max-time", 
                    type=int,
                    default=5,
                    dest="max_time",
                    action="store",
                    help="Maximum number of seconds to let each sort algo run for")

parser.add_argument("-i", "--increase-size-by", 
                    type=int,
                    default=10_000,
                    dest="increase_by",
                    action="store",
                    help="Amount to increase the array by on each subsequent run")

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

    return func.__name__, arr_size, time

def main():
    max_iter_dict = {}

    for sort_func in [merge_sort, selection_sort, radix_sort]:
        name, size, time = time_test_it(sort_func, increase_arr_size_by=args.increase_by, max_time_allowed=args.max_time)
        max_iter_dict[name] = [size, time]


    for k, v in max_iter_dict.items():
        print(f"{k}:\n\tmax time spent: {v[1]:0.5f}\n\tmax array size: {v[0]:,}")

if __name__ == "__main__":
    main()