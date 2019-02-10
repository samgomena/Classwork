
import argparse
import numpy as np
from time import perf_counter
from functools import wraps

from selection_sort import selection_sort
from merge_sort import merge_sort
from radix_sort import radix_sort

parser = argparse.ArgumentParser(description='Benchmark sorting algorithms.')

parser.add_argument("--min-arr-size", 
                    type=int,
                    default=0,
                    dest="min_arr_size",
                    action="store",
                    help="Minimum size of an array that will be sorted")

parser.add_argument("--max-arr-size", 
                    type=int,
                    default=50_000,
                    dest="max_arr_size",
                    action="store",
                    help="Maximum size of an array that will be sorted")


parser.add_argument("-t", "--total-time", 
                    type=int,
                    default=30,
                    dest="total_time",
                    action="store",
                    help="Maximum number of seconds to let each sort algo run for")

parser.add_argument("-m", "--max-time", 
                    type=int,
                    default=5,
                    dest="max_time",
                    action="store",
                    help="Maximum number of seconds to let each individual run on the sort algo run for")

parser.add_argument("-i", "--increase-size-by", 
                    type=int,
                    default=10_000,
                    dest="increase_by",
                    action="store",
                    help="Amount to increase the array by on each subsequent run")

parser.add_argument("-v", "--verbose", 
                    type=bool,
                    default=False,
                    dest="verbose",
                    action="store",
                    help="Print extended output to stdout")

class Benchmark:
    def __init__(self, func, args):
        self.func = func
        self.args = args

    
    @staticmethod
    def gen_rand_arr(size=1_000, min=1, max=1000):
        return list(np.random.randint(min, max, size))

    @staticmethod
    def test_em(func, arr):
        start = perf_counter()
        func(arr)
        return perf_counter() - start

    def time_test_it(self, max_time_allowed_per_run=None):
        # Update `arr_size` as our test progresses
        arr_size = self.args.min_arr_size
        total_run_time = 0.0
        
        rand_arr = self.gen_rand_arr(arr_size)
        time = self.test_em(self.func, rand_arr)
        total_run_time += time
        
        if self.args.verbose:
            print(f"{self.func.__name__}([{arr_size:,}]) took {time}s")

        while total_run_time < self.args.max_time:
            arr_size += self.args.increase_by
            rand_arr = self.gen_rand_arr(arr_size)
            
            time = self.test_em(self.func, rand_arr)
            total_run_time += time
            
            # Not sure about this yet
            if time > self.args.max_time:
                break
            
            if self.args.verbose:
                print(f"{self.func.__name__}([{arr_size:,}]) took {time}s")

        return self.func.__name__, arr_size, time

def main():
    max_iter_dict = {}

    args = parser.parse_args()

    for sort_func in [merge_sort, selection_sort, radix_sort, sorted]:
        # Create a new benchmark for each `sort_func`
        benchmark = Benchmark(sort_func, args)
        name, size, time = benchmark.time_test_it()
        max_iter_dict[name] = [size, time]


    for k, v in max_iter_dict.items():
        print(f"{k}:\n\tmax time spent: {v[1]:0.5f}\n\tmax array size: {v[0]:,}")

if __name__ == "__main__":
    main()