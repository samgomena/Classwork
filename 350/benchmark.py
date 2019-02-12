
import argparse
import chartify
import pandas as pd
from numpy.random import randint
from numpy import float64
from time import time, perf_counter
from datetime import datetime

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
                    default=False,
                    dest="verbose",
                    action="store_true",
                    help="Print extended output to stdout")


class Benchmark:
    def __init__(self, func, args):
        self.pseudo_df = []
        self.func = func
        self.args = args

    @staticmethod
    def gen_rand_arr(size=1_000, _min=1, _max=1000):
        return list(randint(_min, _max, size))

    @staticmethod
    def test_em(func, arr):
        start = perf_counter()
        func(arr)
        return perf_counter() - start

    def time_test_it(self):
        # Update `arr_size` as our test progresses
        arr_size = self.args.min_arr_size
        total_run_time = 0.0
        
        rand_arr = self.gen_rand_arr(arr_size)
        runtime = self.test_em(self.func, rand_arr)
        total_run_time += runtime
        
        if self.args.verbose:
            print(f"{self.func.__name__}([{arr_size:,}]) took {runtime}s")

        # Stop benchmarking once we've hit our runtime limit
        while total_run_time < self.args.max_time:
            arr_size += self.args.increase_by
            rand_arr = self.gen_rand_arr(arr_size)
            
            runtime = self.test_em(self.func, rand_arr)
            total_run_time += runtime
            
            if self.args.verbose:
                print(f"{self.func.__name__}([{arr_size:,}]) took {runtime}s")

            self.pseudo_df.append([self.func.__name__, arr_size, runtime, datetime.fromtimestamp(time()).strftime('%c')])

            # Stop benchmarking if we've hit our runtime limit
            if runtime > self.args.max_time:
                break
    
    def get_data(self):
        return pd.DataFrame(self.pseudo_df, columns=["name", "arr_size", "runtime", "datetime"])


def main():

    args = parser.parse_args()

    df = pd.DataFrame(columns=["name", "arr_size", "runtime", "datetime"])

    # Test our three sorting functions and the `sorted` builtin as a baseline
    for sort_func in [merge_sort, selection_sort, radix_sort, sorted]:
        # Create a new benchmark for each `sort_func`
        benchmark = Benchmark(sort_func, args)
        benchmark.time_test_it()

        benched_df = benchmark.get_data()
        df = df.append(benched_df, ignore_index=True)

    # Convert array size object to float64 for reasons unknown
    # See: https://stackoverflow.com/questions/28277137/how-to-convert-datatypeobject-to-float64-in-python
    df["arr_size"] = df["arr_size"].astype(float64)

    # Plot the data
    ch = chartify.Chart(blank_labels=True)
    ch.plot.scatter(
        data_frame=df,
        x_column='arr_size',
        y_column='runtime',
        color_column='name',
    )
    ch.style.color_palette.reset_palette_order()

    ch.set_title("Comparison of different sorting algorithms with varying array sizes")
    ch.set_subtitle("Comparisons made between self-implemented merge, "
                    "selection, and radix sort with python's builtin sorted")
    ch.axes.set_yaxis_label("Time in seconds to sort the array")
    ch.axes.set_xaxis_label("Size of the random array")
    ch.set_legend_location('top_right')

    ch.save("./benchmark.png", "png")


if __name__ == "__main__":
    main()
