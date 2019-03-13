## Rundown of Algorithmic Analysis

#### Overview

Starting with a requirement to analyze three algorithms: merge sort, selection sort, and a third of our choice.
Analysis was then performed by temporally benchmarking each sorting algorithm on arrays of increasing size while measuring 
the amount of time it took to sort them.

Before talking about the results, we should clarify the testing procedure.

Each algorithm was tested by creating a benchmarking object around it that kept metrics on it while it was benchmarked.
Because of this, each benchmarking run of each algorithm was given its own array of random values to sort.
While this simplified the benchmarking, it's reasonable to assume that it lead to some variation between two algorithms. 
That is, one algorithm could have been given a very sparse array, while the other was given a very dense array 
-- though both arrays would have been the same length.

The arrays were generated using `numpy`'s random array generation tools, which were then converted to python's `list` type.
The array was given to a wrapper that isolated the algorithm's run time calculation from everything else, like so:

```python
def time_it(func, arr):
    start = perf_counter()
    func(arr)
    return perf_counter() - start
``` 

Note: `perf_counter()` is a builtin meant for measuring function runtime performance (and aims to be more precise than the system time).

For curiosities sake, I also ran the same benchmarking on python's builtin `sorted` function which uses Timsort (an amalgamation of mergesort and insertion sort).
While it has a theoretical temporal complexity of `O(nlogn)` it runs exceptionally faster in practice which is clearly evidenced in the graph.

All of the benchmarking results compiled here were ran on a 2.3 GHz MacBook Pro with 16 GB of RAM using Python 3.6.4. 

#### Analyzing Complexities

The algorithms and their theoretical worst, average, and best complexities:
* merge sort `O(nlogn)`, `O(nlogn)`, `O(nlogn)`
* selection sort `O(n^2)`, `O(n^2)`, `O(n^2)`; `O(n)`, `O(n)`, `O(n)` (comparisons; swaps)
* radix sort `O(kn)`, `O(kn)`, `O(kn)` where k is the magnitude of the largest digit
* `sorted` `O(nlogn)`, `O(nlogn)`, `O(n)`

Given the theoretical complexities of the three self-implemented algorithms it's clear that radix sort should run the quickest, 
followed by merge sort, and then selection sort.
And an empirical analysis of their respective run times leads us to the same conclusion as seen in the 
[graph](benchmark.png) of their run times.

Due to time constraints and available computing power, the graph isn't very populated with data points but I think there 
are enough to still see where each function begins to trend to.
 
Merge sort, for isntance, follows the path of nlogn almost exactly.
 
Selection sort, which with only a couple data points isn't very representative, still clearly shoots off in the n^2 direction.

And radix sort again follows a linear path almost exactly. 
 
I think it's safe to say that the graph provides a clear visual affirmation that each algorithms average run time 
complexity is what we should expect to see in practice.




