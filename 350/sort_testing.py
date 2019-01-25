
import numpy as np
from time import perf_counter
from functools import wraps


def time_it(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = perf_counter()
        func_value = func(*args, **kwargs)
        delta = perf_counter() - start
        print(f"{func.__name__} took {delta}s to run")
        return func_value
    return wrapper

def selection_sort(m):
    list_len = len(m)
    for i in range(list_len):
        min = i
        for j in range(i+1, list_len):
            if m[j] < m[min]:
                min = j
        
        # Swap lowest found and element at i 
        m[i], m[min] = m[min], m[i]
    return m


def merge_sort(m):
    list_len = len(m)
    
    if list_len <= 1:
        return m

    # Recursive case. First, divide the list into equal-sized sublists
    # consisting of the first half and second half of the list.
    # This assumes lists start at index 0.
    middle = list_len // 2
    # print(f"len: {list_len}\nmiddle: {middle}")
    left, right = m[:middle], m[middle:]
    # print(f"left: {left}\nright: {right}")
    # return 
    # Recursively sort both sublists.
    left = merge_sort(left)
    right = merge_sort(right)

    # Then merge the now-sorted sublists.
    return merge(left, right)

def merge(left, right):
    merged = []

    while left and right:
        if left[0] <= right[0]:
            merged.append(left.pop(0))
        else:
            merged.append(right.pop(0))

    while left:
        merged.append(left.pop(0))
    while right:
        merged.append(right.pop(0))
    
    return merged

def gen_rand_arr(size=1_000, min=1, max=1000):
    return list(np.random.randint(min, max, size))

def test_em(func, arr):
    start = perf_counter()
    func(arr)
    return perf_counter() - start

def time_test_it(func, min_arr_size=1_000, increase_arr_size_by=5_000, max_time_allowed=120):
    # Update `arr_size` as our test continues
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
    time_test_it(merge_sort, max_time_allowed=max_time)
    time_test_it(selection_sort, max_time_allowed=max_time)

if __name__ == "__main__":
    main()