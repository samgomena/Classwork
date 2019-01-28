def counting_sort(arr, max_value, get_index):
    counts = [0] * max_value

    # Counting - O(n)
    for a in arr:
        counts[get_index(a)] += 1
  
    # Accumulating - O(k)
    #   for i in range(len(counts)):
    for i in range(max_value):
        if i != 0:
            counts[i] += counts[i-1]

  # Calculating start index - O(k)
    for i, c in enumerate(counts[:-1]):
        if i == 0:
            counts[i] = 0
        counts[i+1] = c

    ret = [None] * len(arr)
    # Sorting - O(n)
    for a in arr:
        idx = get_index(a)
        index = counts[idx]
        ret[index] = a
        counts[idx] += 1
  
    return ret

def get_digit(n, d):
    for i in range(d-1):
        n //= 10
    return n % 10

def get_num_digit(n):
    i = 0
    while n > 0:
        n //= 10
        i += 1
    return i

def radix_sort(m):
    max_value = max(m)
    num_digits = get_num_digit(max_value)
    # num_digits = int(len(str(max_value)))
    # O(k(n+k))
    for d in range(num_digits):
        # Counting sort takes O(n+k)
        m = counting_sort(m, max_value, lambda a: get_digit(a, d+1))
    return m
