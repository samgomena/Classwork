
__author__ = "sgomena"

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