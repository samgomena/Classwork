
__author__ = "sgomena"

def selection_sort(m):
    """
    This functions sorts a list m using an implementation of the selection sort algorithm.

    :param m: An `np.array` of "random" integers
    :return: A sorted representation of `m`
    :rtype: np.array
    """
    list_len = len(m)
    for i in range(list_len):
        min = i
        for j in range(i+1, list_len):
            if m[j] < m[min]:
                min = j
        
        # Swap lowest found and element at i 
        m[i], m[min] = m[min], m[i]
    return m