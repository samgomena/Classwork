The following algorithms (and their theoretical complexities) were analyzed in this experiment:
* merge sort O(nlogn)
* selection sort O(n^2)
* radix sort O(kn) where k is the length of the longest digit

Given the theoretical complexities it's clear that radix sort should run the quickest, followed by merge sort, and then selection sort.
And an empirical analysis of their respective run times leads us to the same conclusion.



