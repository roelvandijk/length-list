Simple wrapper around an ordinary list which also stores the list's
length. For some functions this will result in improved
performance. The complexity of a function like `length` goes from O(n)
to O(1). Equality testing is also faster as long as the lists are
unequal. Most functions are slightly slower by a constant factor
because of the extra thunk containing the actual list and its length.
