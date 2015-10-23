##ideas for the runtime ##

### matrix struct

- data structure to hold a n x m matrix
- only legal argument to a `foreach ... in ...` loop (other than the iterative index)

untyped concept for matrix stuct:
```
struct Array{

  void *ptr;
  char *type;
  size_t length;

};
```

### foreach <index> in <matrix>
- pass by value
- if called `async`, break the matrix up in four slices
  - [0 - col/2, 0 - row/2]
  - [col/2+ 1 - end, 0 - row/2]
  - [0 - col/2, row/2 + 1 - end]
  - [col/2+ 1 - end, row/2 + 1 - end]
- need to account for edge cases
  - 1D matrix (ie array)
  - 1D matrix (vertical)
    - can we optimize this into an array?

