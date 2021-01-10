## Notes

Looking at the example, how do we go from paths from previous to the final
answer? We want a cumulative sum of each of the valid previous paths.

```
# Paths from previous
    1  1  1  2  3  1  1  2  1  1  1  1
# Cumulative paths from previous
    1  1  1  2  4  4  4  8  8  8  8  8
======================================
 0  1  4  5  6  7 10 11 12 15 16 19 22
 0  1  4  5  6  7 10    12 15 16 19 22
 0  1  4  5     7 10 11 12 15 16 19 22
 0  1  4  5     7 10    12 15 16 19 22
 0  1  4     6  7 10 11 12 15 16 19 22
 0  1  4     6  7 10    12 15 16 19 22
 0  1  4        7 10 11 12 15 16 19 22
 0  1  4        7 10    12 15 16 19 22
```

For example, there are 4 ways to 7 because it sums the # of path to itself:

```
[# of paths to 4, # of paths to 5, # of paths to 6]
[              1,               1,               2] = 4
```

From there, there's only 1 path from 7 to 10. However, there are 4 paths to 7,
meaning there are 4 paths to 10
