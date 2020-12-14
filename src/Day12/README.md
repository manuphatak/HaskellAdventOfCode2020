## Day 12: Rain Risk

Your ferry made decent progress toward the island, but the storm came in faster than anyone expected . The ferry needs to take _evasive actions_ !

Unfortunately, the ship's navigation computer seems to be malfunctioning; rather than giving a route directly to safety, it produced extremely circuitous instructions. When the captain uses the [PA system][1] to ask if anyone can help, you quickly volunteer.

The navigation instructions (your puzzle input) consists of a sequence of single-character _actions_ paired with integer input _values_ . After staring at them for a few minutes, you work out what they probably mean:

- Action _`N`_ means to move _north_ by the given value.
- Action _`S`_ means to move _south_ by the given value.
- Action _`E`_ means to move _east_ by the given value.
- Action _`W`_ means to move _west_ by the given value.
- Action _`L`_ means to turn _left_ the given number of degrees.
- Action _`R`_ means to turn _right_ the given number of degrees.
- Action _`F`_ means to move _forward_ by the given value in the direction the ship is currently facing.

The ship starts by facing _east_ . Only the `L` and `R` actions change the direction the ship is facing. (That is, if the ship is facing east and the next instruction is `N10` , the ship would move north 10 units, but would still move east if the following action were `F` .)

For example:

```
F10
N3
F7
R90
F11
```

These instructions would be handled as follows:

- `F10` would move the ship 10 units east (because the ship starts by facing east) to _east 10, north 0_ .
- `N3` would move the ship 3 units north to _east 10, north 3_ .
- `F7` would move the ship another 7 units east (because the ship is still facing east) to _east 17, north 3_ .
- `R90` would cause the ship to turn right by 90 degrees and face _south_ ; it remains at _east 17, north 3_ .
- `F11` would move the ship 11 units south to _east 17, south 8_ .

At the end of these instructions, the ship's [Manhattan distance][2] (sum of the absolute values of its east/west position and its north/south position) from its starting position is `17 + 8` \= _`25`_ .

Figure out where the navigation instructions lead. _What is the Manhattan distance between that location and the ship's starting position?_

## Link

[https://adventofcode.com/2020/day/12][3]

[1]: https://en.wikipedia.org/wiki/Public_address_system
[2]: https://en.wikipedia.org/wiki/Manhattan_distance
[3]: https://adventofcode.com/2020/day/12
