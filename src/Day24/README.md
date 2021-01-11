## Day 24: Lobby Layout

Your raft makes it to the tropical island; it turns out that the small crab was an excellent navigator. You make your way to the resort.

As you enter the lobby, you discover a small problem: the floor is being renovated. You can't even reach the check-in desk until they've finished installing the _new tile floor_ .

The tiles are all _hexagonal_ ; they need to be arranged in a [hex grid][1] with a very specific color pattern. Not in the mood to wait, you offer to help figure out the pattern.

The tiles are all _white_ on one side and _black_ on the other. They start with the white side facing up. The lobby is large enough to fit whatever pattern might need to appear there.

A member of the renovation crew gives you a _list of the tiles that need to be flipped over_ (your puzzle input). Each line in the list identifies a single tile that needs to be flipped by giving a series of steps starting from a _reference tile_ in the very center of the room. (Every line starts from the same reference tile.)

Because the tiles are hexagonal, every tile has _six neighbors_ : east, southeast, southwest, west, northwest, and northeast. These directions are given in your list, respectively, as `e` , `se` , `sw` , `w` , `nw` , and `ne` . A tile is identified by a series of these directions with _no delimiters_ ; for example, `esenee` identifies the tile you land on if you start at the reference tile and then move one tile east, one tile southeast, one tile northeast, and one tile east.

Each time a tile is identified, it flips from white to black or from black to white. Tiles might be flipped more than once. For example, a line like `esew` flips a tile immediately adjacent to the reference tile, and a line like `nwwswee` flips the reference tile itself.

Here is a larger example:

```
sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew
```

In the above example, 10 tiles are flipped once (to black), and 5 more are flipped twice (to black, then back to white). After all of these instructions have been followed, a total of _`10`_ tiles are _black_ .

Go through the renovation crew's list and determine which tiles they need to flip. After all of the instructions have been followed, _how many tiles are left with the black side up?_

## Link

[https://adventofcode.com/2020/day/24][2]

[1]: https://en.wikipedia.org/wiki/Hexagonal_tiling
[2]: https://adventofcode.com/2020/day/24
