# kanren

Implementations of the Kanren family of relational programming
languages, in Clojure.

These are my personal, work-in-progress adaptations. I've tried to
keep them simple, and consistent with the source papers, but I've made
changes to suit Clojure, personal style, and for runtime
efficiency. I'm also building the cKanren and rKanren code on µKanren,
so those implementations differ from than the originals.

The cKanren implementation only contains disequality constraints.

The rKanren implementation is not finished.

A simple linked list implementation is included, to emulate Scheme's
improper lists, which are so useful for logic programs. Helper
functions are provided to convert Clojure sequences to linked lists
and vice-versa.

Clojure data structures are minimally supported: lists, vectors, and
sets can be walked, but that's about it.

## Usage

See `src/com/pettomato/kanren/demo.clj`.

## Reference

### miniKanren

https://scholarworks.iu.edu/dspace/bitstream/handle/2022/8777/Byrd_indiana_0093A_10344.pdf

### µKanren

http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

### cKanren

http://www.schemeworkshop.org/2011/papers/Alvis2011.pdf

### rKanren

http://cswords.com/paper/rkanren.pdf

## License

Copyright © 2014 Austin Haas

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
