# kanren

Implementations of the Kanren family of relational programming
languages, in Clojure.

These are my personal, work-in-progress adaptations. I've tried to
keep them simple, and consistent with the source papers, but I've made
changes to suit Clojure, for personal style, and for runtime
efficiency.

The cKanren implementation only contains disequality constraints.

A simple linked list implementation is included, to emulate Scheme's
improper lists, which are so useful for logic programs. Helper
functions are provided to convert Clojure sequences to linked lists
and vice-versa.

Clojure data structures are minimally supported: lists, vectors, and
sets can be walked, but that's about it.

## Usage

See the demo.clj files in each implementation subdirectory.

## Reference

* miniKanren: https://mitpress.mit.edu/books/reasoned-schemer, https://scholarworks.iu.edu/dspace/bitstream/handle/2022/8777/Byrd_indiana_0093A_10344.pdf
* µKanren: http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
* cKanren: http://www.schemeworkshop.org/2011/papers/Alvis2011.pdf
* rKanren: http://cswords.com/paper/rkanren.pdf

## License

Copyright © 2014 Austin Haas

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
