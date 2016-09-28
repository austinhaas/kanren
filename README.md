# kanren

Implementations of the Kanren family of relational programming
languages, in Clojure.

These are my personal, work-in-progress implementations.

A simple linked-list implementation is included, to emulate Scheme's
improper lists, which are useful for logic programs. Helper functions
are provided to convert Clojure sequences to linked-lists and
vice-versa.

The code has been split, regrettably, into numerous namespaces to
appease the Clojurescript mandate that confines macros to Clojure
source files. To make the API more friendly, I've included a namespace,
cKanren-api, which aggregates most vars.

## Notice

The muKanren and rKanren implementations are likely broken at the
moment. Originally, I had worked up from muKanren and built each
implementation on top of the next, but I have since focused on cKanren
and allowed the other two implementations to languish.

This library has not been extensively tested.

## Usage

See the demo.clj files in each implementation subdirectory.

## Reference

* miniKanren: https://mitpress.mit.edu/books/reasoned-schemer, https://scholarworks.iu.edu/dspace/bitstream/handle/2022/8777/Byrd_indiana_0093A_10344.pdf
* µKanren: http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
* cKanren: http://www.schemeworkshop.org/2011/papers/Alvis2011.pdf
* rKanren: http://cswords.com/paper/rkanren.pdf

## License

Copyright © 2016 Austin Haas

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
