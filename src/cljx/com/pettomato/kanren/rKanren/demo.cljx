(ns com.pettomato.kanren.rKanren.demo
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.rKanren
    #+clj
    [rKanren :refer (== != fresh conde condr run* run)]
    #+cljs
    [rKanren :refer (== !=)]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.cKanren :refer [fresh conde condr run* run]]))

#_(run* [q]
    (condr
     [3 (== q 'second)]
     [5 (== q 'third)]
     [1 (== q 'first)]))

(defn recur-e [e]
  (fresh [a b]
    (conde
      [(== e ['x])]
      [(== e ['b a]) (recur-e a)]
      [(== e ['a b]) (recur-e b)])))

#_(run 5 [q] (recur-e q))

(defn recur-r [e]
  (fresh [a b]
    (condr
     [10 (== e ['x])]
     [ 4 (== e ['b a]) (recur-r a)]
     [ 2 (== e ['a b]) (recur-r b)])))

#_(run 5 [q] (recur-r q))
