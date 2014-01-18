(ns com.pettomato.kanren.goals
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.llist :refer (empty-llist lcons llist*)]
   [com.pettomato.kanren.cKanren :refer (!= == fresh conde run walk*)]))

(defn emptyo [l]
  (== l empty-llist))

(defn conso [a d l]
  (== (lcons a d) l))

(defn membero [x l]
  (fresh [head tail]
    (conso head tail l)
    (conde
      [(== x head)]
      [(!= x head) (membero x tail)])))

(defn appendo [x y z]
  (conde
    [(emptyo x) (== y z)]
    [(fresh [a d r]
       (conso a d x)
       (conso a r z)
       (appendo d y r))]))

(defn nonmembero
  "A relation where l is a collection, such that l does not contain x."
  [x l]
  (conde
    [(emptyo l)]
    [(fresh [head tail]
       (conso head tail l)
       (!= x head)
       (nonmembero x tail))]))

(defn trace-lvar [msg v]
  (fn [a]
    (let [[[s d c] counter] a]
      (println msg (walk* v s)))
    a))

(defn trace-s [msg]
  (fn [a]
    (let [[[s d c] counter] a]
      (println msg s))
    a))
