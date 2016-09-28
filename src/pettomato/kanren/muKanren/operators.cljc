(ns pettomato.kanren.muKanren.operators
  (:refer-clojure :exclude [== conj disj])
  (:require
   [pettomato.kanren.muKanren.types :refer [mzero unit choice]]
   #+clj
   [pettomato.kanren.muKanren.core-macros :refer [case-inf]])
  #+cljs
  (:require-macros
   [pettomato.kanren.muKanren.core-macros :refer [case-inf]]))

(defn mplus [a-inf f]
  (case-inf a-inf
            []     (force f)
            [f']   (delay (mplus f (force f')))
            [a]    (choice a f)
            [a f'] (choice a (delay (mplus f f')))))

(defn bind [a-inf g]
  (case-inf a-inf
            []     mzero
            [f]    (delay (bind (force f) g))
            [a]    (g a)
            [a f]  (mplus (g a) (delay (bind f g)))))

(defn disj [g1 g2] (fn [a] (mplus (g1 a) (g2 a))))

(defn conj [g1 g2] (fn [a] (bind (g1 a) g2)))
