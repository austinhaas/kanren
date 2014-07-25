(ns com.pettomato.kanren.cKanren.disequality-goals
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.cKanren.cKanren :refer [goal-construct]]
   [com.pettomato.kanren.cKanren.goals :refer [== emptyo conso]]
   [com.pettomato.kanren.cKanren.disequality :refer [!=c]]
   #+clj
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [fresh conde]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [fresh conde]]))

(defn != [u v] (goal-construct (!=c u v)))

(defn membero [x l]
  (fresh [head tail]
    (conso head tail l)
    (conde
      [(== x head)]
      [(!= x head) (membero x tail)])))

(defn nonmembero
  "A relation where l is a collection, such that l does not contain x."
  [x l]
  (conde
    [(emptyo l)]
    [(fresh [head tail]
       (conso head tail l)
       (!= x head)
       (nonmembero x tail))]))
