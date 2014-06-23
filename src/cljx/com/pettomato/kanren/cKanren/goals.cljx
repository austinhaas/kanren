(ns com.pettomato.kanren.cKanren.goals
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.util.llist :refer [empty-llist lcons]]
   [com.pettomato.kanren.cKanren.types :refer [unit mzero]]
   [com.pettomato.kanren.cKanren.core :refer [reify-var ==c !=c goal-construct]]
   #+clj
   [com.pettomato.kanren.cKanren.cKanren-macros :refer [fresh conde]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.cKanren-macros :refer [fresh conde]]))

(defn == [u v] (goal-construct (==c u v)))

(defn != [u v] (goal-construct (!=c u v)))

(defn succeed [a] (unit a))

(defn fail [a] mzero)

(defn emptyo [l]
  (== l empty-llist))

(defn conso [a d l]
  (== (lcons a d) l))

(defn firsto [l a]
  (fresh [d]
    (conso a d l)))

(defn resto [l d]
  (fresh [a]
    (== (lcons a d) l)))

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

(defn appendo [x y z]
  (conde
    [(emptyo x) (== y z)]
    [(fresh [a d r]
       (conso a d x)
       (conso a r z)
       (appendo d y r))]))

(defn anyo [g]
  (conde
    [g succeed]
    [(anyo g)]))

(def alwayso (anyo succeed))

(defn trace-lvar [msg v]
  (fn [{:keys [s] :as pkg}]
    (println msg (reify-var v s))
    (unit pkg)))

(defn trace-pkg [msg]
  (fn [pkg]
    (println msg pkg)
    (unit pkg)))

(defn trace-s [msg]
  (fn [{:keys [s] :as pkg}]
    (println msg s)
    (unit pkg)))

(defn log [msg]
  (fn [{:keys [s] :as pkg}]
    (println msg)
    (unit pkg)))
