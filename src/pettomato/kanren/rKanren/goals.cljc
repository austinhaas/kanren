(ns pettomato.kanren.rKanren.goals
  (:refer-clojure :exclude [==])
  (:require
   [pettomato.kanren.util.llist :refer [empty-llist lcons]]
   [pettomato.kanren.cKanren.cKanren-api :as c]
   [pettomato.kanren.rKanren.streams :refer [unit mzero]]
   #?(:clj
      [pettomato.kanren.rKanren.rKanren-macros :refer [fresh conde]]))
  #?(:cljs
     (:require-macros
      [pettomato.kanren.rKanren.rKanren-macros :refer [fresh conde]])))

(def == c/==)
(def != c/!=)

(defn succeed [a] (unit a))

(defn fail [a] mzero)

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
    (println msg (c/reify-var v s))
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
