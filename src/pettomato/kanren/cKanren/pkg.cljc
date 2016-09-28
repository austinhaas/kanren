(ns pettomato.kanren.cKanren.pkg
  (:require
   [pettomato.kanren.cKanren.lvar :refer [any-lvar?]]
   [pettomato.kanren.cKanren.oc :refer [oc->rands]]))

(def empty-s {})
(def empty-d {})
(def empty-c ())

(defn ext-s [s x v] (assoc s x v))

(defn ext-d [d x dom] (assoc d x dom))

(defn ext-c [c oc]
  (if (any-lvar? (oc->rands oc))
    (conj c oc)
    c))

(def empty-pkg
  {:s empty-s
   :d empty-d
   :c empty-c})
