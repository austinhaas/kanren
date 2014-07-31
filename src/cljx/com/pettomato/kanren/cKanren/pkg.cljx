(ns com.pettomato.kanren.cKanren.pkg
  (:require
   [com.pettomato.kanren.cKanren.lvar :refer [any-lvar?]]
   [com.pettomato.kanren.cKanren.constraint-helpers :refer [oc->rands]]))

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
