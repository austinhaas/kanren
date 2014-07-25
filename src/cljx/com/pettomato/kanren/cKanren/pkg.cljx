(ns com.pettomato.kanren.cKanren.pkg
  (:require
   [com.pettomato.kanren.cKanren.lvar :refer [any-lvar?]]
   [com.pettomato.kanren.cKanren.constraints :refer [oc->rands]]))

(def empty-s {})
(def empty-d {})
(def empty-c ())

(defn ext-s [x v s] (assoc s x v))

(defn ext-d [x fd d] (assoc d x fd))

(defn ext-c [oc c]
  (if (any-lvar? (oc->rands oc))
    (conj c oc)
    c))

(def empty-pkg
  {:s empty-s
   :d empty-d
   :c empty-c})
