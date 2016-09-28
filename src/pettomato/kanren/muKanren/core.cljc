(ns pettomato.kanren.muKanren.core
  (:require
   [pettomato.kanren.muKanren.types :refer [lvar? lvar=?]]))

(def empty-s {})

(defn ext-s [x v s] (assoc s x v))

(defn walk [u s]
  (let [x (get s u ::not-found)]
    (if (= x ::not-found)
      u
      (recur x s))))

(defn unify [u v s]
  (let [u (walk u s)
        v (walk v s)]
    (cond
     (and (lvar? u) (lvar? v) (lvar=? u v)) s
     (lvar? u)                              (ext-s u v s)
     (lvar? v)                              (ext-s v u s)
     (and (coll? u) (coll? v))              (let [s (unify (first u) (first v) s)]
                                              (and s (unify (next u) (next v) s)))
     :else                                  (and (= u v) s))))
