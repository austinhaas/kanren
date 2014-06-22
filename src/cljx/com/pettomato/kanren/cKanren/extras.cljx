(ns com.pettomato.kanren.cKanren.extras
  (:refer-clojure :exclude [disj])
  (:require
   [com.pettomato.kanren.cKanren.types :refer [lvar]]
   [com.pettomato.kanren.cKanren.core :refer [goal-construct]]
   [com.pettomato.kanren.muKanren.extras :as mu]
   [com.pettomato.kanren.muKanren.operators :refer [disj]]))

(def take* mu/take*)

(defn freshen [vars t]
  (let [smap (zipmap vars (repeatedly lvar))]
    (clojure.walk/prewalk-replace smap t)))

(defn disj+r [gs]
  (cond
   (empty? gs)        (goal-construct identity)
   (empty? (rest gs)) (first gs)
   :else              (disj (first gs) (disj+r (rest gs)))))
