(ns com.pettomato.kanren.cKanren.extras
  (:refer-clojure :exclude [disj])
  (:require
   [com.pettomato.kanren.cKanren.lvar :refer [lvar]]
   [com.pettomato.kanren.cKanren.core :refer [goal-construct]]
   [com.pettomato.kanren.cKanren.operators :refer [disj]]
   #+clj
   [com.pettomato.kanren.cKanren.case-inf :refer [case-inf]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.case-inf :refer [case-inf]]))

(defn take* [f]
  (case-inf (force f)
            []    ()
            [f]   (recur f)
            [a]   (cons a ())
            [a f] (cons a (lazy-seq (take* f)))))

(defn freshen [vars t]
  (let [smap (zipmap vars (repeatedly lvar))]
    (clojure.walk/prewalk-replace smap t)))

(defn disj+r [gs]
  (cond
   (empty? gs)        (goal-construct identity)
   (empty? (rest gs)) (first gs)
   :else              (disj (first gs) (disj+r (rest gs)))))
