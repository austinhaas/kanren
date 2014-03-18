(ns com.pettomato.kanren.cKanren.extras
  (:require
   [com.pettomato.kanren.cKanren.cKanren :as c]
   [com.pettomato.kanren.muKanren.muKanren :as mu]))

(defn freshen [vars t]
  (let [smap (zipmap vars (repeatedly c/lvar))]
    (clojure.walk/prewalk-replace smap t)))

(defn disj+r [gs]
  (cond
   (empty? gs)        (c/goal-construct identity)
   (empty? (rest gs)) (first gs)
   :else              (mu/disj (first gs) (disj+r (rest gs)))))
