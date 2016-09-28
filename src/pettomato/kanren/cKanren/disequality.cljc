(ns pettomato.kanren.cKanren.disequality
  (:require
   [pettomato.kanren.cKanren.lvar :refer [any-lvar? recover-lvars]]
   [pettomato.kanren.cKanren.streams :refer [unit]]
   [pettomato.kanren.cKanren.pkg :refer [ext-c]]
   [pettomato.kanren.cKanren.oc :refer [oc->delta oc->rator]]
   [pettomato.kanren.cKanren.miniKanren :refer [walk* unify+delta]]
   [pettomato.kanren.cKanren.cKanren :refer [subsumes? run-constraints]]
   #?(:clj
      [pettomato.kanren.cKanren.build-oc :refer [build-oc]]))
  #?(:cljs
     (:require-macros
      [pettomato.kanren.cKanren.build-oc :refer [build-oc]])))

(declare !=c-NEQ)

(defn normalize-store [p]
  (fn [{:keys [c] :as pkg}]
    (loop [c c, c' ()]
      (cond
       (empty? c)               (update-in pkg [:c] ext-c (build-oc !=c-NEQ p))

       (= (oc->rator (first c))
          (quote !=c-NEQ))      (let [oc (first c)
                                      p' (oc->delta oc)]
                                  (cond
                                   (subsumes? p' p ) pkg
                                   (subsumes? p  p') (recur (rest c) c')
                                   :else             (recur (rest c) (cons oc c'))))

          :else                  (recur (rest c) (cons (first c) c'))))))

(defn !=c-NEQ [p]
  (fn [{:keys [s] :as pkg}]
    (if-let [sp (unify+delta s (seq p))]
      (let [[s' p'] sp]
        (if (empty? p')
          false
          ((normalize-store p') pkg)))
      pkg)))

(defn !=c [u v]
  (fn [{:keys [s] :as pkg}]
    (if-let [sp (unify+delta s (list [u v]))]
      (let [[s' p] sp]
        ((!=c-NEQ p) pkg))
      pkg)))

(defn process-delta-NEQ [p c]
  (run-constraints (recover-lvars p) c))

(defn enforce-constraints-NEQ [x] unit)

(defn reify-constraints-NEQ [m r]
  (fn [{:keys [c] :as pkg}]
    (let [c  (walk* c r)
          p* (->> (map oc->delta c)
                  (remove any-lvar?)
                  (into {}))]
      (if (empty? p*)
        m
        `(~m :- (~'!= ~p*))))))
