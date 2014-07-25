(ns com.pettomato.kanren.cKanren.disequality
  (:require
   [com.pettomato.kanren.cKanren.lvar :refer [any-lvar? recover-lvars]]
   [com.pettomato.kanren.cKanren.streams :refer [unit]]
   [com.pettomato.kanren.cKanren.pkg :refer [ext-c]]
   [com.pettomato.kanren.cKanren.constraint-helpers :refer [oc->prefix oc->rator]]
   [com.pettomato.kanren.cKanren.miniKanren :refer [walk* unify-prefix]]
   [com.pettomato.kanren.cKanren.cKanren :refer [subsumes? run-constraints]]
   #+clj
   [com.pettomato.kanren.cKanren.build-oc :refer [build-oc]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.build-oc :refer [build-oc]]))

(declare !=c-NEQ)

(defn normalize-store [p]
  (fn [{:keys [c] :as pkg}]
    (loop [c c, c' ()]
      (cond
       (empty? c)               (let [c'' (ext-c (build-oc !=c-NEQ p) c')]
                                  (assoc pkg :c c''))

       (= (oc->rator (first c))
          (quote !=c-NEQ))      (let [oc (first c)
                                      p' (oc->prefix oc)]
                                  (cond
                                   (subsumes? p' p ) pkg
                                   (subsumes? p  p') (recur (rest c) c')
                                   :else             (recur (rest c) (cons oc c'))))

          :else                  (recur (rest c) (cons (first c) c'))))))

(defn !=c-NEQ [p]
  (fn [{:keys [s] :as pkg}]
    (if-let [sp (unify-prefix (seq p) s)]
      (let [[s' p'] sp]
        (if (empty? p')
          false
          ((normalize-store p') pkg)))
      pkg)))

(defn !=c [u v]
  (fn [{:keys [s] :as pkg}]
    (if-let [sp (unify-prefix (list [u v]) s)]
      (let [[s' p] sp]
        ((!=c-NEQ p) pkg))
      pkg)))

(defn process-prefix-NEQ [p c]
  (run-constraints (recover-lvars p) c))

(defn enforce-constraints-NEQ [x] unit)

(defn reify-constraints-NEQ [m r]
  (fn [{:keys [c] :as pkg}]
    (let [c  (walk* c r)
          p* (->> (map oc->prefix c)
                  (remove any-lvar?))]
      (if (empty? p*)
        m
        `(~m :- (~'!= ~@p*))))))
