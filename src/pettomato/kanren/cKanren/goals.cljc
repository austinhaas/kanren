(ns pettomato.kanren.cKanren.goals
  (:refer-clojure :exclude [==])
  (:require
   [pettomato.kanren.util.llist :refer [empty-llist lcons]]
   [pettomato.kanren.cKanren.lvar :refer [lvar]]
   [pettomato.kanren.cKanren.streams :refer [unit mzero]]
   [pettomato.kanren.cKanren.miniKanren :refer [take* unify+delta]]
   [pettomato.kanren.cKanren.cKanren :refer [reify-var goal-construct process-delta]]
   #?(:clj
      [pettomato.kanren.cKanren.miniKanren-operators :refer [fresh conde condu]]))
  #?(:cljs
     (:require-macros
      [pettomato.kanren.cKanren.miniKanren-operators :refer [fresh conde condu]])))

(defn ==c [u v]
  (fn [{:keys [s c] :as pkg}]
    (if-let [sp (unify+delta s (list [u v]))]
      (let [[s' p] sp]
        (if (empty? p)
          pkg
          (let [pkg' (assoc pkg :s s')]
            ((process-delta p c) pkg'))))
      false)))

(defn == [u v] (goal-construct (==c u v)))

(defn succeed [a] (unit a))

(defn fail [a] mzero)

(defn emptyo [l]
  (== l empty-llist))

(defn conso [a d l]
  (== (lcons a d) l))

(defn firsto [l a]
  (fresh [d]
    (conso a d l)))

(defn resto [l d]
  (fresh [a]
    (== (lcons a d) l)))

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

(defn onceo [g] (condu (g)))

(defn trace-lvar [msg v]
  (fn [{:keys [s] :as pkg}]
    (println msg (first (take* ((reify-var v) s))))
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
