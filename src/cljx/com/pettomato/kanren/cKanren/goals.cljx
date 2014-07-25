(ns com.pettomato.kanren.cKanren.goals
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.util.llist :refer [empty-llist lcons]]
   [com.pettomato.kanren.cKanren.lvar :refer [lvar]]
   [com.pettomato.kanren.cKanren.streams :refer [unit mzero]]
   [com.pettomato.kanren.cKanren.core :refer [unify-prefix reify-var goal-construct process-prefix]]
   #+clj
   [com.pettomato.kanren.cKanren.cKanren-macros :refer [fresh conde condu]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.cKanren-macros :refer [fresh conde condu]]))

(defn ==c [u v]
  (fn [{:keys [s c] :as pkg}]
    (if-let [sp (unify-prefix (list [u v]) s)]
      (let [[s' p] sp]
        (if (empty? p)
          pkg
          (let [pkg' (assoc pkg :s s')]
            ((process-prefix p c) pkg'))))
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

(defn trace-lvar [msg v]
  (assert false)
  #_(fn [{:keys [s] :as pkg}]
    (println msg (reify-var v s))
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

(defn onceo [g] (condu (g)))
