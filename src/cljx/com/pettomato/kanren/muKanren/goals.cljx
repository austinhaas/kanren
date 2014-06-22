(ns com.pettomato.kanren.muKanren.goals
  (:refer-clojure :exclude [== disj])
  (:require
   [com.pettomato.kanren.util.llist :refer [empty-llist lcons]]
   [com.pettomato.kanren.muKanren.types :refer [unit mzero]]
   [com.pettomato.kanren.muKanren.operators :refer [disj]] ; This is needed, b/c conde expands to disj.
   [com.pettomato.kanren.muKanren.extras :refer [reify-var]]
   [com.pettomato.kanren.muKanren.core :refer [unify]]
   #+clj
   [com.pettomato.kanren.muKanren.extras-macros :refer [fresh conde]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.muKanren.extras-macros :refer [fresh conde]]))

(defn == [u v]
  (fn [{:keys [s] :as pkg}]
    (if-let [s' (unify u v s)]
      (unit (assoc pkg :s s'))
      mzero)))

(defn succeed [a] (unit a))

(defn fail [a] mzero)

(defn emptyo [l]
  (== l empty-llist))

(defn conso [a d l]
  (== (lcons a d) l))

(defn membero [x l]
  (fresh [head tail]
    (conso head tail l)
    (conde
      [(== x head)]
      [(membero x tail)])))

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
  (fn [{:keys [s] :as pkg}]
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
