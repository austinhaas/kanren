(ns com.pettomato.kanren.rKanren.goals
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.util.llist :refer (empty-llist lcons)]
   #+clj
   [com.pettomato.kanren.rKanren.rKanren :refer (== != fresh conde unit mzero reify-var)]
   #+cljs
   [com.pettomato.kanren.rKanren.rKanren :refer (== != unit mzero reify-var)])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.rKanren.rKanren] :refer [fresh conde]))

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
      [(!= x head) (membero x tail)])))

(defn nonmembero
  "A relation where l is a collection, such that l does not contain x."
  [x l]
  (conde
    [(emptyo l)]
    [(fresh [head tail]
       (conso head tail l)
       (!= x head)
       (nonmembero x tail))]))

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
