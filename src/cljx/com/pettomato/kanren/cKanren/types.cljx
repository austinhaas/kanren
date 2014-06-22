(ns com.pettomato.kanren.cKanren.types
  (:require
   [com.pettomato.kanren.muKanren.types :as mu]))

(def lvar-counter (atom 0))
(defn lvar [] [:lvar (swap! lvar-counter inc)])
(defn lvar? [x] (and (vector? x) (= (first x) :lvar)))
(def lvar=? identical?)

(defn any:lvar? [t]
  (cond
   (lvar? t) true
   (coll? t) (some any:lvar? t)
   :else     false))

(def unit mu/unit)
(def mzero mu/mzero)
(def choice mu/choice)

(def unit? mu/unit?)
(def mzero? mu/mzero?)
