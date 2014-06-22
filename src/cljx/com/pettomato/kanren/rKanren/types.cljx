(ns com.pettomato.kanren.rKanren.types
  (:require
   [com.pettomato.kanren.cKanren.types :as c]))

(def lvar c/lvar)

(def mzero  c/mzero)
(def unit   c/unit)
(def choice c/choice)

(def mzero?  c/mzero?)
(def unit?   c/unit?)
(def rdelay? fn?)
(defn choice? [x] (and (vector? x) (= (count x) 2)))
