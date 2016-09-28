(ns pettomato.kanren.rKanren.streams)

(def  mzero  false)
(def  unit   identity)
(defn choice [a f] [a f])

(def mzero?       false?)
(def unit?        map?)
(def rdelay?      fn?)
(defn choice? [x] (and (vector? x) (= (count x) 2)))
