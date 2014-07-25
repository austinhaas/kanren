(ns com.pettomato.kanren.cKanren.streams)

(def  mzero  false)
(def  unit   identity)
(defn choice [a f] [a f])

(def mzero? false?)
(def unit?  map?)

(def empty-f (delay mzero))
