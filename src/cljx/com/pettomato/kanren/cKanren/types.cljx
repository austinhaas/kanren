(ns com.pettomato.kanren.cKanren.types)

(def lvar-counter (atom 0))
(defn lvar [] [:lvar (swap! lvar-counter inc)])
(defn lvar? [x] (and (vector? x) (= (first x) :lvar)))
(def lvar=? identical?)

(defn any:lvar? [t]
  (cond
   (lvar? t) true
   (coll? t) (some any:lvar? t)
   :else     false))

(def  mzero  false)
(def  unit   identity)
(defn choice [a f] [a f])

(def mzero? false?)
(def unit? map?)
