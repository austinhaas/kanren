(ns pettomato.kanren.cKanren.lvar)

(def lvar-counter (atom 0))

(defn lvar [] [:lvar (swap! lvar-counter inc)])

(defn lvar? [x] (and (vector? x) (= (first x) :lvar)))

(def lvar=? identical?)

(defn any-lvar? [t]
  (cond
   (lvar? t) true
   (coll? t) (some any-lvar? t)
   :else     false))

(defn any-relevant-lvar? [t x*]
  (cond
   (lvar? t) (some #{t} x*)
   (coll? t) (some #(any-relevant-lvar? % x*) t)
   :else     false))

(defn recover-lvars [p]
  (reduce (fn [r [x v]]
            (if (lvar? v)
              (conj r x v)
              (conj r x)))
          #{}
          p))
