(ns com.pettomato.kanren.muKanren.types)

(defn lvar [c] [:lvar c])

(defn lvar? [x] (and (vector? x) (= (first x) :lvar)))

(defn lvar=? [x1 x2] (= (second x1) (second x2)))

(def mzero false)

(def mzero? false?)

(defn unit [a] a)

(def unit? map?)

(defn choice [a f] [a f])
