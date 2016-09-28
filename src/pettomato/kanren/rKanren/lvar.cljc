(ns pettomato.kanren.rKanren.lvar)

(def lvar-counter (atom 0))

(defn lvar [] [:lvar (swap! lvar-counter inc)])

(defn lvar? [x] (and (vector? x) (= (first x) :lvar)))

(def lvar=? identical?)
