(ns pettomato.kanren.rKanren.rdelay
  (:require
   [pettomato.kanren.rKanren.rank :refer [set-rank]]))

(defmacro rdelay [r & body]
  `(set-rank
    (fn [] ~@body)
    ~r))
