(ns com.pettomato.kanren.rKanren.rdelay
  (:require
   [com.pettomato.kanren.rKanren.rank :refer [set-rank]]))

(defmacro rdelay [r & body]
  `(set-rank
    (fn [] ~@body)
    ~r))
