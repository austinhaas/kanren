(ns com.pettomato.kanren.muKanren.core-macros
  (:require
   [com.pettomato.kanren.muKanren.types :refer [mzero? unit?]]))

(defmacro case-inf
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (mzero? a#) ~e0
      (delay? a#) (let [~f' a#] ~e1)
      (unit? a#)  (let [~a' a#] ~e2)
      :else       (let [[~a ~f] a#] ~e3))))
