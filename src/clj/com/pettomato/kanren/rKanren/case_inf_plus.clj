(ns com.pettomato.kanren.rKanren.case-inf-plus
  (:require
   [com.pettomato.kanren.rKanren.types :refer [mzero? rdelay? unit? choice?]]
   [com.pettomato.kanren.rKanren.rank :refer [inc-rank]]))

(defmacro case-inf+
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (mzero? a#)  (inc-rank ~e0)
      (rdelay? a#) (let [~f' a#] (inc-rank ~e1))
      (unit? a#)   (let [~a' a#] (inc-rank ~e2))
      (choice? a#) (let [[~a ~f] a#] (inc-rank ~e3))
      :else (throw (Error. (str "Unknown type of a-inf:" a#) )))))
