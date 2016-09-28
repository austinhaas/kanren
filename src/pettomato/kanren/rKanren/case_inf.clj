(ns pettomato.kanren.rKanren.case-inf
  (:require
   [pettomato.kanren.rKanren.streams :refer [mzero? rdelay? unit? choice?]]))

(defmacro case-inf
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (mzero? a#)  ~e0
      (rdelay? a#) (let [~f' a#] ~e1)
      (unit? a#)   (let [~a' a#] ~e2)
      (choice? a#) (let [[~a ~f] a#] ~e3)
      :else (throw (Error. (str "Unknown type of a-inf:" a#) )))))
