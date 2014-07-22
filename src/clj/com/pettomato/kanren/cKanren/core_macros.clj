(ns com.pettomato.kanren.cKanren.core-macros
  (:require
   [com.pettomato.kanren.cKanren.types :refer [mzero? unit?]]))

(defmacro case-inf
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (mzero? a#) ~e0
      (delay? a#) (let [~f' a#] ~e1)
      (unit? a#)  (let [~a' a#] ~e2)
      :else       (let [[~a ~f] a#] ~e3))))

(defmacro build-aux-oc [op args zs args2]
  (if (empty? args)
    `(let [~@(interleave zs args)]
       (list (~op ~@zs) (quote ~op) ~@zs))
    `(build-aux-oc ~op ~(rest args) ~(cons (first args) zs) ~args)))

(defmacro build-oc [op & args]
  `(build-aux-oc ~op ~args () ~args))

(defmacro build-oc2 [op & args]
  `(list (~op ~@args) ~op ~@args))
