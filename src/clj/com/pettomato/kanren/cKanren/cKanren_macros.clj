(ns com.pettomato.kanren.cKanren.cKanren-macros
  (:require
   [com.pettomato.kanren.cKanren.types :refer [lvar mzero]]
   [com.pettomato.kanren.cKanren.operators :refer [mplus bind]]
   [com.pettomato.kanren.cKanren.core :refer [reify-var empty-pkg]]
   [com.pettomato.kanren.cKanren.extras :refer [take*]]
   [com.pettomato.kanren.cKanren.core-macros :refer [case-inf]]))

(defmacro mplus*
  ([e] e)
  ([e & es] `(mplus ~e (delay (mplus* ~@es)))))

(defmacro bind*
  ([e] e)
  ([e g & gs] `(bind* (bind ~e ~g) ~@gs)))

(defmacro conde
  [& clauses]
  (let [a (gensym)]
    `(fn [~a]
       (delay
        (mplus*
         ~@(for [[g & gs] clauses]
             `(bind* (~g ~a) ~@gs)))))))

(defmacro fresh
  [[& vars] g & gs]
  `(fn [a#]
     (delay
      (let [~@(interleave vars (repeat `(lvar)))]
        (bind* (~g a#) ~@gs)))))

(defmacro all
  [g & gs]
  `(fn [a#]
     (delay
      (bind* (~g a#) ~@gs))))

(defmacro run* [[v & vars] g & gs]
  `(let [~v (lvar)
         ~@(interleave vars (repeatedly lvar))]
     (map #(reify-var ~v %)
          (take* (bind* (~g empty-pkg) ~@gs)))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))

;;; Impure control operators.

(defmacro if-u
  ([] mzero)
  ([[e & gs] & bs]
     `(letfn [(step# [a-inf#]
                (case-inf a-inf#
                          []      (if-u ~@bs)
                          [f#]    (delay (step# (force f#)))
                          [a#]    (bind* a-inf# ~@gs)
                          [a# f#] (bind* a# ~@gs)))]
        (step# ~e))))

(defmacro condu
  [& clauses]
  (let [a (gensym)]
    `(fn [~a]
       (delay
        (if-u ~@(for [[g & gs] clauses]
                  `[(~g ~a) ~@gs]))))))
