(ns com.pettomato.kanren.cKanren.cKanren-macros
  (:require
   [com.pettomato.kanren.cKanren.lvar :refer [lvar]]
   [com.pettomato.kanren.cKanren.streams :refer [mzero]]
   [com.pettomato.kanren.cKanren.operators :refer [mplus bind]]
   [com.pettomato.kanren.cKanren.case-inf :refer [case-inf]]))

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
