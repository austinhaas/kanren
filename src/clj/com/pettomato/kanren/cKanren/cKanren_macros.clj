(ns com.pettomato.kanren.cKanren.cKanren-macros
  (:require
   [com.pettomato.kanren.cKanren.types :refer [lvar]]
   [com.pettomato.kanren.muKanren.operators :refer [mplus bind]]
   [com.pettomato.kanren.cKanren.core :refer [reify-var empty-pkg goal-construct]]
   [com.pettomato.kanren.cKanren.extras :refer [take*]]))

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

(defmacro run* [[v & vars] g & gs]
  `(let [~v (lvar)
         ~@(interleave vars (repeatedly lvar))]
     (map #(reify-var ~v %)
          (take* (bind* (~g empty-pkg) ~@gs)))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
