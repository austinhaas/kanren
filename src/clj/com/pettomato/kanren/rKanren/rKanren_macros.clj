(ns com.pettomato.kanren.rKanren.rKanren-macros
  (:require
   [com.pettomato.kanren.rKanren.types :refer [lvar]]
   [com.pettomato.kanren.rKanren.rank :refer [get-rank min-rank inc-rank]]
   [com.pettomato.kanren.rKanren.rKanren :refer [mplus bind reify-var take* empty-pkg]]
   [com.pettomato.kanren.rKanren.rdelay :refer [rdelay]]))

(defmacro mplus*
  ([e] e)
  ([e & es]
     `(let [e-rank# (get-rank ~e)
            min-rank# (min-rank [~@es])]
        (if (< e-rank# min-rank#)
          (mplus ~e (rdelay min-rank# (mplus* ~@es)))
          (mplus (rdelay min-rank# (mplus* ~@es))
                 (rdelay e-rank# ~e))))))

(defmacro bind*
  ([e] e)
  ([e g & gs] `(bind* (bind ~e ~g) ~@gs)))

(defmacro conde
  [& clauses]
  (let [a (gensym)
        a' (gensym)]
    `(fn [~a]
       (rdelay
        (get-rank ~a)
        (let [~a' (inc-rank ~a)]
          (mplus*
           ~@(for [[g & gs] clauses]
               `(bind* (~g ~a') ~@gs))))))))

(defmacro condr
  [& clauses]
  (let [a (gensym)
        a' (gensym)]
    `(fn [~a]
       (rdelay
        (get-rank ~a)
        (let [~a' (inc-rank ~a)]
          (mplus*
           ~@(for [[r g & gs] clauses]
               `(bind* (~g (add-rank ~a' ~r)) ~@gs))))))))

(defmacro fresh
  [[& vars] g & gs]
  `(fn [a#]
     (rdelay
      (get-rank a#)
      (let [~@(interleave vars (repeat `(lvar)))]
        (bind* (~g a#) ~@gs)))))

(defmacro run* [[v & vars] g & gs]
  `(let [~v (lvar)
         ~@(interleave vars (repeatedly lvar))]
     (map #(reify-var ~v %)
          (take* (bind* (~g empty-pkg) ~@gs)))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
