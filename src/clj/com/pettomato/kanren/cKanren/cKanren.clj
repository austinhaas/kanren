(ns com.pettomato.kanren.cKanren.cKanren
  (:refer-clojure :exclude [== conj disj]))

(defmacro build-aux-oc [op args zs args2]
  (if (empty? args)
    `(let [~@(interleave zs args)]
       (list (~op ~@zs) (var ~op) ~@zs))
    `(build-aux-oc ~op ~(rest args) ~(cons (first args) zs) ~args)))

(defmacro build-oc [op & args]
  `(build-aux-oc ~op ~args () ~args))

(defmacro build-oc2 [op & args]
  `(list (~op ~@args) ~op ~@args))

(defmacro mplus*
  ([e] e)
  ([e & es] `(mu/mplus ~e (delay (mplus* ~@es)))))

(defmacro bind*
  ([e] e)
  ([e g & gs] `(bind* (mu/bind ~e ~g) ~@gs)))

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
      (let [~@(interleave vars (repeat (list lvar)))]
        (bind* (~g a#) ~@gs)))))

(defmacro run* [[v & vars] g & gs]
  `(let [~v (lvar)
         ~@(interleave vars (repeatedly lvar))]
     (map #(reify-var ~v %)
          (take* (bind* (~g empty-pkg) ~@gs)))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
