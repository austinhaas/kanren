(ns com.pettomato.kanren.rKanren.rKanren
  (:refer-clojure :exclude [= conj disj]))

(defmacro case-inf
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (mzero? a#)  ~e0
      (rdelay? a#) (let [~f' a#] ~e1)
      (unit? a#)   (let [~a' a#] ~e2)
      (choice? a#) (let [[~a ~f] a#] ~e3)
      :else (throw (Error. (str "Unknown type of a-inf:" a#) )))))

(defmacro rdelay [r & body]
  `(set-rank
    (fn [] ~@body)
    ~r))

(defmacro case-inf+
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (mzero? a#)  (inc-rank ~e0)
      (rdelay? a#) (let [~f' a#] (inc-rank ~e1))
      (unit? a#)   (let [~a' a#] (inc-rank ~e2))
      (choice? a#) (let [[~a ~f] a#] (inc-rank ~e3))
      :else (throw (Error. (str "Unknown type of a-inf:" a#) )))))

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
      (let [~@(interleave vars (repeat (list lvar)))]
        (bind* (~g a#) ~@gs)))))

(defmacro run* [[v & vars] g & gs]
  `(let [~v (lvar)
         ~@(interleave vars (repeatedly lvar))]
     (map #(reify-var ~v %)
          (take* (bind* (~g empty-pkg) ~@gs)))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
