(ns com.pettomato.kanren.rKanren.rKanren
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.cKanren.cKanren :as c]))

(def lvar c/lvar)

(def mzero  c/mzero)
(def unit   c/unit)
(def choice c/choice)

(def mzero?  c/mzero?)
(def unit?   c/unit?)
(def rdelay? fn?)
(defn choice? [x] (and (vector? x) (= (count x) 2)))

(defmacro case-inf
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (mzero? a#)  ~e0
      (rdelay? a#) (let [~f' a#] ~e1)
      (unit? a#)   (let [~a' a#] ~e2)
      (choice? a#) (let [[~a ~f] a#] ~e3)
      :else (throw (Error. (str "Unknown type of a-inf:" a#) )))))

(defn set-rank [a-inf r]
  (assert a-inf)
  (vary-meta a-inf assoc ::rank r))

(defn get-rank [a-inf]
  (case-inf a-inf
            []    -1
            [f]   (get (meta f) ::rank)
            [a]   (get (meta a) ::rank)
            [a f] -1))

(defn add-rank [a r]
  (assert a)
  (vary-meta a update-in [::rank] + r))

(defn inc-rank- [stream-or-subst]
  (vary-meta stream-or-subst update-in [::rank] inc))

(defn inc-rank [a-inf]
  (case-inf a-inf
            []    mzero
            [f]   (inc-rank- f)
            [a]   (inc-rank- a)
            [a f] (choice (inc-rank a) (inc-rank f))))

(defmacro rdelay [r & body]
  `(set-rank
    (fn [] ~@body)
    ~r))

(defn rforce [a-inf]
  (case-inf a-inf
            []    mzero
            [f]   (f)
            [a]   a
            [a f] a-inf))

(defmacro case-inf+
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (mzero? a#)  (inc-rank ~e0)
      (rdelay? a#) (let [~f' a#] (inc-rank ~e1))
      (unit? a#)   (let [~a' a#] (inc-rank ~e2))
      (choice? a#) (let [[~a ~f] a#] (inc-rank ~e3))
      :else (throw (Error. (str "Unknown type of a-inf:" a#) )))))

(defn mplus [a-inf f]
  (case-inf+ a-inf
             []     f
             [f']   (let [f'-rank (get-rank f')
                          f-rank (get-rank f)]
                      (if (< f'-rank f-rank)
                        (rdelay f'-rank (mplus (rforce f') f ))
                        (rdelay f-rank  (mplus (rforce f ) f'))))
             [a]    (choice a f)
             [a f'] (choice a
                            (let [f'-rank (get-rank f')
                                  f-rank (get-rank f)]
                              (if (< f'-rank f-rank)
                                (rdelay f'-rank (mplus (rforce f') f ))
                                (rdelay f-rank  (mplus (rforce f ) f')))))))

(defn bind [a-inf g]
  (case-inf+ a-inf
             []     mzero
             [f]    (rdelay (get-rank f) (bind (rforce f) g))
             [a]    (g a)
             [a f]  (mplus (g a) (rdelay (get-rank f) (bind (rforce f) g)))))

(defn min-rank [as]
  (apply min (map get-rank as)))

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
  (let [n (count vars)
        a (gensym)
        a' (gensym)
        c (gensym)]
    `(fn [~a]
       (rdelay
        (get-rank ~a)
        (let [~c (:i ~a)
              ~@(apply concat (map-indexed (fn [i v] `[~v (lvar (+ ~c ~i))]) vars))
              ~a' (update-in ~a [:i] + ~n)]
          (bind* (~g ~a') ~@gs))))))

(def reify-var c/reify-var)

(defn take* [f]
  (case-inf (rforce f)
            []     ()
            [f']   (recur f')
            [a]    (cons a ())
            [a f'] (cons a (lazy-seq (take* f')))))

(def empty-pkg (set-rank c/empty-pkg 0))

(defmacro run* [[v & vars] g & gs]
  `(let [~v (lvar)
         ~@(interleave vars (repeatedly lvar))]
     (map #(reify-var ~v %)
          (take* (bind* (~g empty-pkg) ~@gs)))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))

(def == c/==)
(def != c/!=)
