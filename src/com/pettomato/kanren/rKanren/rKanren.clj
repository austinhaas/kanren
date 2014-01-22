(ns com.pettomato.kanren.rKanren.rKanren
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.cKanren.cKanren :as c]))

(def lvar c/lvar)

(def unit c/unit)
(def mzero c/mzero)
(def choice c/choice)
(def mzero? c/mzero?)
(def unit? c/unit?)

(defn get-rank [a-inf]
  (get (meta a-inf) ::rank -1))

(defn set-rank [a-inf r]
  (vary-meta a-inf assoc ::rank r))

(defn inc-rank [a-inf]
  (and a-inf
       (let [r (get-rank a-inf)]
         (vary-meta a-inf assoc ::rank (inc r)))))

(defn add-rank [a-inf r]
  (vary-meta a-inf update-in [::rank] + r))

(defmacro rdelay [r & body]
  `(set-rank
    (fn [] ~@body)
    ~r))

(defn rdelay? [d] (fn? d))

(defn rforce [d]
  (assert (rdelay? d) (str d))
  (d))

(defmacro case-inf
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (mzero? a#)  (inc-rank ~e0)
      (rdelay? a#) (let [~f' a#] (inc-rank ~e1))
      (unit? a#)   (let [~a' a#] (inc-rank ~e2))
      :else        (let [[~a ~f] a#] (inc-rank ~e3)))))

(defn mplus [a-inf f]
  (case-inf a-inf
            []     f
            [f']   (let [f'-rank (get-rank f')
                         f-rank (get-rank f)]
                     (if (< f'-rank f-rank)
                       (rdelay f'-rank (mplus (rforce f') f))
                       (rdelay f-rank  (mplus f (rforce f')))))
            [a]    (choice a f)
            [a f'] (choice a
                           (let [f'-rank (get-rank f')
                                 f-rank (get-rank f)]
                             (if (< f'-rank f-rank)
                               (rdelay f'-rank (mplus (rforce f') f))
                               (rdelay f-rank  (mplus f  (rforce f'))))))))

(defn bind [a-inf g]
  (case-inf a-inf
            []     mzero
            [f]    (rdelay (get-rank f) (bind (rforce f) g))
            [a]    (g a)
            [a f]  (mplus (g a) (rdelay (get-rank f) (bind f g)))))

(defmacro mplus*
  ([e] e)
  ([e & es]
     `(let [min-rank# (apply min (map get-rank '~es))
            e-rank# (get-rank ~e)]
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
  (case-inf f
            []    ()
            [f]   (take* (f))
            [a]   (cons a ())
            [a f] (cons a (lazy-seq (take* f)))))

(def empty-pkg (set-rank c/empty-pkg 0))

(defmacro run* [[& vars] & gs]
  `(c/cK-reify (take* ((fresh [~@vars]
                         ~@gs)
                       empty-pkg))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))

(def == c/==)
(def != c/!=)
