(ns com.pettomato.kanren.cKanren
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.muKanren :as mu]))

(defmacro conde
  [& clauses]
  `(mu/conde ~@clauses))

(defmacro fresh
  [[& vars] & gs]
  `(mu/fresh [~@vars] ~@gs))

(defn pair? [x] (and (coll? x) (not (empty? x))))

(declare process-prefix enforce-constraints reify-constraints)

(defn make-a [s d c] [s d c])

(def identity-M identity)

(defn compose-M [f1 f2]
  (fn [a]
    (let [a (f1 a)]
      (and a (f2 a)))))

(defn oc->proc   [oc] (first oc))
(defn oc->rator  [oc] (first (rest oc)))
(defn oc->rands  [oc] (rest (rest oc)))
(defn oc->prefix [oc] (first (oc->rands oc)))

(defn any:lvar? [t]
  (cond
   (mu/lvar? t) true
   (pair? t) (some any:lvar? t)
   :else false))

(defn any-relevant:lvar? [t x*]
  (cond
   (mu/lvar? t) (some (partial = t) x*)
   (pair? t) (some #(any-relevant:lvar? % x*) t)
   :else false))

(defn ext:lvars [x r]
  (if (some (partial = x) r)
    r
    (cons x r)))

(def lhs first)
(def rhs second)

(defn recover:lvars [p]
  (if (empty? p)
    ()
    (let [x (lhs (first p))
          v (rhs (first p))
          r (recover:lvars (rest p))]
      (if (mu/lvar? v)
        (ext:lvars v (ext:lvars x r))
        (ext:lvars x r)))))

(defn unify [e s]
  (if (empty? e)
    s
    (loop [u (first (first e))
           v (second (first e))
           e (rest e)]
      (let [u (mu/walk u s)
            v (mu/walk v s)]
        (cond
         (= u v) (unify e s)
         (mu/lvar? u) (unify e (mu/ext-s u v s))
         (mu/lvar? v) (unify e (mu/ext-s v u s))
         (and (coll? u) (coll? v))
         (recur (first u) (first v) (cons (list (rest u) (rest v)) e))
         :else false)))))

(defn subsumes? [p s]
  (if-let [s' (unify p s)]
    (= s s')
    false))

(defn ext-c [oc c]
  (cond
   (any:lvar? (oc->rands oc)) (cons oc c)
   :else c))

(defn goal-construct [f]
  (fn [[a c]]
    (if-let [a' (f a)]
      (mu/unit [a' c])
      mu/mzero)))

(defn prefix-s [s s']
  (cond
   (empty? s) s'
   :else (loop [s' s'
                acc ()]
           (cond
            (= s' s) (reverse acc)
            :else (recur (rest s') (cons (first s') acc))))))

(defn ==c [u v]
  (fn [a]
    (let [[s d c] a]
      (if-let [s' (unify (list [u v]) s)]
        (if (= s s')
          a
          (let [p (prefix-s s s')
                a' (make-a s' d c)]
            ((process-prefix p c) a')))
        false))))

(defn == [u v]
  (goal-construct (==c u v)))

(defn rem:run [oc]
  (fn [a]
    (let [[s d c] a]
      (if (some (partial = oc) c)
        (let [c' (remove (partial = oc) c)]
          ((oc->proc oc) (make-a s d c')))
        a))))

(defn run-constraints [x* c]
  (cond
   (empty? c) identity-M
   (any-relevant:lvar? (oc->rands (first c)) x*)
   (compose-M
    (rem:run (first c))
    (run-constraints x* (rest c)))
   :else (run-constraints x* (rest c))))

(defn empty-f [] mu/mzero)

(defn reify-state:1st-var [[a c]]
  (let [[s d c] a
        x (mu/lvar 0)]
    (if-let [a ((enforce-constraints x) a)]
      (let [v (mu/walk* x s)
            r (mu/reify-s v mu/empty-s)]
        (if (empty? r)
          v
          (let [v' (mu/walk* v r)]
            (if (empty? c)
              v'
              ((reify-constraints v' r) a)))))
      false)))

(defn cK-reify [a*]
  (map reify-state:1st-var a*))

(defmacro build-aux-oc [op args zs args2]
  (if (empty? args)
    (let [op1 op]
      `(let [~@(interleave zs args)]
         (list (~op ~@zs) ~op1 ~@zs)))
    `(build-aux-oc ~op ~(rest args) ~(cons (first args) zs) ~args)))

(defmacro build-oc [op & args]
  `(build-aux-oc ~op ~args () ~args))

(defmacro build-oc2 [op & args]
  `(list (~op ~@args) ~op ~@args))

(declare normalize-store)

(defn !=c-NEQ [p]
  (fn [a]
    (let [[s d c] a]
      (if-let [s' (unify p s)]
        (let [p' (prefix-s s s')]
          (if (empty? p')
            false
            ((normalize-store p') a)))
        a))))

(defn !=c [u v]
  (fn [a]
    (let [[s d c] a]
      (if-let [s' (unify (list [u v]) s)]
        ((!=c-NEQ (prefix-s s s')) a)
        a))))

(defn != [u v]
  (goal-construct (!=c u v)))

(defn normalize-store [p]
  (fn [a]
    (let [[s d c] a]
      (loop [c c, c' ()]
        (cond
         (empty? c)
         (let [c'' (ext-c (build-oc !=c-NEQ p) c')]
           (make-a s d c''))

         (= (oc->rator (first c)) '!=c-NEQ)
         (let [oc (first c)
               p' (oc->prefix oc)]
           (cond
            (subsumes? p' p) a
            (subsumes? p p') (recur (rest c) c')
            :else (recur (rest c) (cons oc c'))))

         :else (recur (rest c) (cons (first c) c')))))))

(defn process-prefix-NEQ [p c]
  (run-constraints (recover:lvars p) c))

(defn enforce-constraints-NEQ [x] mu/unit)

(defn reify-constraints-NEQ [m r]
  (fn [a]
    (let [[s d c] a
          c' (mu/walk* c r)
          p* (remove any:lvar? (map oc->prefix c'))]
      (if (empty? p*)
        m
        `(~m :- (~'!= ~@p*))))))

(def process-prefix process-prefix-NEQ)
(def enforce-constraints enforce-constraints-NEQ)
(def reify-constraints reify-constraints-NEQ)

(def empty-d ())
(def empty-c ())

(def empty-a (make-a mu/empty-s empty-d empty-c))

(def empty-state [empty-a 0])

(defn call:empty-state [g] (g empty-state))

(defmacro run* [[& vars] & gs]
  `(cK-reify (mu/take* (call:empty-state
                        (mu/fresh [~@vars]
                          ~@gs)))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
