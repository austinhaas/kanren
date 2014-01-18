(ns com.pettomato.kanren.cKanren
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.muKanren :as mu]))

(defn pair? [x] (and (coll? x) (not (empty? x))))

(def unit mu/unit)
(def mzero mu/mzero)

(def lvar mu/lvar)
(def lvar? mu/lvar?)
(def lvar=? mu/lvar=?)

(def ext-s mu/ext-s)

(def empty-s mu/empty-s)
(def empty-d ())
(def empty-c ())

(defn make-a [s d c] [s d c])

(def empty-a (make-a empty-s empty-d empty-c))

(def walk mu/walk)
(def walk* mu/walk*)

(defn occurs-check [x v s]
  (let [v (walk v s)]
    (cond
     (lvar? v) (lvar=? v x)
     (pair? v) (some #(occurs-check x % s) v)
     :else     false)))

(defn unify:prefix
  ([e s] (unify:prefix e s empty-s))
  ([e s p]
     (if (empty? e)
       [s p]
       (let [[[u v] & e] e
             u (walk u s)
             v (walk v s)]
         (cond
          (= u v)         (recur e s p)
          (lvar? u)       (and (not (occurs-check u v s))
                               (recur e (ext-s u v s) (ext-s u v p)))
          (lvar? v)       (and (not (occurs-check v u s))
                               (recur e (ext-s v u s) (ext-s v u p)))
          (and (pair? u)
               (pair? v)) (let [[u1 & us] u
                                [v1 & vs] v]
                            (recur (list* [u1 v1] [us vs] e) s p))
               :else      false)))))

(def identity-M identity)

(defn compose-M [f1 f2]
  (fn [a]
    (let [a (f1 a)]
      (and a (f2 a)))))

(declare process-prefix enforce-constraints reify-constraints)

(defn oc->proc   [oc] (first oc))
(defn oc->rator  [oc] (first (rest oc)))
(defn oc->rands  [oc] (rest (rest oc)))
(defn oc->prefix [oc] (first (oc->rands oc)))

(defn any:lvar? [t]
  (cond
   (lvar? t) true
   (pair? t) (some any:lvar? t)
   :else     false))

(defn any-relevant:lvar? [t x*]
  ;; The 'first' call is a hack because we are assuming that t is a
  ;; sequence with one item that is a map: the substitution "prefix"
  ;; at the time the constraint was added. The original implementation
  ;; assumed everything is based on conses, but we broke that by
  ;; implementing substitutions as maps. If other constraint systems
  ;; are added, this will have to be fixed, because those may require
  ;; examining more than the first item in t.
  (->> t
       first ;; hack
       seq
       (apply concat)
       (filter lvar?)
       (filter x*)
       empty?
       not))

(defn recover:lvars [p]
  (reduce (fn [r [x v]]
            (if (lvar? v)
              (conj r x v)
              (conj r x)))
          #{}
          p))

(defn ==c [u v]
  (fn [[s d c :as a]]
    (if-let [sp (unify:prefix (list [u v]) s)]
      (let [[s' p] sp]
        (if (empty? p)
          a
          (let [a' (make-a s' d c)]
            ((process-prefix p c) a'))))
      false)))

(defmacro build-aux-oc [op args zs args2]
  (if (empty? args)
    `(let [~@(interleave zs args)]
       (list (~op ~@zs) (var ~op) ~@zs))
    `(build-aux-oc ~op ~(rest args) ~(cons (first args) zs) ~args)))

(defmacro build-oc [op & args]
  `(build-aux-oc ~op ~args () ~args))

(defmacro build-oc2 [op & args]
  `(list (~op ~@args) ~op ~@args))

(declare normalize-store)

(defn !=c-NEQ [p]
  (fn [[s d c :as a]]
    (if-let [sp (unify:prefix (seq p) s)]
      (let [[s' p'] sp]
        (if (empty? p')
          false
          ((normalize-store p') a)))
      a)))

(defn !=c [u v]
  (fn [[s d c :as a]]
    (if-let [sp (unify:prefix (list [u v]) s)]
      (let [[s' p] sp]
        ((!=c-NEQ p) a))
      a)))

(defn subsumes? [p s]
  (if-let [sp (unify:prefix (seq p) s)]
    (let [[s' p'] sp]
      (= s s'))
    false))

(defn ext-c [oc c]
  (if (any:lvar? (oc->rands oc))
    (cons oc c)
    c))

(defn normalize-store [p]
  (fn [[s d c :as a]]
    (loop [c c, c' ()]
      (cond
       (empty? c)               (let [c'' (ext-c (build-oc !=c-NEQ p) c')]
                                  (make-a s d c''))

       (= (oc->rator (first c))
          (var !=c-NEQ))        (let [oc (first c)
                                      p' (oc->prefix oc)]
                                  (cond
                                   (subsumes? p' p ) a
                                   (subsumes? p  p') (recur (rest c) c')
                                   :else             (recur (rest c) (cons oc c'))))

       :else                    (recur (rest c) (cons (first c) c'))))))

(def reify-s mu/reify-s)

(defn reify-state:1st-var [[a c]]
  (let [[s d c] a
        x (lvar 0)]
    (if-let [a ((enforce-constraints x) a)]
      (let [v (walk* x s)
            r (reify-s v empty-s)]
        (if (empty? r)
          v
          (let [v' (walk* v r)]
            (if (empty? c)
              v'
              ((reify-constraints v' r) a)))))
      false)))

(defn cK-reify [a*]
  (map reify-state:1st-var a*))

(defn rem:run [oc]
  (fn [[s d c :as a]]
    (if (some #(= oc %) c)
      (let [c' (remove #(= oc %) c)]
        ((oc->proc oc) (make-a s d c')))
      a)))

(defn run-constraints [x* c]
  (cond
   (empty? c)             identity-M

   (any-relevant:lvar?
    (oc->rands (first c))
    x*)                   (compose-M (rem:run (first c))
                                     (run-constraints x* (rest c)))

    :else                  (run-constraints x* (rest c))))

(defn process-prefix-NEQ [p c]
  (run-constraints (recover:lvars p) c))

(defn enforce-constraints-NEQ [x] unit)

(defn reify-constraints-NEQ [m r]
  (fn [[s d c :as a]]
    (let [p* (remove any:lvar?
                     (walk*
                      (map seq (map oc->prefix c))
                      r))]
      (if (empty? p*)
        m
        `(~m :- (~'!= ~@p*))))))

(def process-prefix process-prefix-NEQ)
(def enforce-constraints enforce-constraints-NEQ)
(def reify-constraints reify-constraints-NEQ)

(defn goal-construct [f]
  (fn [[a c]]
    (if-let [a' (f a)]
      (unit [a' c])
      mzero)))

(defn == [u v]
  (goal-construct (==c u v)))

(defn != [u v]
  (goal-construct (!=c u v)))

(defmacro conde
  [& clauses]
  `(mu/conde ~@clauses))

(def take* mu/take*)

(defmacro fresh
  [[& vars] & gs]
  `(mu/fresh [~@vars] ~@gs))

(def empty-state [empty-a 0])

(defn call:empty-state [g] (g empty-state))

(defmacro run* [[& vars] & gs]
  `(cK-reify (take* (call:empty-state
                     (fresh [~@vars]
                       ~@gs)))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
