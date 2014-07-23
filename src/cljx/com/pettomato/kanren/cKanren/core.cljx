(ns com.pettomato.kanren.cKanren.core
  (:require
   [com.pettomato.kanren.cKanren.types :refer [lvar? lvar=? any:lvar? unit mzero]]
   #+clj
   [com.pettomato.kanren.cKanren.core-macros :refer [build-oc]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.core-macros :refer [build-oc]]))

(def empty-s {})
(def empty-c ())

(defn ext-s [x v s] (assoc s x v))

(declare oc->rands)

(defn ext-c [oc c]
  (if (any:lvar? (oc->rands oc))
    (cons oc c)
    c))

(def empty-pkg
  {:s empty-s
   :c empty-c})

(defn walk [u s]
  (let [x (get s u ::not-found)]
    (if (= x ::not-found)
      u
      (recur x s))))

(defn walk* [v s]
  (let [v (walk v s)]
    (cond
     (lvar? v) v
     (seq? v)  (map #(walk* % s) v)
     (map? v)  (zipmap (walk* (keys v) s) (walk* (vals v) s))
     (coll? v) (into (empty v) (map #(walk* % s) v))
     :else     v)))

(defn occurs-check [x v s]
  (let [v (walk v s)]
    (cond
     (lvar? v) (lvar=? v x)
     (coll? v) (some #(occurs-check x % s) v)
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
          (and (coll? u)
               (coll? v)) (let [[u1 & us] u
                                [v1 & vs] v]
                            (recur (list* [u1 v1] [us vs] e) s p))
               :else      false)))))

(defn unify [u v s]
  (let [u (walk u s)
        v (walk v s)]
    (cond
     (= u v)         s
     (lvar? u)       (and (not (occurs-check u v s))
                          (ext-s u v s))
     (lvar? v)       (and (not (occurs-check v u s))
                          (ext-s v u s))
     (and (coll? u)
          (coll? v)) (let [s (unify (first u) (first v) s)]
                       (and s (unify (rest u) (rest v) s)))
     :else           false)))

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
  (fn [{:keys [s c] :as pkg}]
    (if-let [sp (unify:prefix (list [u v]) s)]
      (let [[s' p] sp]
        (if (empty? p)
          pkg
          (let [pkg' (assoc pkg :s s')]
            ((process-prefix p c) pkg'))))
      false)))

(declare normalize-store)

(defn !=c-NEQ [p]
  (fn [{:keys [s] :as pkg}]
    (if-let [sp (unify:prefix (seq p) s)]
      (let [[s' p'] sp]
        (if (empty? p')
          false
          ((normalize-store p') pkg)))
      pkg)))

(defn !=c [u v]
  (fn [{:keys [s] :as pkg}]
    (if-let [sp (unify:prefix (list [u v]) s)]
      (let [[s' p] sp]
        ((!=c-NEQ p) pkg))
      pkg)))

(defn subsumes? [p s]
  (if-let [sp (unify:prefix (seq p) s)]
    (let [[s' p'] sp]
      (= s s'))
    false))

(defn normalize-store [p]
  (fn [{:keys [c] :as pkg}]
    (loop [c c, c' ()]
      (cond
       (empty? c)               (let [c'' (ext-c (build-oc !=c-NEQ p) c')]
                                  (assoc pkg :c c''))

       (= (oc->rator (first c))
          (quote !=c-NEQ))        (let [oc (first c)
                                        p' (oc->prefix oc)]
                                    (cond
                                     (subsumes? p' p ) pkg
                                     (subsumes? p  p') (recur (rest c) c')
                                     :else             (recur (rest c) (cons oc c'))))

          :else                    (recur (rest c) (cons (first c) c'))))))

(defn reify-name [n]
  (symbol (str "_" "." n)))

(defn reify-s [v s]
  (let [v (walk v s)]
    (cond
     (lvar? v) (let [n (reify-name (count s))]
                 (ext-s v n s))
     (and (coll? v) (not (empty? v))) (reify-s (rest v) (reify-s (first v) s))
     :else s)))

(defn reify-var [x pkg]
  (if-let [pkg' ((enforce-constraints x) pkg)]
    (let [{:keys [s c]} pkg'
          v (walk* x s)
          r (reify-s v empty-s)]
      (if (empty? r)
        v
        (let [v' (walk* v r)]
          (if (empty? c)
            v'
            ((reify-constraints v' r) pkg')))))
    false))

(defn rem:run [oc]
  (fn [{:keys [s d c] :as pkg}]
    (if (some #(= oc %) c)
      (let [c' (remove #(= oc %) c)]
        ((oc->proc oc) (assoc pkg :c c')))
      pkg)))

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
  (fn [{:keys [c] :as pkg}]
    (let [c  (walk* c r)
          p* (->> (map oc->prefix c)
                  (map seq)
                  (remove any:lvar?))]
      (if (empty? p*)
        m
        `(~m :- (~'!= ~@p*))))))

(def process-prefix process-prefix-NEQ)
(def enforce-constraints enforce-constraints-NEQ)
(def reify-constraints reify-constraints-NEQ)

(defn goal-construct [f]
  (fn [pkg]
    (if-let [pkg' (f pkg)]
      (unit pkg')
      mzero)))
