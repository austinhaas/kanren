(ns com.pettomato.kanren.cKanren.core
  (:require
   [com.pettomato.kanren.cKanren.streams
    :refer [mzero unit choice empty-f]]
   [com.pettomato.kanren.cKanren.lvar
    :refer [lvar? lvar=? any-relevant-lvar?]]
   [com.pettomato.kanren.cKanren.pkg
    :refer [empty-s ext-s]]
   [com.pettomato.kanren.cKanren.constraints
    :refer [oc->proc oc->rands]]
   #+clj
   [com.pettomato.kanren.cKanren.cKanren-macros
    :refer [all]])
  #+cljx
   [com.pettomato.kanren.cKanren.cKanren-macros
    :refer [all]])

(def process-prefix-impl      (atom nil))
(def enforce-constraints-impl (atom nil))
(def reify-constraints-impl   (atom nil))

(defn process-prefix [p c]    (@process-prefix-impl p c))
(defn enforce-constraints [x] (@enforce-constraints-impl x))
(defn reify-constraints [m r] (@reify-constraints-impl m r))

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

(defn unify-prefix
  ([e s] (unify-prefix e s empty-s))
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

(defn subsumes? [p s]
  (if-let [sp (unify-prefix (seq p) s)]
    (let [[s' p'] sp]
      (= s s'))
    false))

(def identity-M identity)

(defn compose-M [f1 f2]
  (fn [a]
    (let [a (f1 a)]
      (and a (f2 a)))))

(defn reify-name [n]
  (symbol (str "_" "." n)))

(defn reify-s [v s]
  (let [v (walk v s)]
    (cond
     (lvar? v)
     (let [n (reify-name (count s))]
       (ext-s v n s))

     (and (coll? v) (not (empty? v)))
     (reify-s (rest v) (reify-s (first v) s))

     :else s)))

(defn reify-var [x]
  (all
   (enforce-constraints x)
   (fn [{:keys [s d c] :as pkg}]
     (choice
      (let [v (walk* x s)
            r (reify-s v empty-s)]
        (cond
         (empty? r) v
         :else      (let [v (walk* v r)]
                      (cond
                       (empty? c) v
                       :else      ((reify-constraints v r) pkg)))))
      empty-f))))

(defn rem-run [oc]
  (fn [{:keys [s d c] :as pkg}]
    (if (some #(= oc %) c)
      (let [c' (remove #(= oc %) c)]
        ((oc->proc oc) (assoc pkg :c c')))
      pkg)))

(defn run-constraints [x* c]
  (cond
   (empty? c)             identity-M

   (any-relevant-lvar?
    (oc->rands (first c))
    x*)                   (compose-M (rem-run (first c))
                                     (run-constraints x* (rest c)))

    :else                  (run-constraints x* (rest c))))

(defn goal-construct [f]
  (fn [pkg]
    (if-let [pkg' (f pkg)]
      (unit pkg')
      mzero)))
