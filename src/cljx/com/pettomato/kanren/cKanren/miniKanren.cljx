(ns com.pettomato.kanren.cKanren.miniKanren
  (:require
   [com.pettomato.kanren.cKanren.lvar
    :refer [lvar? lvar=?]]
   [com.pettomato.kanren.cKanren.pkg
    :refer [empty-s ext-s]]
   #+clj
   [com.pettomato.kanren.cKanren.case-inf :refer [case-inf]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.case-inf :refer [case-inf]]))

(defn take* [f]
  (case-inf (force f)
            []    ()
            [f]   (recur f)
            [a]   (cons a ())
            [a f] (cons a (lazy-seq (take* f)))))

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
