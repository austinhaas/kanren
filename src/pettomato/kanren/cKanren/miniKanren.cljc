(ns pettomato.kanren.cKanren.miniKanren
  (:require
   [pettomato.kanren.cKanren.lvar :refer [lvar? lvar=?]]
   [pettomato.kanren.cKanren.pkg :refer [empty-s ext-s]]
   #+clj
   [pettomato.kanren.cKanren.case-inf :refer [case-inf]])
  #+cljs
  (:require-macros
   [pettomato.kanren.cKanren.case-inf :refer [case-inf]]))

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
     (seq?  v) (map #(walk* % s) v)
     (map?  v) (zipmap (walk* (keys v) s)
                       (walk* (vals v) s))
     (coll? v) (into (empty v) (map #(walk* % s) v))
     :else     v)))

(defn occurs-check [s x v]
  (let [v (walk v s)]
    (cond
     (lvar? v) (lvar=? v x)
     (coll? v) (some #(occurs-check s x %) v)
     :else     false)))

(defn unify+delta
  "A unification algorithm that takes a sequence of pairs to be
  unified and a substitution, and attempts to unify each pair in
  sequence. If successful, returns a pair [s p] where s is the new
  substitution and p is a partial substitution that contains only the
  new mappings added by the unification. If each pair cannot be
  unified, false is returned."
  [s e]
  (loop [e e, s s, p empty-s]
    (if (empty? e)
      [s p]
      (let [[[u v] & e] e
            u (walk u s)
            v (walk v s)]
        (cond
         (= u v)         (recur e s p)
         (lvar? u)       (and (not (occurs-check s u v))
                              (recur e (ext-s s u v) (ext-s p u v)))
         (lvar? v)       (and (not (occurs-check s v u))
                              (recur e (ext-s s v u) (ext-s p v u)))
         (and (coll? u)
              (coll? v)) (let [[u1 & us] (seq u)
                               [v1 & vs] (seq v)]
                           (recur (list* [u1 v1] [us vs] e) s p))
              :else           false)))))

(defn unify [s u v]
  (let [u (walk u s)
        v (walk v s)]
    (cond
     (= u v)         s
     (lvar? u)       (and (not (occurs-check s u v))
                          (ext-s s u v))
     (lvar? v)       (and (not (occurs-check s v u))
                          (ext-s s v u))
     (and (coll? u)
          (coll? v)) (let [s (unify s (first u) (first v))]
                       (and s (unify s (rest u) (rest v))))
     :else           false)))

(defn reify-name [n]
  (symbol (str "_" "." n)))

(defn reify-s [s v]
  (let [v (walk v s)]
    (cond
     (lvar? v) (ext-s s v (reify-name (count s)))
     (coll? v) (reduce reify-s s v)
     :else     s)))
