(ns com.pettomato.kanren.muKanren.muKanren
  (:refer-clojure :exclude [== conj disj])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.muKanren.muKanren :refer [case-inf Zzz conj+ disj+ conde fresh run* run]]))

(defn lvar [c] [:lvar c])

(defn lvar? [x] (and (vector? x) (= (first x) :lvar)))

(defn lvar=? [x1 x2] (= (second x1) (second x2)))

(defn walk [u s]
  (let [x (get s u ::not-found)]
    (if (= x ::not-found)
      u
      (recur x s))))

(def empty-s {})

(defn ext-s [x v s] (assoc s x v))

(def mzero false)

(def mzero? false?)

(defn unit [a] a)

(def unit? map?)

(defn choice [a f] [a f])

(defn unify [u v s]
  (let [u (walk u s)
        v (walk v s)]
    (cond
     (and (lvar? u) (lvar? v) (lvar=? u v)) s
     (lvar? u) (ext-s u v s)
     (lvar? v) (ext-s v u s)
     (and (coll? u) (coll? v))
     (let [s (unify (first u) (first v) s)]
       (and s (unify (next u) (next v) s)))
     :else (and (= u v) s))))

(defn == [u v]
  (fn [{:keys [s] :as pkg}]
    (if-let [s' (unify u v s)]
      (unit (assoc pkg :s s'))
      mzero)))

(defmacro case-inf
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (mzero? a#) ~e0
      (delay? a#) (let [~f' a#] ~e1)
      (unit? a#)  (let [~a' a#] ~e2)
      :else       (let [[~a ~f] a#] ~e3))))

(defn mplus [a-inf f]
  (case-inf a-inf
            []     (force f)
            [f']   (delay (mplus f (force f')))
            [a]    (choice a f)
            [a f'] (choice a (delay (mplus f f')))))

(defn bind [a-inf g]
  (case-inf a-inf
            []     mzero
            [f]    (delay (bind (force f) g))
            [a]    (g a)
            [a f]  (mplus (g a) (delay (bind f g)))))

(defn disj [g1 g2] (fn [a] (mplus (g1 a) (g2 a))))

(defn conj [g1 g2] (fn [a] (bind (g1 a) g2)))

;;; Extras

(defmacro Zzz [g]
  `(fn [a#]
     (delay (~g a#))))

(defmacro conj+
  ([g] `(Zzz ~g))
  ([g & gs]
     `(conj (Zzz ~g) (conj+ ~@gs))))

(defmacro disj+
  ([g] `(Zzz ~g))
  ([g & gs]
     `(disj (Zzz ~g) (disj+ ~@gs))))

(defmacro conde
  [& clauses]
  `(disj+
    ~@(for [c clauses]
        `(conj+ ~@c))))

(defn call:fresh [f]
  (fn [{:keys [i] :as pkg}]
    ((f (lvar i)) (update-in pkg [:i] inc))))

(defmacro fresh
  [[& vars] & gs]
  (if (empty? vars)
    `(conj+ ~@gs)
    `(call:fresh (fn [~(first vars)] (fresh ~(rest vars) ~@gs)))))

(defn take* [f]
  (case-inf (force f)
            []    ()
            [f]   (recur f)
            [a]   (cons a ())
            [a f] (cons a (lazy-seq (take* f)))))

(defn reify-name [n]
  (symbol (str "_" "." n)))

(defn reify-s [v s]
  (let [v (walk v s)]
    (cond
     (lvar? v) (let [n (reify-name (count s))]
                 (ext-s v n s))
     (and (coll? v) (not (empty? v))) (reify-s (rest v) (reify-s (first v) s))
     :else s)))

(defn walk* [v s]
  (let [v (walk v s)]
    (cond
     (lvar? v) v
     (seq? v) (map #(walk* % s) v)
     #+clj (instance? clojure.lang.MapEntry v) #+clj (into [] (map #(walk* % s) v))
     #+cljs (satisfies? IMapEntry v) #+cljs (into [] (map #(walk* % s) v))
     (coll? v) (into (empty v) (map #(walk* % s) v))
     :else v)))

(defn reify-var [v s]
  (let [v (walk* v s)]
    (walk* v (reify-s v empty-s))))

(defn reify-state:1st-var [{:keys [s]}]
  (reify-var (lvar 0) s))

(defn mK-reify [a*]
  (map reify-state:1st-var a*))

(def empty-pkg
  {:i 0
   :s empty-s})

(defn call:empty-pkg [g] (g empty-pkg))

(defmacro run* [[& vars] & gs]
  `(mK-reify (take* (call:empty-pkg
                     (fresh [~@vars]
                       ~@gs)))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
