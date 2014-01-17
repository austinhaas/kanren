(ns com.pettomato.kanren.muKanren
  (:refer-clojure :exclude [== conj disj]))

(defn lvar [c] [:lvar c])

(defn lvar? [x] (and (vector? x) (= (first x) :lvar)))

(defn lvar=? [x1 x2] (= (second x1) (second x2)))

(defn walk [u s]
  (if-let [x (and (lvar? u) (some #(when (lvar=? (first %) u) %) s))]
    (recur (second x) s)
    u))

(defn ext-s [x v s] (cons [x v] s))

(def mzero false)

(defn unit [a] a)

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
  (fn [[s c]]
    (if-let [s' (unify u v s)]
      (unit [s' c])
      mzero)))

(defn call:fresh [f]
  (fn [[s c]]
    ((f (lvar c)) [s (inc c)])))

(defmacro case-inf
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (false? a#) ~e0
      (fn? a#) (let [~f' a#] ~e1)
      (not (and (coll? a#)
                (fn? (second a#))))
      (let [~a' a#]
        ~e2)
      :else (let [~a (first a#)
                  ~f (second a#)]
              ~e3))))

(defn mplus [a-inf f]
  (case-inf a-inf
            []     (f)
            [f']   (fn [] (mplus (f) f'))
            [a]    (choice a f)
            [a f'] (choice a (fn [] (mplus (f) f')))))

(defn bind [a-inf g]
  (case-inf a-inf
            []     mzero
            [f]    (fn [] (bind (f) g))
            [a]    (g a)
            [a f]  (mplus (g a) (fn [] (bind (f) g)))))

(defn disj [g1 g2] (fn [a] (mplus (g1 a) (g2 a))))

(defn conj [g1 g2] (fn [a] (bind (g1 a) g2)))

;;; Extras

(defmacro Zzz [g]
  `(fn [a#]
     (fn [] (~g a#))))

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

(defmacro fresh
  [[& vars] & gs]
  (if (empty? vars)
    `(conj+ ~@gs)
    `(call:fresh (fn [~(first vars)] (fresh ~(rest vars) ~@gs)))))

(defn take* [f]
  (case-inf (f)
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
     (and (coll? v) (not (empty? v)))
     (reify-s (rest v) (reify-s (first v) s))
     :else s)))

(defn walk* [v s]
  (let [v (walk v s)]
    (cond
     (lvar? v) v
     (list? v) (map #(walk* % s) v)
     (coll? v) (into (empty v) (map #(walk* % s) v))
     :else v)))

(def empty-s ())

(defn reify-state:1st-var [[s c]]
  (let [v (walk* (lvar 0) s)]
    (walk* v (reify-s v empty-s))))

(defn mK-reify [a*]
  (map reify-state:1st-var a*))

(def empty-state [empty-s 0])

(defn call:empty-state [g] (g empty-state))

(defmacro run* [[& vars] & gs]
  `(mK-reify (take* (call:empty-state
                     (fresh [~@vars]
                       ~@gs)))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
