(ns com.pettomato.kanren.muKanren.muKanren
  (:refer-clojure :exclude [== conj disj]))

(defmacro case-inf
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (mzero? a#) ~e0
      (delay? a#) (let [~f' a#] ~e1)
      (unit? a#)  (let [~a' a#] ~e2)
      :else       (let [[~a ~f] a#] ~e3))))

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

(defmacro fresh
  [[& vars] & gs]
  (if (empty? vars)
    `(conj+ ~@gs)
    `(call:fresh (fn [~(first vars)] (fresh ~(rest vars) ~@gs)))))

(defmacro run* [[& vars] & gs]
  `(mK-reify (take* (call:empty-pkg
                     (fresh [~@vars]
                       ~@gs)))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
