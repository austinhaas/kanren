(ns pettomato.kanren.muKanren.extras-macros
  (:refer-clojure :exclude [conj disj])
  (:require
   [pettomato.kanren.muKanren.operators :refer [conj disj]]
   [pettomato.kanren.muKanren.extras :refer [call:fresh mK-reify take* call:empty-pkg]]))

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

(defmacro all
  [& gs]
  `(conj+ ~@gs))

(defmacro run* [[& vars] & gs]
  `(mK-reify (take* (call:empty-pkg
                     (fresh [~@vars]
                       ~@gs)))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
