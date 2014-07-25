(ns com.pettomato.kanren.rKanren.rank
  (:require
   [com.pettomato.kanren.rKanren.streams :refer [mzero choice]]
   #+clj
   [com.pettomato.kanren.rKanren.case-inf :refer [case-inf]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.rKanren.case-inf :refer [case-inf]]))

(defn set-rank [a-inf r]
  (assert a-inf)
  (vary-meta a-inf assoc ::rank r))

(defn get-rank [a-inf]
  (case-inf a-inf
            []    -1
            [f]   (get (meta f) ::rank)
            [a]   (get (meta a) ::rank)
            [a f] -1))

(defn add-rank [a r]
  (assert a)
  (vary-meta a update-in [::rank] + r))

(defn inc-rank- [stream-or-subst]
  (vary-meta stream-or-subst update-in [::rank] inc))

(defn inc-rank [a-inf]
  (case-inf a-inf
            []    mzero
            [f]   (inc-rank- f)
            [a]   (inc-rank- a)
            [a f] (choice (inc-rank a) (inc-rank f))))

(defn min-rank [as]
  (apply min (map get-rank as)))
