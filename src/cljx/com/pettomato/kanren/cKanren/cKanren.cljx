(ns com.pettomato.kanren.cKanren.cKanren
  (:require
   [com.pettomato.kanren.cKanren.lvar :refer [any-relevant-lvar?]]
   [com.pettomato.kanren.cKanren.pkg :refer [empty-s]]
   [com.pettomato.kanren.cKanren.streams :refer [mzero unit choice empty-f]]
   [com.pettomato.kanren.cKanren.miniKanren :refer [walk* unify-prefix reify-s]]
   [com.pettomato.kanren.cKanren.constraint-helpers :refer [oc->proc oc->rands]]
   #+clj
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [all]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [all]]))

(def process-prefix-impl      (atom nil))
(def enforce-constraints-impl (atom nil))
(def reify-constraints-impl   (atom nil))

(defn process-prefix [p c]    (@process-prefix-impl p c))
(defn enforce-constraints [x] (@enforce-constraints-impl x))
(defn reify-constraints [m r] (@reify-constraints-impl m r))

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
