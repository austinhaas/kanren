(ns com.pettomato.kanren.cKanren.fd
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.cKanren.lvar :refer [lvar?]]
   [com.pettomato.kanren.cKanren.pkg :refer [ext-s ext-d]]
   [com.pettomato.kanren.cKanren.oc :refer [oc->rands]]
   [com.pettomato.kanren.cKanren.miniKanren :refer [walk]]
   [com.pettomato.kanren.cKanren.cKanren :refer [identity-M compose-M run-constraints]]
   [com.pettomato.kanren.cKanren.goals :refer [succeed fail == onceo]]
   #+clj
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [conde all]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [conde all]]))

(defn map-sum [f]
  (fn step [ls]
    (cond
     (empty? ls) fail
     :else       (conde
                   [(f (first ls))]
                   [(step (rest ls))]))))

(def non-negative-integer? (every-pred integer? (complement neg?)))

(def dom-value? non-negative-integer?)

(defn dom-contains? [dom v] (and (dom-value? v) (boolean (some #{v} dom))))

(def dom-singleton? (comp nil? next))

(def dom-singleton-element first)

(def dom-min first)
(def dom-max last)

(defn dom-disjoint? [d1 d2]
  (cond
   (or (empty? d1) (empty? d2)) true
   (= (first d1) (first d2))    false
   (< (first d1) (first d2))    (recur (rest d1) d2)
   :else                        (recur d1 (rest d2))))

(defn dom-diff [d1 d2]
  (loop [d1 d1, d2 d2, acc []]
    (cond
     (or (empty? d1) (empty? d2)) (into acc d1)
     (= (first d1) (first d2))    (recur (rest d1) (rest d2) acc)
     (< (first d1) (first d2))    (recur (rest d1) d2 (conj acc (first d1)))
     :else                        (recur d1 (rest d2) acc))))

(defn dom-intersection [d1 d2]
  (loop [d1 d1, d2 d2, acc []]
    (cond
     (or (empty? d1) (empty? d2)) acc
     (= (first d1) (first d2))    (recur (rest d1) (rest d2) (conj acc (first d1)))
     (< (first d1) (first d2))    (recur (rest d1) d2 acc)
     :else                        (recur d1 (rest d2) acc))))

(defn make-dom [n*] n*)

(defn get-dom [d x] (get d x false))

(defn resolve-storables [dom x]
  (fn [{:keys [s d c] :as pkg}]
    (cond
     (dom-singleton? dom) (let [n    (dom-singleton-element dom)
                                pkg' (update-in pkg [:s] ext-s x n)]
                            ((run-constraints (list x) c) pkg'))
     :else                (update-in pkg [:d] ext-d x dom))))

(defn update-var [x dom]
  (fn [{:keys [s d c] :as pkg}]
    (if-let [x-dom (get-dom d x)]
      (let [i (dom-intersection x-dom dom)]
        (cond
         (empty? i) false
         :else      ((resolve-storables i x) pkg)))
      ((resolve-storables dom x) pkg))))

(defn process-dom [v dom]
  (fn [pkg]
    (cond
     (lvar? v)             ((update-var v dom) pkg)
     (dom-contains? dom v) pkg
     :else                 false)))

(defn force-ans [x]
  (fn [{:keys [s d c] :as pkg}]
    (let [x (walk x s)
          y (and (lvar? x) (get-dom d x))]
      ((cond
        y                      ((map-sum (fn [v] (== x v))) y)
        (and (coll? x)
             (not (empty? x))) (all
                                (force-ans (first x))
                                (force-ans (rest x)))
        :else                  succeed)
       pkg))))

(defn verify-all-bound [c d]
  ;; This doesn't test NEQ constraints because their only operand is a
  ;; map, which fails the lvar? test.
  (assert (->> (mapcat oc->rands c)
               (filter lvar?)
               (every? #(contains? d %)))
          "verify-all-bound: Constrained variable without domain."))

(defn process-delta-FD [p c]
  (if (empty? p)
    identity-M
    (let [[[x v] & ps] (seq p)
          t (compose-M
             (run-constraints (list x) c)
             (process-delta-FD ps c))]
      (fn [{:keys [s d c] :as pkg}]
        (if-let [dom (get-dom d x)]
          ((compose-M (process-dom v dom) t) pkg)
          (t pkg))))))

(defn enforce-constraints-FD [x]
  (all
   (force-ans x)
   (fn [{:keys [s d c] :as pkg}]
     (verify-all-bound c d)
     ((onceo (force-ans (keys d))) pkg))))

(defn reify-constraints-FD [m r]
  (assert false "reify-constraints-FD: Unbound vars at end."))
