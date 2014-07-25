(ns com.pettomato.kanren.cKanren.fd
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.cKanren.lvar :refer [lvar?]]
   [com.pettomato.kanren.cKanren.pkg :refer [ext-s ext-d]]
   [com.pettomato.kanren.cKanren.constraints :refer [oc->rands]]
   [com.pettomato.kanren.cKanren.core :refer [walk identity-M compose-M run-constraints]]
   [com.pettomato.kanren.cKanren.goals :refer [succeed fail == onceo]]
   [com.pettomato.kanren.cKanren.cKanren-macros :refer [conde all]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.cKanren-macros :refer [conde all]]))

(defn map-sum [f]
  (fn step [ls]
    (cond
     (empty? ls) fail
     :else       (conde
                   [(f (first ls))]
                   [(step (rest ls))]))))

(defn fd-value?     [v]    (and (integer? v) (<= 0 v)))
(defn fd-contains?  [fd v] (boolean (and (fd-value? v) (some #{v} fd))))
(defn fd-singleton? [fd]   (nil? (next fd)))

(defn fd-singleton-element [fd] (first fd))

(defn fd-min [fd] (first fd))
(defn fd-max [fd] (last fd))

(defn fd-disjoint? [fd-1 fd-2]
  (cond
   (or (empty? fd-1) (empty? fd-2)) true
   (= (first fd-1) (first fd-2))    false
   (< (first fd-1) (first fd-2))    (recur (rest fd-1) fd-2)
   :else                            (recur fd-1 (rest fd-2))))

(defn fd-diff [fd-1 fd-2]
  (loop [fd-1 fd-1, fd-2 fd-2, acc []]
    (cond
     (or (empty? fd-1) (empty? fd-2)) (into acc fd-1)
     (= (first fd-1) (first fd-2))    (recur (rest fd-1) (rest fd-2) acc)
     (< (first fd-1) (first fd-2))    (recur (rest fd-1) fd-2 (conj acc (first fd-1)))
     :else                            (recur fd-1 (rest fd-2) acc))))

(defn fd-intersection [fd-1 fd-2]
  (loop [fd-1 fd-1, fd-2 fd-2, acc []]
    (cond
     (or (empty? fd-1) (empty? fd-2)) acc
     (= (first fd-1) (first fd-2))    (recur (rest fd-1) (rest fd-2) (conj acc (first fd-1)))
     (< (first fd-1) (first fd-2))    (recur (rest fd-1) fd-2 acc)
     :else                            (recur fd-1 (rest fd-2) acc))))

(defn make-fd [n*] n*)

(defn get-fd [x d] (get d x false))

(defn resolve-storables [fd x]
  (fn [{:keys [s d c] :as pkg}]
    (cond
     (fd-singleton? fd) (let [n    (fd-singleton-element fd)
                              pkg' (assoc pkg :s (ext-s x n s))]
                          ((run-constraints (list x) c) pkg'))
     :else              (assoc pkg :d (ext-d x fd d)))))

(defn update-var [x fd]
  (fn [{:keys [s d c] :as pkg}]
    (if-let [x-fd (get-fd x d)]
      (let [i (fd-intersection x-fd fd)]
        (cond
         (empty? i) false
         :else      ((resolve-storables i x) pkg)))
      ((resolve-storables fd x) pkg))))

(defn process-fd [v fd]
  (fn [pkg]
    (cond
     (lvar? v)           ((update-var v fd) pkg)
     (fd-contains? fd v) pkg
     :else               false)))

(defn force-ans [x]
  (fn [{:keys [s d c] :as pkg}]
    (let [x (walk x s)
          y (and (lvar? x) (get-fd x d))]
      ((cond
        y                      ((map-sum (fn [v] (== x v))) y)
        (and (coll? x)
             (not (empty? x))) (all
                                (force-ans (first x))
                                (force-ans (rest x)))
        :else                  succeed)
       pkg))))

(defn verify-all-bound [c d]
  ;; This doesn't test NEQ constraints because their operands are a
  ;; map, which fails the lvar? test.
  (assert (->> (mapcat oc->rands c)
               (filter lvar?)
               (every? #(contains? d %)))
          "verify-all-bound: Constrained variable without domain."))

(defn process-prefix-FD [p c]
  (if (empty? p)
    identity-M
    (let [[[x v] & ps] (seq p)
          t (compose-M
             (run-constraints (list x) c)
             (process-prefix-FD ps c))]
      (fn [{:keys [s d c] :as pkg}]
        (if-let [fd (get-fd x d)]
          ((compose-M (process-fd v fd) t) pkg)
          (t pkg))))))

(defn enforce-constraints-FD [x]
  (all
   (force-ans x)
   (fn [{:keys [s d c] :as pkg}]
     (verify-all-bound c d)
     ((onceo (force-ans (keys d))) pkg))))

(defn reify-constraints-FD [m r]
  (assert false "reify-constraints-FD: Unbound vars at end."))
