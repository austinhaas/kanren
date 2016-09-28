(ns pettomato.kanren.cKanren.fd-goals
  (:refer-clojure :exclude [== < > <= >= + - distinct])
  (:require
   [pettomato.kanren.cKanren.lvar :refer [lvar?]]
   [pettomato.kanren.cKanren.pkg :refer [ext-c]]
   [pettomato.kanren.cKanren.miniKanren :refer [walk]]
   [pettomato.kanren.cKanren.cKanren :refer [identity-M compose-M goal-construct]]
   [pettomato.kanren.cKanren.fd :refer [process-dom make-dom dom-min dom-max dom-singleton? dom-singleton-element dom-disjoint? dom-diff get-dom dom-contains?]]
   #?(:clj [pettomato.kanren.cKanren.build-oc :refer [build-oc]])
   #?(:clj [pettomato.kanren.cKanren.miniKanren-operators :refer [all]])
   #?(:clj [pettomato.kanren.cKanren.fd-implementor :refer [let-dom c-op]]))
  #?(:cljs
     (:require-macros
      [pettomato.kanren.cKanren.build-oc :refer [build-oc]]
      [pettomato.kanren.cKanren.miniKanren-operators :refer [all]]
      [pettomato.kanren.cKanren.fd-implementor :refer [let-dom c-op]])))

(defn domc [x n*]
  (fn [{:keys [s] :as pkg}]
    ((process-dom (walk x s) (make-dom n*)) pkg)))

(defn <=c [u v]
  (c-op <=c [[u ud] [v vd]]
        (let [u-min (dom-min ud)
              v-max (dom-max vd)]
          (compose-M
           (process-dom u (take-while #(clojure.core/<= % v-max) ud))
           (process-dom v (drop-while #(clojure.core/<  % u-min) vd))))))

(defn +c [u v w]
  (c-op +c [[u ud] [v vd] [w wd]]
        (let [u-min (dom-min ud), u-max (dom-max ud)
              v-min (dom-min vd), v-max (dom-max vd)
              w-min (dom-min wd), w-max (dom-max wd)]
          (compose-M
           (process-dom w (range (clojure.core/+ u-min v-min) (inc (clojure.core/+ u-max v-max))))
           (compose-M
            (process-dom u (range (clojure.core/- w-min v-max) (inc (clojure.core/- w-max v-min))))
            (process-dom v (range (clojure.core/- w-min u-max) (inc (clojure.core/- w-max u-min)))))))))

(defn !=c [u v]
  (fn [{:keys [s d c] :as pkg}]
    (let-dom [s d] [[u ud] [v vd]]
             (cond
              (or (not ud) (not vd))               (update-in pkg [:c] ext-c (build-oc !=c u v))
              (and (dom-singleton? ud)
                   (dom-singleton? vd)
                   (= (dom-singleton-element ud)
                      (dom-singleton-element vd))) false
              (dom-disjoint? ud vd)                pkg
              :else                                (let [pkg' (update-in pkg [:c] ext-c (build-oc !=c u v))]
                                                     (cond
                                                      (dom-singleton? ud) ((process-dom v (dom-diff vd ud)) pkg')
                                                      (dom-singleton? vd) ((process-dom u (dom-diff ud vd)) pkg')
                                                      :else               pkg'))))))

(defn- exclude-from-dom [d d1 x*]
  (letfn [(step [x*]
            (cond
             (empty? x*) identity-M
             :else       (if-let [d2 (get-dom d (first x*))]
                           (compose-M
                            (process-dom (first x*) (dom-diff d2 d1))
                            (step (rest x*)))
                           (step (rest x*)))))]
    (step x*)))

(defn- list-insert [pred l x]
  (cond
   (empty? l)         (list x)
   (pred x (first l)) (cons x l)
   :else              (cons (first l) (list-insert pred (rest l) x))))

(defn all-diff%c [y* n*]
  (fn [{:keys [s d c] :as pkg}]
    (loop [y* y*, n* n*, x* (list)]
      (cond
       (empty? y*) (let [oc   (build-oc all-diff%c x* n*)
                         pkg' (update-in pkg [:c] ext-c oc)]
                     ((exclude-from-dom d (make-dom n*) x*) pkg'))
       :else       (let [y (walk (first y*) s)]
                     (cond
                      (lvar? y)            (recur (rest y*) n* (conj x* y))
                      (dom-contains? n* y) false
                      :else                (let [n* (list-insert clojure.core/< n* y)]
                                             (recur (rest y*) n* x*))))))))

(defn all-diff-c [v*]
  (fn [{:keys [s d c] :as pkg}]
    (let [v* (walk v* s)]
      (cond
       (lvar? v*) (update-in pkg [:c] ext-c (build-oc all-diff-c v*))
       :else      (let [{x* true n* false} (group-by lvar? v*)
                        n* (sort clojure.core/< n*)]
                    (cond
                     ;; Make sure all vals are different.
                     (or (empty? n*)
                         (apply clojure.core/< n*)) ((all-diff%c x* n*) pkg)
                     :else                          false))))))

(defn dom [x n*]  (goal-construct (domc x n*)))
(defn !=  [u v]   (goal-construct (!=c u v)))
(defn <=  [u v]   (goal-construct (<=c u v)))
(defn <   [u v]   (all (<= u v) (!= u v)))
(defn >=  [u v]   (<= v u))
(defn >   [u v]   (< v u))

(defn +   [u v w] (goal-construct (+c u v w)))
(defn -   [u v w] (+ v w u))

(defn distinct [v*] (goal-construct (all-diff-c v*)))
