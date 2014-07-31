(ns com.pettomato.kanren.cKanren.fd-goals
  (:refer-clojure :exclude [== < > <= >= + -])
  (:require
   [com.pettomato.kanren.cKanren.lvar :refer [lvar?]]
   [com.pettomato.kanren.cKanren.pkg :refer [ext-c]]
   [com.pettomato.kanren.cKanren.miniKanren :refer [walk]]
   [com.pettomato.kanren.cKanren.cKanren :refer [compose-M goal-construct]]
   [com.pettomato.kanren.cKanren.fd :refer [process-dom make-dom dom-min dom-max dom-singleton? dom-singleton-element dom-disjoint? dom-diff]]
   #+clj
   [com.pettomato.kanren.cKanren.build-oc :refer [build-oc]]
   #+clj
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [all]]
   #+clj
   [com.pettomato.kanren.cKanren.fd-implementor :refer [let-dom c-op]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.build-oc :refer [build-oc]]
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [all]]
   [com.pettomato.kanren.cKanren.fd-implementor :refer [let-dom c-op]]))

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

#_(defn all-diff-slash-c [y* n*]
  (fn [{:keys [s d c] :as pkg}]
    ))

#_(defn all-diff-c [v*]
  (fn [{:keys [s d c] :as pkg}]
    (let [v (walk v* s)]
      (cond
       (lvar? v*) (let [oc (build-oc all-diff-c v*)]
                    (assoc pkg :c (ext-c oc c)))
       :else      (let [{x* true n* false} (group-by lvar? v*)
                        n* (sort clojure.core/< n*)]
                    (cond
                     (list-sorted? < n*) ((all-diff-slash-c x* n*) pkg)
                     :else               false))))))

(defn dom [x n*]  (goal-construct (domc x n*)))
(defn !=  [u v]   (goal-construct (!=c u v)))
(defn <=  [u v]   (goal-construct (<=c u v)))
(defn <   [u v]   (all (<= u v) (!= u v)))
(defn >=  [u v]   (<= v u))
(defn >   [u v]   (< v u))

(defn +   [u v w] (goal-construct (+c u v w)))
(defn -   [u v w] (+ v w u))

#_(defn all-diff [v*] (goal-construct (all-diff-c v*)))