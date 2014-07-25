(ns com.pettomato.kanren.cKanren.fd-goals
  (:refer-clojure :exclude [== < > <= >= + -])
  (:require
   [com.pettomato.kanren.cKanren.pkg :refer [ext-c]]
   [com.pettomato.kanren.cKanren.miniKanren :refer [walk]]
   [com.pettomato.kanren.cKanren.cKanren :refer [compose-M goal-construct]]
   [com.pettomato.kanren.cKanren.fd :refer [process-fd make-fd fd-min fd-max fd-singleton? fd-singleton-element fd-disjoint? fd-diff]]
   #+clj
   [com.pettomato.kanren.cKanren.build-oc :refer [build-oc]]
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [all]]
   [com.pettomato.kanren.cKanren.fd-implementor :refer [let-fd c-op]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.build-oc :refer [build-oc]]
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [all]]
   [com.pettomato.kanren.cKanren.fd-implementor :refer [let-fd c-op]]))

(alias 'core 'clojure.core)

(defn domc [x n*]
  (fn [{:keys [s d c] :as pkg}]
    ((process-fd (walk x s) (make-fd n*)) pkg)))

(defn <=c [u v]
  (c-op <=c [[u ud] [v vd]]
        (let [u-min (fd-min ud)
              v-max (fd-max vd)]
          (compose-M
           (process-fd u (take-while #(core/<= % v-max) ud))
           (process-fd v (drop-while #(core/<  % u-min) vd))))))

(defn +c [u v w]
  (c-op +c [[u ud] [v vd] [w wd]]
        (let [u-min (fd-min ud), u-max (fd-max ud)
              v-min (fd-min vd), v-max (fd-max vd)
              w-min (fd-min wd), w-max (fd-max wd)]
          (compose-M
           (process-fd w (range (core/+ u-min v-min) (inc (core/+ u-max v-max))))
           (compose-M
            (process-fd u (range (core/- w-min v-max) (inc (core/- w-max v-min))))
            (process-fd v (range (core/- w-min u-max) (inc (core/- w-max u-min)))))))))

(defn !=c-FD [u v]
  (fn [{:keys [s d c] :as pkg}]
    (let-fd [s d] [[u ud] [v vd]]
            (cond
             (or (not ud) (not vd))
             (assoc pkg :c (ext-c (build-oc !=c-FD u v) c))

             (and (fd-singleton? ud)
                  (fd-singleton? vd)
                  (= (fd-singleton-element ud)
                     (fd-singleton-element vd)))
             false

             (fd-disjoint? ud vd)
             pkg

             :else
             (let [c'   (ext-c (build-oc !=c-FD u v) c)
                   pkg' (assoc pkg :c c')]
               (cond
                (fd-singleton? ud) ((process-fd v (fd-diff vd ud)) pkg')
                (fd-singleton? vd) ((process-fd u (fd-diff ud vd)) pkg')
                :else              pkg'))))))

(defn dom [x n*]  (goal-construct (domc x n*)))
(defn !=  [u v]   (goal-construct (!=c-FD u v)))
(defn <=  [u v]   (goal-construct (<=c u v)))
(defn <   [u v]   (all (<= u v) (!= u v)))
(defn >=  [u v]   (<= v u))
(defn >   [u v]   (< v u))

(defn +   [u v w] (goal-construct (+c u v w)))
(defn -   [u v w] (+ v w u))
