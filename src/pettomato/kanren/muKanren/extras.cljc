(ns pettomato.kanren.muKanren.extras
  (:require
   [pettomato.kanren.muKanren.types :refer [lvar lvar?]]
   [pettomato.kanren.muKanren.core :refer [empty-s ext-s walk]]
   #+clj
   [pettomato.kanren.muKanren.core-macros :refer [case-inf]])
  #+cljs
  (:require-macros
   [pettomato.kanren.muKanren.core-macros :refer [case-inf]]))

(defn call:fresh [f]
  (fn [{:keys [i] :as pkg}]
    ((f (lvar i)) (update-in pkg [:i] inc))))

(defn take* [f]
  (case-inf (force f)
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
     (and (coll? v) (not (empty? v))) (reify-s (rest v) (reify-s (first v) s))
     :else s)))

(defn walk* [v s]
  (let [v (walk v s)]
    (cond
     (lvar? v) v
     (seq? v) (map #(walk* % s) v)
     #+clj (instance? clojure.lang.MapEntry v) #+clj (into [] (map #(walk* % s) v))
     #+cljs (satisfies? IMapEntry v) #+cljs (into [] (map #(walk* % s) v))
     (coll? v) (into (empty v) (map #(walk* % s) v))
     :else v)))

(defn reify-var [v s]
  (let [v (walk* v s)]
    (walk* v (reify-s v empty-s))))

(defn reify-state:1st-var [{:keys [s]}]
  (reify-var (lvar 0) s))

(defn mK-reify [a*]
  (map reify-state:1st-var a*))

(def empty-pkg
  {:i 0
   :s empty-s})

(defn call:empty-pkg [g] (g empty-pkg))
