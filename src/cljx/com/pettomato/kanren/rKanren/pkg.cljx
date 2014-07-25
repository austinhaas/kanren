(ns com.pettomato.kanren.rKanren.pkg)

(def empty-s {})
(def empty-d {})
(def empty-c ())

(defn ext-s [x v s] (assoc s x v))

(defn ext-d [x fd d] (assoc d x fd))

(def empty-pkg
  {:s empty-s
   :d empty-d
   :c empty-c})
