(ns com.pettomato.kanren.cKanren.fd-macros
  (:require
   [com.pettomato.kanren.cKanren.lvar :refer [lvar?]]
   [com.pettomato.kanren.cKanren.pkg :refer [ext-c]]
   [com.pettomato.kanren.cKanren.core :refer [walk]]
   [com.pettomato.kanren.cKanren.constraint-macros :refer [build-oc]]
   [com.pettomato.kanren.cKanren.fd :refer [make-fd get-fd]]))

(defmacro let-fd [[s d] bindings & body]
  `(let [~@(apply concat
                  (for [[u ud] bindings]
                    `[~u  (walk ~u ~s)
                      ~ud (cond
                           (lvar? ~u) (get-fd ~u ~d)
                           :else      (make-fd (list ~u)))]))]
     ~@body))

(defmacro c-op [op bindings & body]
  `(fn [{s# :s, d# :d, c# :c :as pkg#}]
     (let-fd [s# d#] ~bindings
             (let [c'#   (ext-c (build-oc ~op ~@(map first bindings)) c#)
                   pkg'# (assoc pkg# :c c'#)]
               (cond
                (and ~@(map second bindings)) (~@body pkg'#)
                :else                         pkg'#)))))
