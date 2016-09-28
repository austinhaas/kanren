(ns pettomato.kanren.cKanren.fd-implementor
  (:require
   [pettomato.kanren.cKanren.lvar :refer [lvar?]]
   [pettomato.kanren.cKanren.pkg :refer [ext-c]]
   [pettomato.kanren.cKanren.miniKanren :refer [walk]]
   [pettomato.kanren.cKanren.build-oc :refer [build-oc]]
   [pettomato.kanren.cKanren.fd :refer [make-dom get-dom]]))

(defmacro let-dom [[s d] bindings & body]
  `(let [~@(apply concat
                  (for [[u ud] bindings]
                    `[~u  (walk ~u ~s)
                      ~ud (cond
                           (lvar? ~u) (get-dom ~u ~d)
                           :else      (make-dom (list ~u)))]))]
     ~@body))

(defmacro c-op [op bindings & body]
  `(fn [{s# :s, d# :d, c# :c :as pkg#}]
     (let-dom [s# d#] ~bindings
              (let [oc#   (build-oc ~op ~@(map first bindings))
                    pkg'# (update-in pkg# [:c] ext-c oc#)]
                (cond
                 (and ~@(map second bindings)) (~@body pkg'#)
                 :else                         pkg'#)))))
