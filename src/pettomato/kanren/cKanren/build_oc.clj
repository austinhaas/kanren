(ns com.pettomato.kanren.cKanren.build-oc)

(defmacro build-oc [op & args]
  (let [vars (repeatedly (count args) gensym)]
   `(let [~@(interleave vars args)]
      (list (~op ~@vars) (quote ~op) ~@vars))))
