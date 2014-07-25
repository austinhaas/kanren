(ns com.pettomato.kanren.cKanren.constraint-macros)

(defmacro build-aux-oc [op args zs args2]
  (if (empty? args)
    `(let [~@(interleave zs args)]
       (list (~op ~@zs) (quote ~op) ~@zs))
    `(build-aux-oc ~op ~(rest args) ~(conj zs (first args)) ~args)))

(defmacro build-oc [op & args]
  `(build-aux-oc ~op ~args [] ~args))
