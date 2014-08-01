(ns com.pettomato.kanren.cKanren.in-dom
  (:require
   [com.pettomato.kanren.cKanren.fd-goals :refer [dom]]
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [all]]))

(defmacro in-dom [& args]
  (let [e    (last args)
        vars (butlast args)
        n*   (gensym)]
   `(let [~n* ~e]
      (all ~@(for [v vars] `(dom ~v ~n*))))))
