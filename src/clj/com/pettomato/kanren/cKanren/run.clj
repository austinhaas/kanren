(ns com.pettomato.kanren.cKanren.run
  (:require
   [com.pettomato.kanren.cKanren.pkg :refer [empty-pkg]]
   [com.pettomato.kanren.cKanren.miniKanren :refer [take*]]
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [fresh]]
   [com.pettomato.kanren.cKanren.cKanren :refer [reify-var]]))

(defmacro run* [[v & vars] & gs]
  `(take*
    (delay
     ((fresh [~v ~@vars]
        ~@gs
        (reify-var ~v))
      empty-pkg))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
