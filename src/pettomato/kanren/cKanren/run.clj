(ns pettomato.kanren.cKanren.run
  (:require
   [pettomato.kanren.cKanren.pkg :refer [empty-pkg]]
   [pettomato.kanren.cKanren.miniKanren :refer [take*]]
   [pettomato.kanren.cKanren.miniKanren-operators :refer [fresh]]
   [pettomato.kanren.cKanren.cKanren :refer [reify-var]]))

(defmacro run* [[v & vars] & gs]
  `(take*
    (delay
     ((fresh [~v ~@vars]
        ~@gs
        (reify-var ~v))
      empty-pkg))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
