(ns com.pettomato.kanren.cKanren.run
  (:require
   [com.pettomato.kanren.cKanren.pkg :refer [empty-pkg]]
   [com.pettomato.kanren.cKanren.core :refer [reify-var]]
   [com.pettomato.kanren.cKanren.extras :refer [take*]]
   [com.pettomato.kanren.cKanren.cKanren-macros :refer [fresh]]))

(defmacro run* [[v & vars] & gs]
  `(take*
    (delay
     ((fresh [~v ~@vars]
        ~@gs
        (reify-var ~v))
      empty-pkg))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))
