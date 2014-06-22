(ns com.pettomato.kanren.cKanren.cKanren-test
  (:refer-clojure :exclude [==])
  (:require
   #+clj
   [clojure.test :refer [is deftest]]
   [com.pettomato.kanren.util.llist :refer [empty-llist llist llist* llist->seq]]
   [com.pettomato.kanren.cKanren.cKanren :refer [== != emptyo conso membero appendo succeed anyo alwayso]]
   #+clj
   [com.pettomato.kanren.cKanren.cKanren-macros :refer [fresh conde run* run]]
   #+cljs
   [cemerick.cljs.test])
  #+cljs
  (:require-macros
   [cemerick.cljs.test :refer [is deftest]]
   [com.pettomato.kanren.cKanren.cKanren-macros :refer [fresh conde run* run]]))

(deftest basic-tests
  (is (= (run* [q] (== q 1))
         '(1)))
  (is (= (run* [q] (!= q 1))
         '((_.0 :- (!= ([_.0 1])))))))
