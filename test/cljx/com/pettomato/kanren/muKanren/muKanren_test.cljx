(ns com.pettomato.kanren.muKanren.muKanren-test
  (:refer-clojure :exclude [==])
  (:require
   #+clj
   [clojure.test :refer [is deftest]]
   [com.pettomato.kanren.util.llist :refer [empty-llist llist llist* llist->seq]]
   [com.pettomato.kanren.muKanren.goals :refer [== emptyo conso membero appendo succeed anyo alwayso]]
   #+clj
   [com.pettomato.kanren.muKanren.extras-macros :refer [fresh conde run* run]]
   #+cljs
   [cemerick.cljs.test])
  #+cljs
  (:require-macros
   [cemerick.cljs.test :refer [is deftest]]
   [com.pettomato.kanren.muKanren.extras-macros :refer [fresh conde run* run]]))

(deftest basic-tests
  (is (= (run* [q] (== q 1))
         '(1))))
