(ns com.pettomato.kanren.cKanren.tests
  (:refer-clojure :exclude [==])
  (:require
   #+clj
   [clojure.test :refer [is deftest]]
   [com.pettomato.kanren.util.llist :refer [empty-llist llist llist* llist->seq lcons]]
   [com.pettomato.kanren.cKanren.lvar :refer [lvar]]
   [com.pettomato.kanren.cKanren.goals :refer
    [== succeed fail emptyo conso firsto resto appendo anyo alwayso onceo]]
   [com.pettomato.kanren.cKanren.disequality-goals
    :refer [!= membero nonmembero]]
   [com.pettomato.kanren.cKanren.fd-goals :as fd]
   [com.pettomato.kanren.cKanren.pkg :refer [empty-s ext-s]]
   [com.pettomato.kanren.cKanren.core :refer [walk* compose-M]]
   #+clj
   [com.pettomato.kanren.cKanren.run :refer [run* run]]
   #+clj
   [com.pettomato.kanren.cKanren.cKanren-macros :refer [fresh conde all condu]]
   #+cljs
   [cemerick.cljs.test])
  #+cljs
  (:require-macros
   [cemerick.cljs.test :refer [is deftest]]
   [com.pettomato.kanren.cKanren.run :refer [run* run]]
   [com.pettomato.kanren.cKanren.cKanren-macros :refer [fresh conde all condu]]))

(require '[com.pettomato.kanren.cKanren.core
           :refer [process-prefix-impl enforce-constraints-impl reify-constraints-impl]])
(require '[com.pettomato.kanren.cKanren.fd
           :refer [process-prefix-FD enforce-constraints-FD reify-constraints-FD]])
(require '[com.pettomato.kanren.cKanren.disequality
           :refer [process-prefix-NEQ enforce-constraints-NEQ reify-constraints-NEQ]])
(require '[com.pettomato.kanren.cKanren.goals :refer [trace-pkg]])


(reset! process-prefix-impl
        (fn [p c]
          (compose-M (process-prefix-NEQ p c)
                     (process-prefix-FD p c))))
(reset! enforce-constraints-impl
        (fn [x]
          (compose-M (enforce-constraints-NEQ x)
                     (enforce-constraints-FD x))))
(reset! reify-constraints-impl
        (fn [m r]
          (reify-constraints-NEQ m r)
          #_(compose-M
                     (reify-constraints-FD m r))))

;; =============================================================================
;; walk*

(defn to-s [v] (reduce (fn [s [k v]] (ext-s k v s)) empty-s v))

(deftest test-walk*
  (is (= (let [x  (lvar)
               y  (lvar)]
           (walk* `(~x ~y) (to-s [[x 5] [y x]])))
         '(5 5))))

;; =============================================================================
;; run and unify

(deftest test-basic-unify
  (is (= (run* [q]
           (== true q))
         '(true))))

(deftest test-basic-unify-2
  (is (= (run* [q]
           (fresh [x y]
             (== [x y] [1 5])
             (== [x y] q)))
         [[1 5]])))

(deftest test-basic-unify-3
  (is (=  (run* [q]
            (fresh [x y]
              (== [x y] q)))
          '[[_.0 _.1]])))

;; =============================================================================
;; fail

(deftest test-basic-failure
  (is (= (run* [q]
           fail
           (== true q))
         [])))

;; =============================================================================
;; Basic

(deftest test-all
  (is (= (run* [q]
           (all
            (== 1 1)
            (== q true)))
         '(true))))

;; =============================================================================
;; TRS

(defn pairo [p]
  (fresh [a d]
    (== (lcons a d) p)))

(defn twino [p]
  (fresh [x]
    (conso x x p)))

(defn listo [l]
  (conde
    [(emptyo l) succeed]
    [(pairo l)
     (fresh [d]
       (resto l d)
       (listo d))]))

(defn flatteno [s out]
  (conde
    [(emptyo s) (== '() out)]
    [(pairo s)
     (fresh [a d res-a res-d]
       (conso a d s)
       (flatteno a res-a)
       (flatteno d res-d)
       (appendo res-a res-d out))]
    [(conso s '() out)]))

;; =============================================================================
;; conde

(deftest test-basic-conde
  (is (=  (into #{}
            (run* [x]
              (conde
                [(== x 'olive) succeed]
                [succeed succeed]
                [(== x 'oil) succeed])))
          (into #{}
            '[olive _.0 oil]))))

(deftest test-basic-conde-2
  (is (= (into #{}
           (run* [r]
             (fresh [x y]
               (conde
                 [(== 'split x) (== 'pea y)]
                 [(== 'navy x) (== 'bean y)])
               (== (cons x (cons y ())) r))))
         (into #{}
           '[(split pea) (navy bean)]))))

(defn teacupo [x]
  (conde
    [(== 'tea x) succeed]
    [(== 'cup x) succeed]))

(deftest test-basic-conde-e-3
  (is (= (into #{}
           (run* [r]
             (fresh [x y]
               (conde
                 [(teacupo x) (== true y) succeed]
                 [(== false x) (== true y)])
               (== (cons x (cons y ())) r))))
         (into #{} '((false true) (tea true) (cup true))))))

;; =============================================================================
;; conso

(deftest test-conso
  (is (= (run* [q]
           (fresh [a d]
             (conso a d '())))
         ())))

(deftest test-conso-1
  (let [a (lvar)
        d (lvar)]
    (is (= (run* [q]
             (conso a d q))
           ['[_.0 _.1]]))))

(deftest test-conso-2
  (is (= (run* [q]
           (== [q] nil))
         [])))

(deftest test-conso-3
  (is (=
       (run* [q]
         (conso 'a '() q))
       [(llist '(a))])))

(deftest test-conso-4
  (is (= (run* [q]
           (conso 'a (llist '(d)) q))
         [(llist '(a d))])))

(deftest test-conso-empty-list
  (is (= (run* [q]
           (conso 'a q (llist '(a))))
         '[()])))

(deftest test-conso-5
  (is (= (run* [q]
           (conso q (llist '(b c)) (llist '(a b c))))
         '[a])))

;; =============================================================================
;; firsto

(deftest test-firsto
  (is (= (run* [q]
           (firsto '(1 2) q))
         '(1))))

;; =============================================================================
;; resto

(deftest test-resto
  (is (= (run* [q]
           (resto q (llist '(1 2))))
         [(llist '(_.0 1 2))])))

(deftest test-resto-2
  (is (= (run* [q]
           (resto q (llist '[1 2])))
         [(llist '(_.0 1 2))])))

(deftest test-resto-3
  (is (= (run* [q]
           (resto (llist [1 2]) q))
         [(llist '(2))])))

(deftest test-resto-4
  (is (= (run* [q]
           (resto (llist [1 2 3 4 5 6 7 8]) q))
         [(llist '(2 3 4 5 6 7 8))])))

;; =============================================================================
;; flatteno

(deftest test-flatteno
  #_(is (= (into #{}
           (run* [x]
             (flatteno '[[a b] c] x)))
         (into #{}
           '(([[a b] c]) ([a b] (c)) ([a b] c) ([a b] c ())
             (a (b) (c)) (a (b) c) (a (b) c ()) (a b (c))
             (a b () (c)) (a b c) (a b c ()) (a b () c)
             (a b () c ()))))))

;; =============================================================================
;; membero

(deftest membero-1
  ;; This this broken b/c llist is required.
  #_(is (= (run* [q]
           (all
            (== q [(lvar)])
            (membero ['foo (lvar)] q)
            (membero [(lvar) 'bar] q)))
         '([[foo bar]]))))

(deftest membero-2
  (is (= (into #{}
           (run* [q]
             (membero q (llist [1 2 3]))))
         #{1 2 3})))

(deftest membero-3
  (is (= (run* [q]
           (membero q (llist [1 1 1 1 1])))
         '(1))))

;; =============================================================================
;; nonmembero

(deftest nonmembero-1
  (is (= (run* [q]
           (nonmembero 1 (llist [1 2 3])))
         '())))

(deftest nonmembero-2
  (is (= (run* [q]
           (nonmembero 0 (llist [1 2 3])))
         '(_.0))))

;; -----------------------------------------------------------------------------
;; conde clause count

(defn digit-1 [x]
  (conde
    [(== 0 x)]))

(defn digit-4 [x]
  (conde
    [(== 0 x)]
    [(== 1 x)]
    [(== 2 x)]
    [(== 3 x)]))

(deftest test-conde-1-clause
  (is (= (run* [q]
           (fresh [x y]
             (digit-1 x)
             (digit-1 y)
             (== q [x y])))
         '([0 0]))))

(deftest test-conde-4-clauses
  (is (= (into #{}
           (run* [q]
             (fresh [x y]
               (digit-4 x)
               (digit-4 y)
               (== q [x y]))))
         (into #{}
           '([0 0] [0 1] [0 2] [1 0] [0 3] [1 1] [1 2] [2 0]
               [1 3] [2 1] [3 0] [2 2] [3 1] [2 3] [3 2] [3 3])))))

;; -----------------------------------------------------------------------------
;; anyo

(deftest test-anyo-1
  (is (= (run 1 [q]
           (anyo succeed)
           (== true q))
         (list true))))

(deftest test-anyo-2
  (is (= (run 5 [q]
           (anyo succeed)
           (== true q))
         (list true true true true true))))

;; -----------------------------------------------------------------------------
;; divergence

(def f1 (fresh [] f1))

(deftest test-divergence-1
  (is (= (run 1 [q]
           (conde
             [f1]
             [(== false false)]))
         '(_.0))))

(deftest test-divergence-2
  (is (= (run 1 [q]
           (conde
             [f1 (== false false)]
             [(== false false)]))
         '(_.0))))

(def f2
  (fresh []
    (conde
      [f2 (conde
            [f2]
            [(== false false)])]
      [(== false false)])))

(deftest test-divergence-3
  (is (= (run 5 [q] f2)
         '(_.0 _.0 _.0 _.0 _.0))))

;; -----------------------------------------------------------------------------
;; nil in collection

(deftest test-nil-in-coll-1
  (is (= (run* [q]
           (== q [nil]))
         '([nil]))))

(deftest test-nil-in-coll-2
  (is (= (run* [q]
           (== q [1 nil]))
         '([1 nil]))))

(deftest test-nil-in-coll-3
  (is (= (run* [q]
           (== q [nil 1]))
         '([nil 1]))))

(deftest test-nil-in-coll-4
  (is (= (run* [q]
           (== q '(nil)))
         '((nil)))))

(deftest test-nil-in-coll-5
  (is (= (run* [q]
           (== q {:foo nil}))
         '({:foo nil}))))

(deftest test-nil-in-coll-6
  (is (= (run* [q]
           (== q {nil :foo}))
         '({nil :foo}))))

;; -----------------------------------------------------------------------------
;; Occurs Check

#_(deftest test-occurs-check-1
  (is (= (run* [q]
           (== q [q]))
         ())))

;; -----------------------------------------------------------------------------
;; Unifications that should fail

(deftest test-unify-fail-1
  (is (= (run* [p] (fresh [a b] (== b ()) (== '(0 1) (lcons a b)) (== p [a b])))
         ())))

(deftest test-unify-fail-2
  (is (= (run* [p] (fresh [a b] (== b '(1)) (== '(0) (lcons a b)) (== p [a b])))
         ())))

(deftest test-unify-fail-3
  (is (= (run* [p] (fresh [a b c d] (== () b) (== '(1) d) (== (lcons a b) (lcons c d)) (== p [a b c d])))
         ())))

;; -----------------------------------------------------------------------------
;; disequality

(deftest test-disequality-1
  (is (= (run* [q]
           (fresh [x]
             (!= x 1)
             (== q x)))
         '((_.0 :- (!= ([_.0 1])))))))

(deftest test-disequality-2
  (is (= (run* [q]
           (fresh [x]
             (== q x)
             (!= x 1)))
         '((_.0 :- (!= ([_.0 1])))))))

(deftest test-disequality-3
  (is (= (run* [q]
           (fresh [x]
             (!= x 1)
             (== x 1)
             (== q x)))
         ())))

(deftest test-disequality-4
  (is (= (run* [q]
           (fresh [x]
             (== x 1)
             (!= x 1)
             (== q x)))
         ())))

(deftest test-disequality-5
  (is (= (run* [q]
           (fresh [x y]
             (!= x y)
             (== x 1)
             (== y 1)
             (== q x)))
         ())))

(deftest test-disequality-6
  (is (= (run* [q]
           (fresh [x y]
             (== x 1)
             (== y 1)
             (!= x y)
             (== q x)))
         ())))

(deftest test-disequality-7
  (is (= (run* [q]
           (fresh [x y]
             (== x 1)
             (!= x y)
             (== y 2)
             (== q x)))
         '(1))))

(deftest test-disequality-8
  (is (= (run* [q]
           (fresh [x y]
             (!= [x 2] [y 1])
             (== x 1)
             (== y 3)
             (== q [x y])))
         '([1 3]))))

(deftest test-disequality-9
  (is (= (run* [q]
           (fresh [x y]
             (== x 1)
             (== y 3)
             (!= [x 2] [y 1])
             (== q [x y])))
         '([1 3]))))

(deftest test-disequality-10
  (is (= (run* [q]
           (fresh [x y]
             (!= [x 2] [1 y])
             (== x 1)
             (== y 2)
             (== q [x y])))
         ())))

(deftest test-disequality-11
  (is (= (run* [q]
           (fresh [x y]
             (== x 1)
             (== y 2)
             (!= [x 2] [1 y])
             (== q [x y])))
         ())))

(deftest test-disequality-12
  (is (= (run* [q]
           (fresh [x y z]
             (!= x y)
             (== y z)
             (== x z)
             (== q x)))
         ())))

(deftest test-disequality-13
  (is (= (run* [q]
           (fresh [x y z]
             (== y z)
             (== x z)
             (!= x y)
             (== q x)))
         ())))

(deftest test-disequality-14
  (is (= (run* [q]
           (fresh [x y z]
             (== z y)
             (== x z)
             (!= x y)
             (== q x)))
         ())))

(deftest test-disequality-15
  (is (= (run* [q]
           (fresh [x y]
             (== q [x y])
             (!= x 1)
             (!= y 2)))
         '(([_.0 _.1] :- (!= ([_.1 2]) ([_.0 1])))))))

(deftest test-disequality-16
  (is (= (run* [q]
           (fresh [x y z]
             (== y [z])
             (!= [z] x)
             (== z 'foo)
             (== x ['foo])))
         '())))

(deftest test-disequality-17
  ;; This is broken because although we seem to get the correct
  ;; answer, the constraint is not reified to the same structure as
  ;; the test case.
  #_(is (= (run* [q]
           (fresh [x y]
             (!= [1 x] [y 2])
             (== q [x y])))
         '(([_.0 _.1] :- (!= ([_.0 2]) ([_.1 1]))))))
  ;; This works in core.logic, but I don't see why the disequality
  ;; portion of this result is desirable, since it doesn't include the
  ;; query variable.
  #_(is (= (run* [q]
           (fresh [x y]
             (!= [x 1] [2 y])))
         '((_.0 :- (!= ([_.1 1]) ([_.2 2])))))))

(deftest test-logic-95-disequality-1
  (is (= (run* [q]
           (fresh [x y w z]
             (!= x y)
             (!= w z)
             (== z y)
             (== x 'foo)
             (== y 'foo)))
        ())))

(deftest test-logic-95-disequality-2
  (is (= (run* [q]
           (fresh [x y w z]
             (!= x [y])
             (== x ['foo])
             (== y 'foo)))
        ())))

(deftest test-logic-96-disequality-1
  (is (= (run* [q]
           (fresh [x y z]
             (!= x [y])
             (== x [z])
             (== y 'foo)
             (== z 'bar)))
        '(_.0))))

(deftest test-logic-100-disequality-1
  (is (= (run* [q]
           (fresh [a b]
             (== q [a b])
             (!= a q)
             (== a 1)))
        '([1 _.0]))))

(deftest test-logic-100-disequality-2
  (is (= (run* [q]
           (fresh [a b]
             (!= a q)
             (== q [a b])))
        '([_.0 _.1])))
  (is (= (run* [q]
           (fresh [a b]
             (== q [a b])
             (!= a q)))
        '([_.0 _.1]))))

(deftest test-logic-100-disequality-3
  (is (= (run* [q]
           (fresh [x y w z]
             (== x [1 w])
             (== y [2 z])
             (!= x y)))
        '(_.0)))
  (is (= (run* [q]
           (fresh [x y w z]
             (!= x y)
             (== x [1 w])
             (== y [2 z])))
        '(_.0))))

(deftest test-logic-119-disequality-1
  (is (= (run* [q]
           (!= {1 2 3 4} 'foo))
        '(_.0))))

(deftest norvig-test
  ;; http://norvig.com/unify-bug.pdf
  (is (run* [q] (fresh [x y]
                  (== ['p x y] ['p y x])))
      '(_.0))
  (is (run* [x] (fresh [x y z]
                  (== ['q ['p x y] ['p y x]]
                      ['q z z])))
      '(_.0)))

(deftest condu-1 []
  (is (= (run* [q] (onceo (teacupo q)))
         '(tea))))

(deftest condu-2 []
  (is (= (run* [q]
           (== false q)
           (condu
            [(teacupo q) succeed]
            [(== false q) succeed]
            [fail]))
         '(false))))

#_(clojure.test/run-tests)
