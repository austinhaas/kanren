(ns pettomato.kanren.cKanren.tests
  (:refer-clojure :exclude [==])
  (:require
   [clojure.test :refer [is deftest]]
   [pettomato.kanren.cKanren.cKanren-api :refer [empty-llist llist llist* llist->seq lcons lvar lvar? lvar=? empty-pkg empty-s ext-s unit mzero choice unit? mzero? take* walk* walk reify-var == succeed fail emptyo conso firsto resto appendo anyo alwayso onceo trace-lvar trace-pkg trace-s log != membero nonmembero]]
   [pettomato.kanren.cKanren.fd-goals :as fd]
   #?(:clj [pettomato.kanren.cKanren.run :refer [run* run]])
   #?(:clj [pettomato.kanren.cKanren.miniKanren-operators :refer [fresh conde all condu]])
   #?(:clj [pettomato.kanren.cKanren.in-dom :refer [in-dom]]))
  #?(:cljs
     (:require-macros
      [pettomato.kanren.cKanren.run :refer [run* run]]
      [pettomato.kanren.cKanren.miniKanren-operators :refer [fresh conde all condu]]
      [pettomato.kanren.cKanren.in-dom :refer [in-dom]])))

;; Most of these tests were taken from core.logic:
;; https://github.com/clojure/core.logic/blob/master/src/test/clojure/clojure/core/logic/tests.clj

;; =============================================================================
;; walk*

(defn to-s [v] (reduce (fn [s [k v]] (ext-s s k v)) empty-s v))

(deftest test-basic-walk
  (is (= (let [x  (lvar)
               y  (lvar)
               ss (to-s [[x 5] [y x]])]
           (walk y ss))
         5)))

(deftest test-deep-walk
  (is (= (let [[x y z c b a :as s] (repeatedly lvar)
               ss (to-s [[x 5] [y x] [z y] [c z] [b c] [a b]])]
           (walk a ss))
         5)))

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
         '((_.0 :- (!= {_.0 1}))))))

(deftest test-disequality-2
  (is (= (run* [q]
           (fresh [x]
             (== q x)
             (!= x 1)))
         '((_.0 :- (!= {_.0 1}))))))

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
         '(([_.0 _.1] :- (!= {_.0 1, _.1 2}))))))

(deftest test-disequality-16
  (is (= (run* [q]
           (fresh [x y z]
             (== y [z])
             (!= [z] x)
             (== z 'foo)
             (== x ['foo])))
         '())))

(deftest test-disequality-17
  (is (= (run* [q]
           (fresh [x y]
             (!= [1 x] [y 2])
             (== q [x y])))
         '(([_.0 _.1] :- (!= {_.0 2, _.1 1})))))
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

(deftest test-arch-friends-problem-logic-124
  (let [expected [{:wedges 2,
                    :flats 4,
                    :pumps 1,
                    :sandals 3,
                    :foot-farm 2,
                    :heels-in-a-hand-cart 4,
                    :shoe-palace 1,
                    :tootsies 3}]]
    (is (= expected
           (run* [q]
             (fresh [wedges flats pumps sandals
                     ff hh sp tt pumps+1]
               (in-dom wedges flats pumps sandals
                       ff hh sp tt pumps+1 (range 1 5))
               (fd/distinct [wedges flats pumps sandals])
               (fd/distinct [ff hh sp tt])
               (== flats hh)
               (fd/+ pumps 1 pumps+1)
               (fd/!= pumps+1 tt)
               (== ff 2)
               (fd/+ sp 2 sandals)
               (== q {:wedges wedges
                      :flats flats
                      :pumps pumps
                      :sandals sandals
                      :foot-farm ff
                      :heels-in-a-hand-cart hh
                      :shoe-palace sp
                      :tootsies tt})))))))

;;; cKanren

(deftest test-ckanren-1
  (is (= (into #{}
           (run* [q]
             (fresh [x]
               (in-dom x (range 1 4))
               (== q x))))
         (into #{} '(1 2 3)))))

(deftest test-ckanren-2
  (is (= (into #{}
           (run* [q]
             (fresh [x y z]
               (in-dom x z (range 1 6))
               (in-dom y (range 3 6))
               (fd/+ x y z)
               (== q [x y z]))))
         (into #{} '([1 3 4] [2 3 5] [1 4 5])))))

(deftest test-ckanren-3
  (is (= (into #{}
           (run* [q]
             (fresh [x y]
               (in-dom x y (range 1 4))
               (== x y)
               (== q [x y]))))
         (into #{} '([1 1] [2 2] [3 3])))))

(deftest test-ckanren-4
  (is (true?
       (every? (fn [[x y]] (not= x y))
         (run* [q]
           (fresh [x y]
             (in-dom x y (range 1 10))
             (fd/!= x y)
             (== q [x y])))))))

(deftest test-ckanren-5
  (is (= (into #{}
           (run* [q]
             (fresh [x y]
               (in-dom x y (range 1 4))
               (== x 2)
               (fd/!= x y)
               (== q [x y]))))
         (into #{} '([2 1] [2 3])))))

(deftest test-ckanren-6
  (is (= (run* [q]
           (fresh [x]
             (in-dom x (range 1 3))
             (fd/+ x 1 x)
             (== q x)))
         '())))

(deftest test-ckanren-7
  (is (= (run* [q]
           (fresh [x]
             (in-dom x (range 1 3))
             (fd/+ x x x)))
         '())))

(deftest test-ckanren-8
  (is (= (into #{}
           (run* [q]
             (fresh [x y]
               (in-dom x y (range 1 4))
               (fd/<= x y)
               (== q [x y]))))
         (into #{} '([1 1] [1 2] [2 2] [1 3] [3 3] [2 3])))))

(deftest test-ckanren-9
  (is (= (into #{}
           (run* [q]
             (fresh [x y]
               (in-dom x y (range 1 4))
               (fd/< x y)
               (== q [x y]))))
         (into #{} '([1 2] [2 3] [1 3])))))

(defn subgoal [x]
  (fresh [y]
    (== y x)
    (fd/+ 1 y 3)))

(deftest test-ckanren-10
  (is (= (run* [q]
           (fresh [x]
             (in-dom x (range 1 10))
             (subgoal x)
             (== q x)))
         '(2))))

(deftest test-distinct
  (is (= (into #{}
           (run* [q]
             (fresh [x y z]
               (in-dom x y z (range 1 4))
               (fd/distinct [x y z])
               (== q [x y z]))))
         (into #{} '([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1])))))

(deftest test-=fd-2
  (is (= (into #{}
           (run* [q]
             (fresh [a b]
               (in-dom a b (range 1 4))
               (== a b)
               (== q [a b]))))
         (into #{} '([1 1] [2 2] [3 3])))))

(deftest test-fd-!=-1
  (is (= (into #{}
           (run* [q]
             (fresh [a b]
               (in-dom a b (range 1 4))
               (fd/!= a b)
               (== q [a b]))))
         (into #{} '([1 2] [1 3] [2 1] [2 3] [3 1] [3 2])))))

(deftest test-fd-<-1
  (is (= (into #{}
           (run* [q]
             (fresh [a b c]
               (in-dom a b c (range 1 4))
               (fd/< a b) (fd/< b c)
               (== q [a b c]))))
         (into #{} '([1 2 3])))))

(deftest test-fd-<-2
  (is (= (into #{}
           (run* [q]
             (fresh [x y z]
               (in-dom x y z (range 1 11))
               (fd/+ x y z)
               (fd/< x y)
               (== z 10)
               (== q [x y z]))))
         (into #{} '([1 9 10] [2 8 10] [3 7 10] [4 6 10])))))

(deftest test-fd->-1
  (is (= (into #{}
           (run* [q]
             (fresh [x y z]
               (in-dom x y z (range 1 11))
               (fd/+ x y z)
               (fd/> x y)
               (== z 10)
               (== q [x y z]))))
         (into #{} '([6 4 10] [7 3 10] [8 2 10] [9 1 10])))))

(deftest test-logic-62-fd
  (is (= (run 1 [q]
           (fresh [x y a b]
             (fd/distinct [x y])
             (== [x y] [a b])
             (== q [a b])
             (== a 1)
             (== b 1)))
         ()))
  (is (= (run 1 [q]
           (fresh [x y a b]
             (== [x y] [a b])
             (== q [a b])
             (== a 1)
             (== b 1)
             (fd/distinct [x y])))
         ())))

(deftest test-logic-81-fd
  (is (= (run* [q]
           (fresh [x y]
             (== q x)
             (fd/distinct [q y])
             (== y x)
             (in-dom q x y (range 1 4))))
        ()))
  (is (= (run* [q]
           (fresh [x y z]
             (== q x)
             (== y z)
             (fd/distinct [q y])
             (fd/distinct [q x])
             (== z q)
             (in-dom q x y z (range 1 3))))
        ())))


(defn righto [x y l]
  (conde
    [(fresh [d]
       (conso x d l)
       (firsto d y))]
    [(fresh [d]
       (resto l d)
       (righto x y d))]))

(defn nexto [x y l]
  (conde
    ((righto x y l))
    ((righto y x l))))

(defn membero1 [x l]
  (fresh [head tail]
    (conso head tail l)
    (conde
      [(== x head)]
      [(membero1 x tail)])))

(defn zebrao [hs]
  (all
   (== (llist [(lvar) (lvar) [(lvar) (lvar) 'milk (lvar) (lvar)] (lvar) (lvar)]) hs)
   (firsto hs ['norwegian (lvar) (lvar) (lvar) (lvar)])
   (nexto ['norwegian (lvar) (lvar) (lvar) (lvar)] [(lvar) (lvar) (lvar) (lvar) 'blue] hs)
   (righto [(lvar) (lvar) (lvar) (lvar) 'ivory] [(lvar) (lvar) (lvar) (lvar) 'green] hs)
   (membero1 ['englishman (lvar) (lvar) (lvar) 'red] hs)
   (membero1 [(lvar) 'kools (lvar) (lvar) 'yellow] hs)
   (membero1 ['spaniard (lvar) (lvar) 'dog (lvar)] hs)
   (membero1 [(lvar) (lvar) 'coffee (lvar) 'green] hs)
   (membero1 ['ukrainian (lvar) 'tea (lvar) (lvar)] hs)
   (membero1 [(lvar) 'lucky-strikes 'oj (lvar) (lvar)] hs)
   (membero1 ['japanese 'parliaments (lvar) (lvar) (lvar)] hs)
   (membero1 [(lvar) 'oldgolds (lvar) 'snails (lvar)] hs)
   (nexto [(lvar) (lvar) (lvar) 'horse (lvar)] [(lvar) 'kools (lvar) (lvar) (lvar)] hs)
   (nexto [(lvar) (lvar) (lvar) 'fox (lvar)] [(lvar) 'chesterfields (lvar) (lvar) (lvar)] hs)))

#_(run 1 [q] (zebrao q))

#_(dotimes [_ 100] (time (doall (run 1 [q] (zebrao q)))))

#_(clojure.test/run-tests)
