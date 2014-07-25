(ns com.pettomato.kanren.muKanren.tests
  (:refer-clojure :exclude [==])
  (:require
   #+clj
   [clojure.test :refer [is deftest]]
   [com.pettomato.kanren.util.llist :refer [empty-llist llist llist* llist->seq lcons]]
   [com.pettomato.kanren.muKanren.types :refer [lvar]]
   [com.pettomato.kanren.muKanren.core :refer [empty-s ext-s unify walk]]
   [com.pettomato.kanren.muKanren.goals :refer [== succeed fail emptyo conso firsto resto membero appendo anyo alwayso]]
   [com.pettomato.kanren.muKanren.extras :refer [reify-name walk*]]
   #+clj
   [com.pettomato.kanren.muKanren.extras-macros :refer [fresh conde all run* run]]
   #+cljs
   [cemerick.cljs.test])
  #+cljs
  (:require-macros
   [cemerick.cljs.test :refer [is deftest]]
   [com.pettomato.kanren.muKanren.extras-macros :refer [fresh conde all run* run]]))

;; =============================================================================
;; unify

;; -----------------------------------------------------------------------------
;; nil

(deftest unify-nil-object-1
  (is (= (unify nil 1 empty-s) false)))

(deftest unify-nil-lvar-1
  (let [x (lvar 'x)
        os (ext-s x nil empty-s)]
    (is (= (unify nil x empty-s) os))))

(deftest unify-nil-lseq-1
  (let [x (lvar 'x)]
    (is (= (unify nil (lcons 1 x) empty-s) false))))

(deftest unify-nil-map-1
  (let [x (lvar 'x)]
    (is (= (unify nil {} empty-s) false))))

;; -----------------------------------------------------------------------------
;; object

(deftest unify-object-nil-1
  (is (= (unify 1 nil empty-s) false)))

(deftest unify-object-object-1
  (is (= (unify 1 1 empty-s) empty-s)))

(deftest unify-object-object-2
  (is (= (unify :foo :foo empty-s) empty-s)))

(deftest unify-object-object-3
  (is (= (unify 'foo 'foo empty-s) empty-s)))

(deftest unify-object-object-4
  (is (= (unify "foo" "foo" empty-s) empty-s)))

(deftest unify-object-object-5
  (is (= (unify 1 2 empty-s) false)))

(deftest unify-object-object-6
  (is (= (unify 2 1 empty-s) false)))

(deftest unify-object-object-7
  (is (= (unify :foo :bar empty-s) false)))

(deftest unify-object-object-8
  (is (= (unify 'foo 'bar empty-s) false)))

(deftest unify-object-object-9
  (is (= (unify "foo" "bar" empty-s) false)))

(deftest unify-object-lvar-1
  (let [x (lvar 'x)
        os (ext-s x 1 empty-s)]
    (is (= (unify 1 x empty-s) os))))

(deftest unify-object-lcons-1
  (let [x (lvar 'x)]
    (is (= (unify 1 (lcons 1 'x) empty-s) false))))

(deftest unify-object-seq-1
  (is (= (unify 1 '() empty-s) false)))

(deftest unify-object-seq-2
  (is (= (unify 1 '[] empty-s) false)))

(deftest unify-object-map-1
  (is (= (unify 1 {} empty-s) false)))

;; -----------------------------------------------------------------------------
;; lvar

(deftest unify-lvar-object-1
  (let [x (lvar 'x)
        os (ext-s x 1 empty-s)]
    (is (= (unify x 1 empty-s) os))))

(deftest unify-lvar-lvar-1
  (let [x (lvar 'x)
        y (lvar 'y)
        os (ext-s x y empty-s)]
    (is (= (unify x y empty-s) os))))

(deftest unify-lvar-lcons-1
  (let [x (lvar 'x)
        y (lvar 'y)
        l (lcons 1 y)
        os (ext-s x l empty-s)]
    (is (= (unify x l empty-s) os))))

(deftest unify-lvar-seq-1
  (let [x (lvar 'x)
        os (ext-s x [] empty-s)]
    (is (= (unify x [] empty-s) os))))

(deftest unify-lvar-seq-2
  (let [x (lvar 'x)
        os (ext-s x [1 2 3] empty-s)]
    (is (= (unify x [1 2 3] empty-s) os))))

(deftest unify-lvar-seq-3
  (let [x (lvar 'x)
        os (ext-s x '() empty-s)]
    (is (= (unify x '() empty-s) os))))

(deftest unify-lvar-seq-4
  (let [x (lvar 'x)
        os (ext-s x '(1 2 3) empty-s)]
    (is (= (unify x '(1 2 3) empty-s) os))))

(deftest unify-lvar-map-1
  (let [x (lvar 'x)
        os (ext-s x {} empty-s)]
    (is (= (unify x {} empty-s) os))))

(deftest unify-lvar-map-2
  (let [x (lvar 'x)
        os (ext-s x {1 2 3 4} empty-s)]
    (is (= (unify x {1 2 3 4} empty-s) os))))

;; -----------------------------------------------------------------------------
;; lcons

(deftest unify-lcons-object-1
  (let [x (lvar 'x)]
    (is (= (unify (lcons 1 x) 1 empty-s) false))))

(deftest unify-lcons-lvar-1
  (let [x (lvar 'x)
        y (lvar 'y)
        l (lcons 1 y)
        os (ext-s x l empty-s)]
    (is (= (unify l x empty-s) os))))

(deftest unify-lcons-lcons-1
  (let [x (lvar 'x)
        y (lvar 'y)
        lc1 (lcons 1 x)
        lc2 (lcons 1 y)
        os (ext-s x y empty-s)]
    (is (= (unify lc1 lc2 empty-s) os))))

(deftest unify-lcons-lcons-2
  (let [x (lvar 'x)
        y (lvar 'y)
        z (lvar 'z)
        lc1 (lcons 1 (lcons 2 x))
        lc2 (lcons 1 (lcons z y))
        os (->> empty-s
                (ext-s x y)
                (ext-s z 2))]
    (is (= (unify lc1 lc2 empty-s) os))))

(deftest unify-lcons-lcons-3
  (let [x (lvar 'x)
        y (lvar 'y)
        lc1 (lcons 1 (lcons 2 x))
        lc2 (lcons 1 (lcons 2 (lcons 3 y)))
        os (ext-s x (lcons 3 y) empty-s)]
    (is (= (unify lc1 lc2 empty-s) os))))

(deftest unify-lcons-lcons-4
  (let [x (lvar 'x)
        y (lvar 'y)
        lc1 (lcons 1 (lcons 2 x))
        lc2 (lcons 1 (lcons 3 (lcons 4 y)))]
    (is (= (unify lc1 lc2 empty-s) false))))

(deftest unify-lcons-lcons-5
  (let [x (lvar 'x)
        y (lvar 'y)
        lc2 (lcons 1 (lcons 2 x))
        lc1 (lcons 1 (lcons 3 (lcons 4 y)))]
    (is (= (unify lc1 lc2 empty-s) false))))

(deftest unify-lcons-lcons-6
  (let [x (lvar 'x)
        y (lvar 'y)
        lc1 (lcons 1 (lcons 2 x))
        lc2 (lcons 1 (lcons 2 y))
        os (ext-s x y empty-s)]
    (is (= (unify lc1 lc2 empty-s) os))))

(deftest unify-lcons-seq-1
  (let [x (lvar 'x)
        lc1 (lcons 1 (lcons 2 x))
        l1 (llist '(1 2 3 4))
        os (ext-s x (llist '(3 4)) empty-s)]
    (is (= (unify lc1 l1 empty-s) os))))

(deftest unify-lcons-seq-2
  (let [x (lvar 'x)
        y (lvar 'y)
        lc1 (lcons 1 (lcons y (lcons 3 x)))
        l1 (llist '(1 2 3 4))
        os (->> empty-s
                (ext-s x (llist '(4)))
                (ext-s y 2))]
    (is (= (unify lc1 l1 empty-s) os))))

(deftest unify-lcons-seq-3
  (let [x (lvar 'x)
        lc1 (lcons 1 (lcons 2 (lcons 3 x)))
        l1 (llist '(1 2 3))
        os (ext-s x '() empty-s)]
    (is (= (unify lc1 l1 empty-s) os))))

(deftest unify-lcons-seq-4
  (let [x (lvar 'x)
        lc1 (lcons 1 (lcons 3 x))
        l1 '(1 2 3 4)]
    (is (= (unify lc1 l1 empty-s) false))))

(deftest unify-lcons-seq-5
  (let [x (lvar 'x)
        lc1 (lcons 1 (lcons 2 x))
        l1 '(1 3 4 5)]
    (is (= (unify lc1 l1 empty-s) false))))

(deftest unify-lcons-map-1
  (is (= (unify (lcons 1 (lvar 'x)) {} empty-s) false)))

;; -----------------------------------------------------------------------------
;; seq

(deftest unify-seq-object-1
  (is (= (unify '() 1 empty-s) false)))

(deftest unify-seq-object-2
  (is (= (unify [] 1 empty-s) false)))

(deftest unify-seq-lvar-1
  (let [x (lvar 'x)
        os (ext-s x [] empty-s)]
    (is (= (unify [] x empty-s) os))))

(deftest unify-seq-lcons-1
  (let [x (lvar 'x)
        lc1 (lcons 1 (lcons 2 x))
        l1 (llist '(1 2 3 4))
        os (ext-s x (llist '(3 4)) empty-s)]
    (is (= (unify l1 lc1 empty-s) os))))

(deftest unify-seq-seq-1
  (is (= (unify [1 2 3] [1 2 3] empty-s) empty-s)))

(deftest unify-seq-seq-2
  (is (= (unify '(1 2 3) [1 2 3] empty-s) empty-s)))

(deftest unify-seq-seq-3
  (is (= (unify '(1 2 3) '(1 2 3) empty-s) empty-s)))

(deftest unify-seq-seq-4
  (let [x (lvar 'x)
        os (ext-s x 2 empty-s)]
    (is (= (unify `(1 ~x 3) `(1 2 3) empty-s) os))))

(deftest unify-seq-seq-5
  (is (= (unify [1 2] [1 2 3] empty-s) false)))

(deftest unify-seq-seq-6
  (is (= (unify '(1 2) [1 2 3] empty-s) false)))

(deftest unify-seq-seq-7
  (is (= (unify [1 2 3] [3 2 1] empty-s) false)))

(deftest unify-seq-seq-8
  (is (= (unify '() '() empty-s) empty-s)))

(deftest unify-seq-seq-9
  (is (= (unify '() '(1) empty-s) false)))

(deftest unify-seq-seq-10
  (is (= (unify '(1) '() empty-s) false)))

(deftest unify-seq-seq-11
  (is (= (unify [[1 2]] [[1 2]] empty-s) empty-s)))

(deftest unify-seq-seq-12
  (is (= (unify [[1 2]] [[2 1]] empty-s) false)))

(deftest unify-seq-seq-13
  (let [x (lvar 'x)
        os (ext-s x 1 empty-s)]
    (is (= (unify [[x 2]] [[1 2]] empty-s) os))))

(deftest unify-seq-seq-14
  (let [x (lvar 'x)
        os (ext-s x [1 2] empty-s)]
    (is (= (unify [x] [[1 2]] empty-s) os))))

(deftest unify-seq-seq-15
  (let [x (lvar 'x) y (lvar 'y)
        u (lvar 'u) v (lvar 'v)
        os (->> empty-s
                (ext-s x 'b)
                (ext-s y 'a))]
    (is (= (unify ['a x] [y 'b] empty-s) os))))

(deftest unify-seq-map-1
  (is (= (unify [] {} empty-s) false)))

(deftest unify-seq-map-2
  (is (= (unify '() {} empty-s) false)))

;; -----------------------------------------------------------------------------
;; map

(deftest unify-map-object-1
  (is (= (unify {} 1 empty-s) false)))

(deftest unify-map-lvar-1
  (let [x (lvar 'x)
        os (ext-s x {} empty-s)]
    (is (= (unify {} x empty-s) os))))

(deftest unify-map-lcons-1
  (let [x (lvar 'x)]
    (is (= (unify {} (lcons 1 x) empty-s) false))))

(deftest unify-map-seq-1
  (is (= (unify {} '() empty-s) false)))

(deftest unify-map-map-1
  (is (= (unify {} {} empty-s) empty-s)))

(deftest unify-map-map-2
  (is (= (unify {1 2 3 4} {1 2 3 4} empty-s) empty-s)))

(deftest unify-map-map-3
  (is (= (unify {1 2} {1 2 3 4} empty-s) false)))

(deftest unify-map-map-4
  (let [x (lvar 'x)
        m1 {1 2 3 4}
        m2 {1 2 3 x}
        os (ext-s x 4 empty-s)]
    (is (= (unify m1 m2 empty-s) os))))

(deftest unify-map-map-5
  (let [x (lvar 'x)
        m1 {1 2 3 4}
        m2 {1 4 3 x}]
    (is (= (unify m1 m2 empty-s) false))))

;; =============================================================================
;; walk

(defn to-s [v] (reduce (fn [s [k v]] (ext-s k v s)) empty-s v))

(deftest test-basic-walk
  (is (= (let [x  (lvar 'x)
               y  (lvar 'y)
               ss (to-s [[x 5] [y x]])]
           (walk y ss))
         5)))

(deftest test-deep-walk
  (is (= (let [[x y z c b a :as s] (map lvar '[x y z c b a])
               ss (to-s [[x 5] [y x] [z y] [c z] [b c] [a b]])]
           (walk a ss))
         5)))

;; =============================================================================
;; reify

#_(deftest test-reify-name
  (is (= (let [x  (lvar 'x)
               y  (lvar 'y)]
           (reify-name (to-s [[x 5] [y x]])))
         '_2)))

;; =============================================================================
;; walk*

(deftest test-walk*
  (is (= (let [x  (lvar 'x)
               y  (lvar 'y)]
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
  (let [a (lvar 'a)
        d (lvar 'd)]
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
  (is (= (into #{}
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
  (is (= (run* [q]
           (all
            (== q [(lvar 1)])
            (membero ['foo (lvar 2)] q)
            (membero [(lvar 3) 'bar] q)))
         '([[foo bar]]))))

(deftest membero-2
  (is (= (into #{}
           (run* [q]
             (membero q (llist [1 2 3]))))
         #{1 2 3})))

(deftest membero-3
  (is (= (run* [q]
           (membero q (llist [1 1 1 1 1])))
         '(1 1 1 1 1))))

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
