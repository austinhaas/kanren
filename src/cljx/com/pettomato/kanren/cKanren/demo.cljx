(ns com.pettomato.kanren.cKanren.demo
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.util.llist :refer [empty-llist llist llist* llist->seq]]
   [com.pettomato.kanren.cKanren.goals :refer [== emptyo conso appendo succeed anyo alwayso]]
   [com.pettomato.kanren.cKanren.disequality-goals :refer [!= membero nonmembero]]
   #+clj
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [fresh conde]]
   #+clj
   [com.pettomato.kanren.cKanren.run :refer [run* run]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.cKanren.miniKanren-operators :refer [fresh conde]]
   [com.pettomato.kanren.cKanren.run :refer [run* run]]))

#_(take 5
        (run* [q]
          (fresh [a b c]
            (conde
              [(== a 1)
               (== b 2)
               (== c 3)]
              [(== a 10)
               (== b 20)
               (== c 30)]
              [(== a 100)
               (== b 200)])
            (== q [a b c]))))

#_(run 1 [q a b]
    (== q [a b]))

#_(run 1 [q a b]
    (== q (list a b)))

#_(run 1 [q a b]
    (== q #{a b}))

#_(run 1 [q]
    (!= q 1)
    (!= q 2))

#_(run 5 [q]
    (membero q (llist (range 3))))

#_(llist->seq
   (first
    (run 1 [q]
      (appendo (llist* 1 2) q (llist* 1 2 3 4 5)))))

#_(run 5 [q]
    (nonmembero q (llist* 1 2 3))
    (conde
      [(== q 1)]
      [(== q 4)]))

#_(run 10 [q]
    (conde
      [alwayso
       (conde
         [alwayso])]
      [(== q 3)]))

#_(frequencies
   (run 100 [q]
     (conde
       [alwayso (conde
                  [alwayso (== q 1)]
                  [alwayso (== q 2)]
                  [alwayso (== q 3)])]
       [alwayso (conde
                  [alwayso (== q 10)]
                  [alwayso (== q 20)]
                  [alwayso (== q 30)])]
       [alwayso (conde
                  [alwayso (== q 100)]
                  [alwayso (== q 200)]
                  [alwayso (== q 300)])])))
