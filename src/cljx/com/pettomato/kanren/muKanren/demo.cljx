(ns com.pettomato.kanren.muKanren.demo
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.util.llist :refer (empty-llist llist llist* llist->seq)]
   [com.pettomato.kanren.muKanren
    #+clj
    [muKanren :refer (== fresh conde run* run)]
    #+cljs
    [muKanren :refer (==)]
    [goals :refer (emptyo conso membero appendo succeed anyo alwayso)]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.muKanren.muKanren :refer [fresh conde run* run]]))

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

#_(run 5 [q]
    (membero q (llist (range 3))))

#_(llist->seq
   (first
    (run 1 [q]
      (appendo (llist* 1 2) q (llist* 1 2 3 4 5)))))

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
