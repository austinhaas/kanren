(ns com.pettomato.kanren.demo
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.cKanren :as c :refer (!= == fresh conde run* run)]
   [com.pettomato.kanren.rKanren :as r]
   [com.pettomato.kanren.llist :refer (empty-llist llist llist* llist->seq)]
   [com.pettomato.kanren.goals :refer (emptyo conso membero appendo nonmembero)]))

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

#_(r/run* [q]
    (r/condr
     [3 (r/== q 'second)]
     [5 (r/== q 'third)]
     [1 (r/== q 'first)]))
