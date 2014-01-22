(ns com.pettomato.kanren.demo
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.cKanren :as c :refer (!= == fresh conde run* run unit)]
   [com.pettomato.kanren.rKanren :as r]
   [com.pettomato.kanren.llist :refer (empty-llist llist llist* llist->seq)]
   [com.pettomato.kanren.goals :refer (emptyo conso membero appendo nonmembero succeed anyo alwayso)]))

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
     [2 (r/== q 'second)]
     [3 (r/== q 'third)]
     [1 (r/== q 'first)]))

(defn recur-e [e]
  (r/fresh [a b]
    (r/conde
      [(r/== e ['x])]
      [(r/== e ['b a]) (recur-e a)]
      [(r/== e ['a b]) (recur-e b)])))

#_(r/run 5 [q] (recur-e q))

(defn recur-r [e]
  (r/fresh [a b]
    (r/condr
      [10 (r/== e ['x])]
      [ 4 (r/== e ['b a]) (recur-r a)]
      [ 2 (r/== e ['a b]) (recur-r b)])))

#_(r/run 5 [q] (recur-r q))

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
