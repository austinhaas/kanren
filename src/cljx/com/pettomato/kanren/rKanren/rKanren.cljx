(ns com.pettomato.kanren.rKanren.rKanren
  (:require
   [com.pettomato.kanren.rKanren.pkg :as pkg]
   [com.pettomato.kanren.rKanren.streams :refer [mzero choice]]
   [com.pettomato.kanren.rKanren.rank :refer [set-rank get-rank]]
   #+clj
   [com.pettomato.kanren.rKanren.case-inf :refer [case-inf]]
   #+clj
   [com.pettomato.kanren.rKanren.case-inf-plus :refer [case-inf+]]
   #+clj
   [com.pettomato.kanren.rKanren.rdelay :refer [rdelay]])
  #+cljs
  (:require-macros
   [com.pettomato.kanren.rKanren.case-inf :refer [case-inf]]
   [com.pettomato.kanren.rKanren.case-inf-plus :refer [case-inf+]]
   [com.pettomato.kanren.rKanren.rdelay :refer [rdelay]]))

(defn rforce [a-inf]
  (case-inf a-inf
            []    mzero
            [f]   (f)
            [a]   a
            [a f] a-inf))

(defn mplus [a-inf f]
  (case-inf+ a-inf
             []     f
             [f']   (let [f'-rank (get-rank f')
                          f-rank (get-rank f)]
                      (if (< f'-rank f-rank)
                        (rdelay f'-rank (mplus (rforce f') f ))
                        (rdelay f-rank  (mplus (rforce f ) f'))))
             [a]    (choice a f)
             [a f'] (choice a
                            (let [f'-rank (get-rank f')
                                  f-rank (get-rank f)]
                              (if (< f'-rank f-rank)
                                (rdelay f'-rank (mplus (rforce f') f ))
                                (rdelay f-rank  (mplus (rforce f ) f')))))))

(defn bind [a-inf g]
  (case-inf+ a-inf
             []     mzero
             [f]    (rdelay (get-rank f) (bind (rforce f) g))
             [a]    (g a)
             [a f]  (mplus (g a) (rdelay (get-rank f) (bind (rforce f) g)))))

(defn take* [f]
  (case-inf (rforce f)
            []     ()
            [f']   (recur f')
            [a]    (cons a ())
            [a f'] (cons a (lazy-seq (take* f')))))

(def empty-pkg (set-rank pkg/empty-pkg 0))
