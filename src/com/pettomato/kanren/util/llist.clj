(ns com.pettomato.kanren.util.llist)

(def empty-llist nil)

(defn lcons [a d]
  (vector a d))

(defn llist [l]
  (if (empty? l)
    empty-llist
    (lcons (first l) (llist (rest l)))))

(defn llist* [& args]
  (llist args))

(defn llist->seq [l]
  (if (empty? l)
    ()
    (cons (first l) (llist->seq (second l)))))
