(ns com.pettomato.kanren.llist)

;; We need a list data structure to use within the search. At the very
;; least, it should have the semantics of Scheme's cons. Externally
;; supplied sequences would need to be converted to the internal
;; representation. Users can reify it to a vector, if desired.

(def empty-llist nil)

(defn llist [l]
  (if (empty? l)
    empty-llist
    (vector (first l) (llist (rest l)))))

(defn llist* [& args]
  (llist args))

(defn llist->seq [l]
  (if (empty? l)
    ()
    (cons (first l) (llist->seq (second l)))))
