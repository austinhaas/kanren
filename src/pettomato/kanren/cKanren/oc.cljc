(ns pettomato.kanren.cKanren.oc)

(defn oc->proc  [oc] (first oc))
(defn oc->rator [oc] (first (rest oc)))
(defn oc->rands [oc] (rest (rest oc)))
(defn oc->delta [oc] (first (oc->rands oc)))
