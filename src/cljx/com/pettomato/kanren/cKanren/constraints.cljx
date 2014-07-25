(ns com.pettomato.kanren.cKanren.constraints)

(defn oc->proc   [oc] (first oc))
(defn oc->rator  [oc] (first (rest oc)))
(defn oc->rands  [oc] (rest (rest oc)))
(defn oc->prefix [oc] (first (oc->rands oc)))
