(ns com.pettomato.kanren.cKanren.cKanren
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.cKanren.types :as types]
   [com.pettomato.kanren.cKanren.core :as core]
   [com.pettomato.kanren.cKanren.extras :as extras]
   [com.pettomato.kanren.cKanren.goals :as goals]))

(def unit   types/unit)
(def mzero  types/mzero)
(def choice types/choice)
(def unit?  types/unit?)
(def mzero? types/mzero?)
(def lvar   types/lvar)
(def lvar?  types/lvar?)
(def lvar=? types/lvar=?)

(def empty-pkg core/empty-pkg)
(def reify-var core/reify-var)
(def unify     core/unify)

(def take*   extras/take*)
(def freshen extras/freshen)
(def disj+r  extras/disj+r)

(def ==         goals/==)
(def !=         goals/!=)
(def succeed    goals/succeed)
(def fail       goals/fail)
(def emptyo     goals/emptyo)
(def conso      goals/conso)
(def membero    goals/membero)
(def nonmembero goals/nonmembero)
(def appendo    goals/appendo)
(def anyo       goals/anyo)
(def alwayso    goals/alwayso)
(def trace-lvar goals/trace-lvar)
(def trace-pkg  goals/trace-pkg)
(def trace-s    goals/trace-s)
(def log        goals/log)
