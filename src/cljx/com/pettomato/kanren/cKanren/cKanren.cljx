(ns com.pettomato.kanren.cKanren.cKanren
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.cKanren.lvar :as lvar]
   [com.pettomato.kanren.cKanren.streams :as streams]
   [com.pettomato.kanren.cKanren.pkg :as pkg]
   [com.pettomato.kanren.cKanren.core :as core]
   [com.pettomato.kanren.cKanren.extras :as extras]
   [com.pettomato.kanren.cKanren.goals :as goals]))

(def unit   streams/unit)
(def mzero  streams/mzero)
(def choice streams/choice)
(def unit?  streams/unit?)
(def mzero? streams/mzero?)

(def lvar   lvar/lvar)
(def lvar?  lvar/lvar?)
(def lvar=? lvar/lvar=?)

(def empty-pkg pkg/empty-pkg)

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
(def onceo      goals/onceo)
