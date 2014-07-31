(ns com.pettomato.kanren.cKanren.cKanren-api
  (:refer-clojure :exclude [==])
  (:require
   [com.pettomato.kanren.util.llist :as ll]
   [com.pettomato.kanren.cKanren.lvar :as lvar]
   [com.pettomato.kanren.cKanren.pkg :as pkg]
   [com.pettomato.kanren.cKanren.streams :as streams]
   [com.pettomato.kanren.cKanren.miniKanren :as mK]
   [com.pettomato.kanren.cKanren.goals :as goals]
   [com.pettomato.kanren.cKanren.cKanren :as cK]
   [com.pettomato.kanren.cKanren.fd :as fd]
   [com.pettomato.kanren.cKanren.disequality :as neq]
   [com.pettomato.kanren.cKanren.disequality-goals :as neq-goals]))

(reset! cK/process-delta-impl (fn [p c]
                                (cK/compose-M (neq/process-delta-NEQ p c)
                                              (fd/process-delta-FD p c))))
(reset! cK/enforce-constraints-impl (fn [x]
                                      (cK/compose-M (neq/enforce-constraints-NEQ x)
                                                    (fd/enforce-constraints-FD x))))
(reset! cK/reify-constraints-impl (fn [m r]
                                    (neq/reify-constraints-NEQ m r)))

(def empty-llist ll/empty-llist)
(def llist       ll/llist)
(def llist*      ll/llist*)
(def llist->seq  ll/llist->seq)
(def lcons       ll/lcons)

(def lvar   lvar/lvar)
(def lvar?  lvar/lvar?)
(def lvar=? lvar/lvar=?)

(def empty-pkg pkg/empty-pkg)
(def empty-s   pkg/empty-s)
(def ext-s     pkg/ext-s)

(def unit   streams/unit)
(def mzero  streams/mzero)
(def choice streams/choice)
(def unit?  streams/unit?)
(def mzero? streams/mzero?)

(def take* mK/take*)
(def walk  mK/walk)
(def walk* mK/walk*)

(def reify-var cK/reify-var)

(def ==         goals/==)
(def succeed    goals/succeed)
(def fail       goals/fail)
(def emptyo     goals/emptyo)
(def conso      goals/conso)
(def firsto     goals/firsto)
(def resto      goals/resto)
(def appendo    goals/appendo)
(def anyo       goals/anyo)
(def alwayso    goals/alwayso)
(def onceo      goals/onceo)

(def trace-lvar goals/trace-lvar)
(def trace-pkg  goals/trace-pkg)
(def trace-s    goals/trace-s)
(def log        goals/log)

(def !=         neq-goals/!=)
(def membero    neq-goals/membero)
(def nonmembero neq-goals/nonmembero)
