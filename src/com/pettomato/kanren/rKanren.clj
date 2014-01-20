(ns com.pettomato.kanren.rKanren
  (:refer-clojure :exclude [== conj disj])
  (:require
   [com.pettomato.kanren.muKanren :as mu]
   [com.pettomato.kanren.cKanren :as c]))

(def pair? mu/pair?)

(def mzero mu/mzero)
(def choice mu/choice)

(defn get-rank [a-inf]
  (get (meta a-inf) ::rank -1))

(defn set-rank [a-inf r]
  (vary-meta a-inf assoc ::rank r))

(defn inc-rank [a-inf]
  (and a-inf
       (let [r (get-rank a-inf)]
         (vary-meta a-inf assoc ::rank (inc r)))))

(defn add-rank [a-inf r]
  (vary-meta a-inf update-in [::rank] + r))

(defmacro case-inf
  [e _ e0 [f'] e1 [a'] e2 [a f] e3]
  `(let [a# ~e]
     (cond
      (false? a#) (inc-rank ~e0)
      (fn? a#) (let [~f' a#] (inc-rank ~e1))
      (not (and (pair? a#) (fn? (second a#))))
      (let [~a' a#]
        (inc-rank ~e2))
      :else (let [~a (first a#)
                  ~f (second a#)]
              (inc-rank ~e3)))))

(defn mplus [a-inf f]
  (case-inf a-inf
            []     f
            [f']   (let [f'-rank (get-rank f')
                         f-rank  (get-rank f)]
                     (if (< f'-rank f-rank)
                       (set-rank (fn [] (mplus (f') f))
                                 f'-rank)
                       (set-rank (fn [] (mplus (f)  f'))
                                 f-rank)))
            [a]    (choice a f)
            [a f'] (choice a
                           (let [f'-rank (get-rank f')
                                 f-rank  (get-rank f)]
                             (if (< f'-rank f-rank)
                               (set-rank (fn [] (mplus (f') f))
                                         f'-rank)
                               (set-rank (fn [] (mplus (f)  f'))
                                         f-rank))))))

(defn bind [a-inf g]
  (case-inf a-inf
            []    mzero
            [f]   (set-rank (fn [] (bind (f) g))
                            (get-rank f))
            [a]   (g a)
            [a f] (mplus (g a) (set-rank (fn [] (bind (f) g))
                                         (get-rank f)))))

(defn disj [g1 g2] (fn [a] (mplus (g1 a) (g2 a))))

(defn conj [g1 g2] (fn [a] (bind (g1 a) g2)))

(defmacro Zzz [g]
  `(fn [a#]
     (set-rank
      (fn [] (~g a#))
      (get-rank a#))))

(defmacro disj+
  ([g] `(Zzz ~g))
  ([g & gs]
     `(disj (Zzz ~g) (disj+ ~@gs))))

(defmacro conj+
  ([g] `(Zzz ~g))
  ([g & gs]
     `(conj (Zzz ~g) (conj+ ~@gs))))

(defmacro conde
  [& clauses]
  `(disj+
    ~@(for [c clauses]
        `(conj+ ~@c))))

(defmacro condr
  [& clauses]
  `(disj+
    ~@(for [[r & c] clauses]
        `(fn [a#]
           ((conj+ ~@c) (add-rank a# ~r))))))

(defn call:fresh [f]
  (fn [{:keys [i] :as pkg}]
    (let [pkg' (update-in pkg [:i] inc)]
      ((f (c/lvar i)) pkg'))))

(defmacro fresh
  [[& vars] & gs]
  (if (empty? vars)
    `(conj+ ~@gs)
    `(call:fresh (fn [~(first vars)] (fresh ~(rest vars) ~@gs)))))

(def empty-pkg (set-rank c/empty-pkg 0))

(defmacro run* [[& vars] & gs]
  `(c/cK-reify (c/take* ((fresh [~@vars]
                           ~@gs)
                         empty-pkg))))

(defmacro run [n [& vars] & gs]
  `(take ~n (run* ~vars ~@gs)))

(def == c/==)
(def != c/!=)
