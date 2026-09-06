(ns lilac.core
  (:require [clojure.core :as core]
            [clojure.set :as set]
            [clojure.walk :as walk]))

(defn ^:private λ-parts
  "Splits a `(vars... . body...)` tail (with the leading λ already stripped) into [vars body]."
  [tail]
  (let [[vars [_ & body]] (split-with (partial not= '.) tail)]
    [vars body]))

(defn ^:private λ1 [args body]
  (if (empty? args)
    body
    `(fn
       ~@(reduce
           #(cons `([~@(take %2 args)] ~(λ1 (drop %2 args) body)) %1)
           (list)
           (range 1 (inc (count args)))))))

(defn ^:private λ-flatten [args [fst & rst :as body]]
  (if (and (not rst) (seq? fst) (= 'λ (first fst)))
    (let [[inner-args inner-body] (λ-parts (next fst))]
      (λ-flatten (concat args inner-args) inner-body))
    [args body]))

(defmacro λ [& lst]
  (let [[args body] (λ-parts lst)
        [λ-args [fst snd :as λ-body]] (λ-flatten args body)]
    `(with-meta ~(λ1 λ-args (if snd (reduce #(list %1 %2) λ-body) fst))
                {:def '~(cons 'λ lst)})))

(defn λ-def [x] (:def (meta (var-get (resolve x)))))

(defn ^:private λ-resolved-def [x] (some-> x resolve var-get meta :def))

(defn λ-def? [x] (and (symbol? x) (some? (λ-resolved-def x))))

(defn ^:private λ-application? [x]
  (and (coll? x)
       (> (count x) 1)
       (or (λ-def? (first x))
           (and (coll? (first x))
                (= 'λ (first (first x)))))))

(defn ^:private walk-some [pred coll]
  (some pred (tree-seq coll? seq coll)))

(defn ^:private λ-concat [vars body] (concat (cons 'λ vars) (cons '. body)))

(defn ^:private term-vars [t]
  (cond
    (symbol? t) #{t}
    (and (coll? t) (= 'λ (first t)))
    (let [[vs bd] (λ-parts (next t))]
      (set/difference (apply set/union (map term-vars bd)) (set vs)))
    (coll? t) (apply set/union (map term-vars t))
    :else #{}))

(defn ^:private β-reduction-substitution [v a body]
  (let [free (term-vars a)
        subst (fn subst [term]
                (cond
                  (not (coll? term)) (if (= term v) a term)
                  (= 'λ (first term))
                  (let [[vs bd] (λ-parts (next term))]
                    (cond
                      (some #{v} vs) term
                      (some free vs)
                      (let [renames (map (fn [w] [w (gensym (str w "__"))]) (filter free vs))
                            vs' (replace (into {} renames) vs)
                            bd' (reduce (fn [b [old new]] (β-reduction-substitution old new b)) bd renames)]
                        (λ-concat vs' (map subst bd')))
                      :else (λ-concat vs (map subst bd))))
                  :else (map subst term)))]
    (map subst body)))

(defn ^:private β-reduction-part [[fst & args]]
  (let [expr (if (symbol? fst) (λ-def fst) fst)
        [vars body] (λ-parts (next expr))]
    (loop [vars* vars args* args body* body]
      (cond
        (empty? vars*) (if (empty? args*)
                          (if (= 1 (count body*)) (first body*) body*)
                          (concat body* args*))
        (empty? args*) (λ-concat vars* body*)
        :else (recur (next vars*) (next args*) (β-reduction-substitution (first vars*) (first args*) body*))))))

(defn ^:private β-reduce-step [form]
  (walk/postwalk #(if (λ-application? %) (β-reduction-part %) %) form))

(defmacro β-reduction [x]
  (loop [form x]
    (if (walk-some λ-application? form)
      (recur (β-reduce-step form))
      form)))

(def id (λ x . x))

(def If (λ l m n . l m n))

(def apply (λ g . g g))

(def compose (λ f g x . f (g x)))

;; Boolean

(def True (λ t f . t))

(def False (λ t f . f))

(def not (λ x . x False True))

(def and (λ b c . b c False))

(def or (λ b c . b True c))

(def xor (λ b c . b (not c) c))

(def imply (λ b c . or (not b) c))

(def ->bool (λ f . f true false))

;; Pair

(def pair (λ f s b . b f s))

(def first (λ p . p True))

(def second (λ p . p False))

;; Numeral

(def zero (λ s z . z))

(def one (λ s z . s z))

(def succ (λ n s z . s (n s z)))

(def plus (λ m n s z . m s (n s z)))

(def mult (λ m n . m (plus n) zero))

(def pow (λ n m . m n))

(def zero? (λ m . m (λ x . False) True))

(def even? (λ n . n not True))

(def odd? (λ n . n not False))

(def pred (λ n f x . n (λ g h . h (g f)) (λ u . x) id))

(def minus (λ n m . m pred n))

(def ->int (λ m . m (λ x . inc x) 0))

(def leq (λ n m . zero? (minus n m)))

(def less (λ n m . not (leq m n)))

(def eq (λ n m . and (zero? (minus n m)) (zero? (minus m n))))

(def geq (λ n m . leq m n))

;; Integer (a pair of naturals a b represents the integer a - b)

(def int-zero (pair zero zero))

(def int-neg (λ z . pair (second z) (first z)))

(def int-plus (λ x y . pair (plus (first x) (first y)) (plus (second x) (second y))))

(def int-minus (λ x y . int-plus x (int-neg y)))

(def int-mult (λ x y . pair (plus (mult (first x) (first y)) (mult (second x) (second y)))
                            (plus (mult (first x) (second y)) (mult (second x) (first y)))))

(def int-zero? (λ z . eq (first z) (second z)))

(def int-pos? (λ z . less (second z) (first z)))

(def int-neg? (λ z . less (first z) (second z)))

(def int-eq (λ x y . eq (plus (first x) (second y)) (plus (second x) (first y))))

(def int-leq (λ x y . leq (plus (first x) (second y)) (plus (second x) (first y))))

(def int-less (λ x y . less (plus (first x) (second y)) (plus (second x) (first y))))

(def int-geq (λ x y . geq (plus (first x) (second y)) (plus (second x) (first y))))

;; Rational (a pair of integers p q, with q kept positive, represents p / q)

(def rat-normalize (λ r . (int-neg? (second r)) (pair (int-neg (first r)) (int-neg (second r))) r))

(def rat (λ p q . rat-normalize (pair p q)))

(def rat-zero (rat int-zero (pair one zero)))

(def rat-one (rat (pair one zero) (pair one zero)))

(def rat-neg (λ r . pair (int-neg (first r)) (second r)))

(def rat-inv (λ r . rat-normalize (pair (second r) (first r))))

(def rat-plus (λ x y . rat (int-plus (int-mult (first x) (second y)) (int-mult (first y) (second x)))
                            (int-mult (second x) (second y))))

(def rat-minus (λ x y . rat-plus x (rat-neg y)))

(def rat-mult (λ x y . rat (int-mult (first x) (first y)) (int-mult (second x) (second y))))

(def rat-div (λ x y . rat-mult x (rat-inv y)))

(def rat-zero? (λ r . int-zero? (first r)))

(def rat-pos? (λ r . int-pos? (first r)))

(def rat-neg? (λ r . int-neg? (first r)))

(def rat-eq (λ x y . int-eq (int-mult (first x) (second y)) (int-mult (first y) (second x))))

(def rat-leq (λ x y . int-leq (int-mult (first x) (second y)) (int-mult (first y) (second x))))

(def rat-less (λ x y . int-less (int-mult (first x) (second y)) (int-mult (first y) (second x))))

(def rat-geq (λ x y . int-geq (int-mult (first x) (second y)) (int-mult (first y) (second x))))

;; Combinator

(def K (λ x y . x))

(def S (λ x y z . x z (y z)))

(def ι (λ x . x S K))

(def ω (λ x . x x))

(def Y (λ g . (λ x . g (x x)) (λ x . g (x x))))

(def FP (λ f . (λ x . f (λ y . x x y)) (λ x . f (λ y . x x y))))

(def Θ ((λ x g . g (x x g)) (λ x g . g (x x g))))

(def I (S K K))

(def B compose)

(def C (λ f x y . f y x))

(def W (λ f x . f x x))

(def T (λ x f . f x))

(def M ω)

(def V pair)

;; List (right fold function)

(def Nil (λ c n . n))

(def cons (λ h t c n . c h (t c n)))

(def head (λ l . l (λ h t . h) False))

(def tail (λ l c n . l (λ h t g . g h (t c)) (λ t . n) (λ h t . t)))

(def Nil? (λ l . l (λ h t . False) True))

(def foldr (λ c n l . l c n))

(def length (λ l . l (λ h acc . succ acc) zero))

(def append (λ l1 l2 . l1 cons l2))

(def map (λ f l . l (λ h acc . cons (f h) acc) Nil))

(def filter (λ p l . l (λ h acc . (p h) (cons h acc) acc) Nil))

(def member? (λ eqp x l . l (λ h acc . or (eqp x h) acc) False))

(def foldl (λ f z l . l (λ h g . (λ acc . g (f acc h))) id z))

(def reverse (λ l . foldl (λ acc h . cons h acc) Nil l))

(def drop (λ n l . n tail l))

(def nth (λ n l . head (drop n l)))

(def take (λ n l . reverse (first (n (λ p . (Nil? (second p)) p (pair (cons (head (second p)) (first p)) (tail (second p)))) (pair Nil l)))))

;; List (pair)

(def Nil* False)

(def cons* pair)

(def head* first)

(def tail* second)

(def Nil*? (λ l . l (λ h t d . False) True))

;; Tree

(def tree (λ d l r . pair d (pair l r)))

(def root (λ t . first t))

(def left (λ t . first (second t)))

(def right (λ t . second (second t)))

;; Convert

(defmulti ->λ #(if (coll? %) :collection (type %)))
(defmethod ->λ Boolean [val] (if val True False))
(defmethod ->λ Long [val] (loop [i val res zero] (if (core/zero? i) res (recur (dec i) (succ res)))))
(defmethod ->λ Integer [val] (->λ (long val)))
(defmethod ->λ :collection [val] (reduce #(cons %2 %1) Nil (core/reverse val)))
(defmethod ->λ String [val] (->λ (core/map #(->λ (long %)) val)))

(defn string->str [l]
  (String. (char-array
             (loop [l l acc []]
               (if (true? (->bool (Nil? l))) acc (recur (tail l) (conj acc (char (->int (head l))))))))))

(defn ->integer [n] (if (>= n 0) (pair (->λ n) zero) (pair zero (->λ (- n)))))

(def integer->long (λ z . (- (->int (first z)) (->int (second z)))))

(defn ->rational
  ([n] (if (ratio? n) (->rational (long (numerator n)) (long (denominator n))) (->rational n 1)))
  ([p q] (rat (->integer p) (->integer q))))

(def rational->ratio (λ r . (/ (integer->long (first r)) (integer->long (second r)))))
