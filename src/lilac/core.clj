(ns lilac.core
  (:require [clojure.core :as core]
            [clojure.walk :as walk]))

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
    (let [[inner-args [_ & inner-body]] (split-with (partial not= '.) (next fst))]
      (λ-flatten (concat args inner-args) inner-body))
    [args body]))

(defmacro λ [& lst]
  (let [[args [_ & body]] (split-with (partial not= '.) lst)
        [λ-args [fst snd :as λ-body]] (λ-flatten args body)]
    `(with-meta ~(λ1 λ-args (if snd (reduce #(list %1 %2) λ-body) fst))
                {:def '~(cons 'λ lst)})))

(defn λ-def [x] (:def (meta (var-get (resolve x)))))

(defn λ-def? [x]
  (and (symbol? x)
       (some? (resolve x))
       (some? (var-get (resolve x)))
       (some? (meta (var-get (resolve x))))
       (some? (:def (meta (var-get (resolve x)))))))

(defn ^:private λ-application? [x]
  (and (coll? x)
       (> (count x) 1)
       (or (λ-def? (first x))
           (and (coll? (first x))
                (= 'λ (first (first x)))))))

(defn ^:private walk-some [pred coll]
  (let [count (atom 0)]
    (walk/postwalk #(do (if (pred %) (swap! count inc)) %) coll)
    (> @count 0)))

(defn ^:private λ-concat [vars body] (concat (cons 'λ vars) (cons '. body)))

(defn ^:private β-reduction-substitution [v a body]
  (map #(cond
          (not (coll? %)) (if (= % v) a %)
          (= 'λ (first %)) (let [[vs [_ & bd]] (split-with (partial not= '.) (next %))]
                             (if (some #{v} vs) bd (λ-concat vs (β-reduction-substitution v a bd))))
          :else (β-reduction-substitution v a %))
       body))

(defn ^:private β-reduction-part [[fst & args]]
  (let [expr (if (symbol? fst) (λ-def fst) fst)
        [vars [_ & body]] (split-with (partial not= '.) (next expr))]
    (loop [vars* vars args* args body* body]
      (cond
        (empty? vars*) (if (= 1 (count body*)) (first body*) body*)
        (empty? args*) (λ-concat vars* body*)
        :else (recur (next vars*) (next args*) (β-reduction-substitution (first vars*) (first args*) body*))))))

(defmacro β-reduction [x]
  (loop [form x]
    (if (walk-some λ-application? form)
      (recur (walk/postwalk #(if (λ-application? %) (β-reduction-part %) %) form))
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

(def less (λ n m . leq n (pred m)))

(def eq (λ n m . and (zero? (minus n m)) (zero? (minus m n))))

(def geq (λ n m . leq m n))

;; Combinator

(def K (λ x y . x))

(def S (λ x y z . x z (y z)))

(def ι (λ x . x S K))

(def ω (λ x . x x))

(def Y (λ g . (λ x . g (x x)) (λ x . g (x x))))

(def FP (λ f . (λ x . f (λ y . x x y)) (λ x . f (λ y . x x y))))

;; List (right fold function)

(def Nil (λ c n . n))

(def cons (λ h t c n . c h (t c n)))

(def head (λ l . l (λ h t . h) False))

(def tail (λ l c n . l (λ h t g . g h (t c)) (λ t . n) (λ h t . t)))

(def Nil? (λ l . l (λ h t . False) True))

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
(defmethod ->λ :collection [val] (reduce #(cons %2 %1) Nil (reverse val)))
