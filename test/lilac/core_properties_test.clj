(ns lilac.core-properties-test
  (:require [clojure.test :refer :all]
            [clojure.core :as core]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [lilac.core :refer :all]))

;; Church numerals are unary (succ applied n times), so both building and
;; consuming them costs O(n) stack depth; keep generated magnitudes small so
;; generative testing (which explores growing sizes) doesn't overflow the stack.
(def small-nat (gen/choose 0 25))
(def small-int (gen/choose -25 25))
(def small-nonzero-int (gen/one-of [(gen/choose -25 -1) (gen/choose 1 25)]))
(def small-vec (gen/vector (gen/choose -100 100) 0 20))

(defn ->vec [l]
  (loop [l l acc []]
    (if (true? (->bool (Nil? l))) acc (recur (tail l) (conj acc (head l))))))

;; --- Natural numbers ---

(defspec plus-commutative 100
  (prop/for-all [a small-nat b small-nat]
    (= (->int (plus (->λ a) (->λ b))) (->int (plus (->λ b) (->λ a))))))

(defspec plus-associative 100
  (prop/for-all [a small-nat b small-nat c small-nat]
    (= (->int (plus (plus (->λ a) (->λ b)) (->λ c)))
       (->int (plus (->λ a) (plus (->λ b) (->λ c)))))))

(defspec plus-matches-native 100
  (prop/for-all [a small-nat b small-nat]
    (= (+ a b) (->int (plus (->λ a) (->λ b))))))

(defspec mult-distributes-over-plus 100
  (prop/for-all [a small-nat b small-nat c small-nat]
    (= (->int (mult (->λ a) (plus (->λ b) (->λ c))))
       (->int (plus (mult (->λ a) (->λ b)) (mult (->λ a) (->λ c)))))))

(defspec minus-truncated-matches-native 100
  (prop/for-all [a small-nat b small-nat]
    (= (core/max 0 (- a b)) (->int (minus (->λ a) (->λ b))))))

(defspec pred-succ-inverse 100
  (prop/for-all [a small-nat]
    (= a (->int (pred (succ (->λ a)))))))

(defspec leq-matches-native 100
  (prop/for-all [a small-nat b small-nat]
    (= (<= a b) (->bool (leq (->λ a) (->λ b))))))

(defspec less-matches-native 100
  (prop/for-all [a small-nat b small-nat]
    (= (< a b) (->bool (less (->λ a) (->λ b))))))

(defspec eq-matches-native 100
  (prop/for-all [a small-nat b small-nat]
    (= (core/= a b) (->bool (eq (->λ a) (->λ b))))))

;; --- Integers (pair of naturals) ---

(defspec integer-round-trip 100
  (prop/for-all [n small-int]
    (= n (integer->long (->integer n)))))

(defspec int-plus-commutative 100
  (prop/for-all [a small-int b small-int]
    (= (integer->long (int-plus (->integer a) (->integer b)))
       (integer->long (int-plus (->integer b) (->integer a))))))

(defspec int-plus-matches-native 100
  (prop/for-all [a small-int b small-int]
    (= (+ a b) (integer->long (int-plus (->integer a) (->integer b))))))

(defspec int-neg-involution 100
  (prop/for-all [a small-int]
    (= a (integer->long (int-neg (int-neg (->integer a)))))))

(defspec int-minus-matches-native 100
  (prop/for-all [a small-int b small-int]
    (= (- a b) (integer->long (int-minus (->integer a) (->integer b))))))

(defspec int-mult-matches-native 100
  (prop/for-all [a small-int b small-int]
    (= (* a b) (integer->long (int-mult (->integer a) (->integer b))))))

(defspec int-leq-matches-native 100
  (prop/for-all [a small-int b small-int]
    (= (<= a b) (->bool (int-leq (->integer a) (->integer b))))))

(defspec int-less-matches-native 100
  (prop/for-all [a small-int b small-int]
    (= (< a b) (->bool (int-less (->integer a) (->integer b))))))

(defspec int-eq-matches-native 100
  (prop/for-all [a small-int b small-int]
    (= (core/= a b) (->bool (int-eq (->integer a) (->integer b))))))

;; --- Rationals (pair of integers, denominator kept positive) ---

(defspec rational-round-trip 100
  (prop/for-all [p small-int q small-nonzero-int]
    (= (/ p q) (rational->ratio (->rational p q)))))

(defspec rat-denominator-stays-positive 100
  (prop/for-all [p small-int q small-nonzero-int]
    (false? (->bool (int-neg? (second (->rational p q)))))))

(defspec rat-plus-commutative 100
  (prop/for-all [p1 small-int q1 small-nonzero-int p2 small-int q2 small-nonzero-int]
    (= (rational->ratio (rat-plus (->rational p1 q1) (->rational p2 q2)))
       (rational->ratio (rat-plus (->rational p2 q2) (->rational p1 q1))))))

(defspec rat-plus-matches-native 100
  (prop/for-all [p1 small-int q1 small-nonzero-int p2 small-int q2 small-nonzero-int]
    (= (+ (/ p1 q1) (/ p2 q2))
       (rational->ratio (rat-plus (->rational p1 q1) (->rational p2 q2))))))

(defspec rat-mult-matches-native 100
  (prop/for-all [p1 small-int q1 small-nonzero-int p2 small-int q2 small-nonzero-int]
    (= (* (/ p1 q1) (/ p2 q2))
       (rational->ratio (rat-mult (->rational p1 q1) (->rational p2 q2))))))

(defspec rat-eq-matches-native 100
  (prop/for-all [p1 small-int q1 small-nonzero-int p2 small-int q2 small-nonzero-int]
    (= (core/= (/ p1 q1) (/ p2 q2))
       (->bool (rat-eq (->rational p1 q1) (->rational p2 q2))))))

;; --- Booleans (De Morgan) ---

(defspec de-morgan-and 100
  (prop/for-all [a gen/boolean b gen/boolean]
    (= (->bool (not (and (->λ a) (->λ b))))
       (->bool (or (not (->λ a)) (not (->λ b)))))))

(defspec de-morgan-or 100
  (prop/for-all [a gen/boolean b gen/boolean]
    (= (->bool (not (or (->λ a) (->λ b))))
       (->bool (and (not (->λ a)) (not (->λ b)))))))

;; --- Lists ---

(defspec reverse-involution 100
  (prop/for-all [v small-vec]
    (= v (->vec (reverse (reverse (->λ v)))))))

(defspec append-length-additive 100
  (prop/for-all [v1 small-vec v2 small-vec]
    (= (+ (core/count v1) (core/count v2)) (->int (length (append (->λ v1) (->λ v2)))))))

(defspec append-identity 100
  (prop/for-all [v small-vec]
    (core/and (= v (->vec (append (->λ v) Nil)))
              (= v (->vec (append Nil (->λ v)))))))

(defspec map-matches-native 100
  (prop/for-all [v small-vec]
    (= (core/mapv core/inc v) (->vec (map core/inc (->λ v))))))

(defspec filter-matches-native 100
  (prop/for-all [v small-vec]
    (= (core/filterv core/even? v) (->vec (filter (fn [x] (->λ (core/even? x))) (->λ v))))))

;; --- Strings ---

(defspec string-round-trip 100
  (prop/for-all [s gen/string-ascii]
    (= s (string->str (->λ s)))))
