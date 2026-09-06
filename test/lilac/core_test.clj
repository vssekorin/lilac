(ns lilac.core-test
  (:require [clojure.test :refer :all]
            [clojure.core :as core]
            [lilac.core :refer :all]))

(defn ->vec [l]
  (loop [l l acc []]
    (if (true? (->bool (Nil? l))) acc (recur (tail l) (conj acc (head l))))))

(def two (succ (succ zero)))
(def three (succ (succ (succ zero))))
(def four (succ (succ (succ (succ zero)))))
(def five (succ (succ (succ (succ (succ zero))))))
(def free-var 42)

(deftest id-test
  (testing
    (is (= 6 (id 6)))
    (is (= "abc" (id "abc")))
    (is (= one (id one)))))

(deftest if-test
  (testing
    (is (= 4 (If True 4 6)))
    (is (= 6 (If False 4 6)))))

(deftest compose-test
  (testing
    (is (= 5 (compose ->int succ four)))))

(deftest bool-test
  (testing
    (is (= true (->bool True)))
    (is (= false (->bool False)))))

(deftest not-test
  (testing
    (is (= False (not True)))
    (is (= True (not False)))))

(deftest and-test
  (testing
    (is (= false (->bool (and False False))))
    (is (= false (->bool (and False True))))
    (is (= false (->bool (and True False))))
    (is (= true (->bool (and True True))))))

(deftest or-test
  (testing
    (is (= false (->bool (or False False))))
    (is (= true (->bool (or False True))))
    (is (= true (->bool (or True False))))
    (is (= true (->bool (or True True))))))

(deftest xor-test
  (testing
    (is (= false (->bool (xor False False))))
    (is (= true (->bool (xor False True))))
    (is (= true (->bool (xor True False))))
    (is (= false (->bool (xor True True))))))

(deftest imply-test
  (testing
    (is (= true (->bool (imply False False))))
    (is (= true (->bool (imply False True))))
    (is (= false (->bool (imply True False))))
    (is (= true (->bool (imply True True))))))

(deftest first-test
  (testing
    (is (= 4 (first (pair 4 6))))))

(deftest second-test
  (testing
    (is (= 6 (second (pair 4 6))))))

(deftest numbers-test
  (testing
    (is (= 0 (->int zero)))
    (is (= 1 (->int one)))
    (is (= 2 (->int two)))
    (is (= 3 (->int three)))
    (is (= 4 (->int four)))
    (is (= 5 (->int five)))))

(deftest succ-test
  (testing
    (is (= 1 (->int (succ zero))))
    (is (= 2 (->int (succ (succ zero)))))
    (is (= 2 (->int (succ one))))))

(deftest plus-test
  (testing
    (is (= 0 (->int (plus zero zero))))
    (is (= 1 (->int (plus one zero))))
    (is (= 1 (->int (plus zero one))))
    (is (= 5 (->int (plus two three))))))

(deftest mult-test
  (testing
    (is (= 0 (->int (mult zero three))))
    (is (= 0 (->int (mult three zero))))
    (is (= 5 (->int (mult five one))))
    (is (= 5 (->int (mult one five))))
    (is (= 6 (->int (mult two three))))))

(deftest pow-test
  (testing
    (is (= 1 (->int (pow zero zero))))
    (is (= 0 (->int (pow zero two))))
    (is (= 1 (->int (pow three zero))))
    (is (= 8 (->int (pow two three))))))

(deftest zero?-test
  (testing
    (is (true? (->bool (zero? zero))))
    (is (false? (->bool (zero? one))))))

(deftest even?-test
  (testing
    (is (true? (->bool (even? zero))))
    (is (true? (->bool (even? two))))
    (is (false? (->bool (even? three))))))

(deftest odd?-test
  (testing
    (is (true? (->bool (odd? one))))
    (is (false? (->bool (odd? two))))))

(deftest pred-test
  (testing
    (is (= 0 (->int (pred zero))))
    (is (= 0 (->int (pred one))))
    (is (= 4 (->int (pred five))))))

(deftest minus-test
  (testing
    (is (= 3 (->int (minus five two))))
    (is (= 0 (->int (minus two five))))))

(deftest leq-test
  (testing
    (is (true? (->bool (leq two five))))
    (is (true? (->bool (leq five five))))
    (is (false? (->bool (leq five two))))))

(deftest less-test
  (testing
    (is (false? (->bool (less zero zero))))
    (is (true? (->bool (less two five))))
    (is (false? (->bool (less five five))))
    (is (false? (->bool (less five two))))))

(deftest eq-test
  (testing
    (is (false? (->bool (eq two five))))
    (is (true? (->bool (eq five five))))
    (is (false? (->bool (eq five two))))))

(deftest geq-test
  (testing
    (is (false? (->bool (geq two five))))
    (is (true? (->bool (geq five five))))
    (is (true? (->bool (geq five two))))))

(deftest head-test
  (testing
    (is (= 2 (head (cons 2 (cons 1 Nil)))))))

(deftest tail-test
  (testing
    (is (= 1 (head (tail (cons 2 (cons 1 Nil))))))))

(deftest Nil?-test
  (testing
    (is (true? (->bool (Nil? Nil))))
    (is (false? (->bool (Nil? (cons 1 Nil)))))))

(deftest head*-test
  (testing
    (is (= 2 (head* (cons* 2 (cons* 1 Nil*)))))))

(deftest tail*-test
  (testing
    (is (= 1 (head* (tail* (cons* 2 (cons* 1 Nil*))))))))

(deftest Nil*?-test
  (testing
    (is (true? (->bool (Nil*? Nil*))))
    (is (false? (->bool (Nil*? (cons* 1 Nil*)))))))

(deftest root-test
  (testing
    (is (= 1 (root (tree 1 2 3))))))

(deftest left-tree
  (testing
    (is (= 2 (left (tree 1 2 3))))))

(deftest right-test
  (testing
    (is (= 3 (right (tree 1 2 3))))))

(deftest convert-boolean-test
  (testing
    (is (true? (->bool (->λ true))))
    (is (false? (->bool (->λ false))))))

(deftest convert-int-test
  (testing
    (is (= 0 (->int (->λ 0))))
    (is (= 6 (->int (->λ 6))))
    (is (= 6 (->int (->λ (int 6)))))))

(deftest convert-list-test
  (testing
    (is (true? (->bool (Nil? (->λ (list))))))
    (is (= 1 (head (->λ (list 1 2 3)))))
    (is (= 2 (head (tail (->λ (list 1 2 3))))))))

(deftest β-reduction-test
  (testing
    (is (= 4 (β-reduction (If True 4 6))))
    (is (= 6 (β-reduction (If False 4 6))))
    (is (= 10 (->int (β-reduction (plus (->λ 4) (->λ 6))))))
    (is (= (list 'λ 'n '. 'True 4 'n) (:def (meta (β-reduction (If True 4))))))
    (is (= (list 'λ 'm '. 'm 'pred 'five) (:def (meta (β-reduction (minus five))))))
    (is (= (list 'λ 't 'f '. 't) (:def (meta (β-reduction (leq three four))))))
    (is (= (list 'λ 't 'f '. 'f) (:def (meta (β-reduction (leq five four))))))
    (is (= (list 'λ 't 'f '. 't) (:def (meta (β-reduction (even? (succ (succ (succ (succ zero)))))))))))
  (testing "shadowing: an inner binder with the same name blocks substitution"
    (is (= (list 'λ 'x '. 'x) (:def (meta (β-reduction ((λ x . (λ x . x)) 5)))))))
  (testing "over-application: leftover arguments stay applied to the reduced result"
    (is (= 4 (β-reduction ((λ x . x) True 4 6))))
    (is (= 10 (->int (β-reduction ((λ x . x) plus four (->λ 6)))))))
  (testing "capture-avoidance: a substituted free variable must not be captured by a same-named inner binder"
    (is (= free-var ((β-reduction ((λ x . (λ free-var . x)) free-var)) :anything)))))

;; Recursion needs applicative-order fixed point (FP), not the call-by-name Y,
;; since Clojure is strict: both branches of `If` would otherwise be evaluated
;; eagerly, so the recursive branch must be delayed behind a thunk and forced
;; with a dummy argument once the base case has been ruled out.
(def fact
  (FP (λ f n . (zero? n) (λ _ . one) (λ _ . mult n (f (pred n))) False)))

(deftest factorial-test
  (testing
    (is (= 1 (->int (fact zero))))
    (is (= 1 (->int (fact one))))
    (is (= 2 (->int (fact two))))
    (is (= 6 (->int (fact three))))
    (is (= 24 (->int (fact four))))
    (is (= 120 (->int (fact five))))))

(deftest integer-conversion-test
  (testing
    (is (= 0 (integer->long (->integer 0))))
    (is (= 3 (integer->long (->integer 3))))
    (is (= -3 (integer->long (->integer -3))))
    (is (= -3 (integer->long (pair (->λ 2) (->λ 5)))))
    (is (= 3 (integer->long (pair (->λ 5) (->λ 2)))))))

(deftest int-neg-test
  (testing
    (is (= -3 (integer->long (int-neg (->integer 3)))))
    (is (= 3 (integer->long (int-neg (->integer -3)))))
    (is (= 0 (integer->long (int-neg int-zero))))))

(deftest int-plus-test
  (testing
    (is (= -2 (integer->long (int-plus (->integer 3) (->integer -5)))))
    (is (= -8 (integer->long (int-plus (->integer -3) (->integer -5)))))
    (is (= 8 (integer->long (int-plus (->integer 3) (->integer 5)))))))

(deftest int-minus-test
  (testing
    (is (= -2 (integer->long (int-minus (->integer 3) (->integer 5)))))
    (is (= 2 (integer->long (int-minus (->integer -3) (->integer -5)))))
    (is (= 0 (integer->long (int-minus (->integer 3) (->integer 3)))))))

(deftest int-mult-test
  (testing
    (is (= -12 (integer->long (int-mult (->integer 3) (->integer -4)))))
    (is (= 12 (integer->long (int-mult (->integer -3) (->integer -4)))))
    (is (= 0 (integer->long (int-mult (->integer 0) (->integer -4)))))))

(deftest int-zero?-test
  (testing
    (is (true? (->bool (int-zero? (->integer 0)))))
    (is (true? (->bool (int-zero? (pair (->λ 2) (->λ 2))))))
    (is (false? (->bool (int-zero? (->integer 3)))))))

(deftest int-pos?-test
  (testing
    (is (true? (->bool (int-pos? (->integer 3)))))
    (is (false? (->bool (int-pos? (->integer -3)))))
    (is (false? (->bool (int-pos? (->integer 0)))))))

(deftest int-neg?-test
  (testing
    (is (true? (->bool (int-neg? (->integer -3)))))
    (is (false? (->bool (int-neg? (->integer 3)))))
    (is (false? (->bool (int-neg? (->integer 0)))))))

(deftest int-eq-test
  (testing
    (is (true? (->bool (int-eq (->integer 3) (->integer 3)))))
    (is (true? (->bool (int-eq (pair (->λ 2) (->λ 5)) (->integer -3)))))
    (is (false? (->bool (int-eq (->integer 3) (->integer -3)))))))

(deftest int-leq-test
  (testing
    (is (true? (->bool (int-leq (->integer -3) (->integer 3)))))
    (is (true? (->bool (int-leq (->integer 3) (->integer 3)))))
    (is (false? (->bool (int-leq (->integer 3) (->integer -3)))))))

(deftest int-less-test
  (testing
    (is (true? (->bool (int-less (->integer -3) (->integer 3)))))
    (is (false? (->bool (int-less (->integer 3) (->integer 3)))))
    (is (false? (->bool (int-less (->integer 0) (->integer 0)))))))

(deftest int-geq-test
  (testing
    (is (true? (->bool (int-geq (->integer 3) (->integer -3)))))
    (is (false? (->bool (int-geq (->integer -3) (->integer 3)))))
    (is (true? (->bool (int-geq (->integer 3) (->integer 3)))))))

(deftest rational-conversion-test
  (testing
    (is (= 0 (rational->ratio rat-zero)))
    (is (= 1 (rational->ratio rat-one)))
    (is (= 3/4 (rational->ratio (->rational 3 4))))
    (is (= -3/4 (rational->ratio (->rational -3 4))))
    (is (= 5 (rational->ratio (->rational 5))))
    (is (= 3/4 (rational->ratio (->rational 3/4))))))

(deftest rat-normalize-test
  (testing "the denominator is always kept positive, regardless of input signs"
    (is (= -3/4 (rational->ratio (->rational 3 -4))))
    (is (= 3/4 (rational->ratio (->rational -3 -4))))))

(deftest rat-neg-test
  (testing
    (is (= -3/4 (rational->ratio (rat-neg (->rational 3 4)))))
    (is (= 3/4 (rational->ratio (rat-neg (->rational -3 4)))))))

(deftest rat-inv-test
  (testing
    (is (= 4/3 (rational->ratio (rat-inv (->rational 3 4)))))
    (is (= -4/3 (rational->ratio (rat-inv (->rational -3 4)))))))

(deftest rat-plus-test
  (testing
    (is (= 5/6 (rational->ratio (rat-plus (->rational 1 2) (->rational 1 3)))))
    (is (= 0 (rational->ratio (rat-plus (->rational 1 2) (->rational -1 2)))))))

(deftest rat-minus-test
  (testing
    (is (= 1/6 (rational->ratio (rat-minus (->rational 1 2) (->rational 1 3)))))
    (is (= 0 (rational->ratio (rat-minus (->rational 1 2) (->rational 1 2)))))))

(deftest rat-mult-test
  (testing
    (is (= 1/2 (rational->ratio (rat-mult (->rational 2 3) (->rational 3 4)))))
    (is (= -1/2 (rational->ratio (rat-mult (->rational 2 3) (->rational -3 4)))))))

(deftest rat-div-test
  (testing
    (is (= 3/2 (rational->ratio (rat-div (->rational 1 2) (->rational 1 3)))))))

(deftest rat-zero?-test
  (testing
    (is (true? (->bool (rat-zero? (->rational 0 5)))))
    (is (false? (->bool (rat-zero? (->rational 3 4)))))))

(deftest rat-pos?-test
  (testing
    (is (true? (->bool (rat-pos? (->rational 3 4)))))
    (is (false? (->bool (rat-pos? (->rational -3 4)))))
    (is (false? (->bool (rat-pos? rat-zero))))))

(deftest rat-neg?-test
  (testing
    (is (true? (->bool (rat-neg? (->rational -3 4)))))
    (is (false? (->bool (rat-neg? (->rational 3 4)))))
    (is (false? (->bool (rat-neg? rat-zero))))))

(deftest rat-eq-test
  (testing
    (is (true? (->bool (rat-eq (->rational 1 2) (->rational 2 4)))))
    (is (true? (->bool (rat-eq (->rational -1 2) (->rational 1 -2)))))
    (is (false? (->bool (rat-eq (->rational 1 2) (->rational 1 3)))))))

(deftest rat-leq-test
  (testing
    (is (true? (->bool (rat-leq (->rational 1 3) (->rational 1 2)))))
    (is (true? (->bool (rat-leq (->rational -1 2) (->rational 1 3)))))
    (is (false? (->bool (rat-leq (->rational 1 2) (->rational 1 3)))))))

(deftest rat-less-test
  (testing
    (is (true? (->bool (rat-less (->rational 1 3) (->rational 1 2)))))
    (is (false? (->bool (rat-less (->rational 1 2) (->rational 1 2)))))))

(deftest rat-geq-test
  (testing
    (is (true? (->bool (rat-geq (->rational 1 2) (->rational 1 3)))))
    (is (false? (->bool (rat-geq (->rational 1 3) (->rational 1 2)))))))

(deftest length-test
  (testing
    (is (= 0 (->int (length Nil))))
    (is (= 3 (->int (length (->λ (list 1 2 3))))))))

(deftest append-test
  (testing
    (is (= [1 2 3 4 5] (->vec (append (->λ (list 1 2 3)) (->λ (list 4 5))))))
    (is (= [1 2 3] (->vec (append Nil (->λ (list 1 2 3))))))
    (is (= [1 2 3] (->vec (append (->λ (list 1 2 3)) Nil))))))

(deftest map-test
  (testing
    (is (= [2 3 4] (->vec (map inc (->λ (list 1 2 3))))))
    (is (= [] (->vec (map inc Nil))))))

(deftest filter-test
  (testing
    (is (= [2 4] (->vec (filter (λ x . ->λ (core/even? x)) (->λ (list 1 2 3 4 5))))))
    (is (= [] (->vec (filter (λ x . ->λ (core/even? x)) Nil))))))

(deftest member?-test
  (testing
    (is (true? (->bool (member? (λ a b . ->λ (= a b)) 3 (->λ (list 1 2 3))))))
    (is (false? (->bool (member? (λ a b . ->λ (= a b)) 9 (->λ (list 1 2 3))))))
    (is (false? (->bool (member? (λ a b . ->λ (= a b)) 1 Nil))))))

(deftest foldr-test
  (testing
    (is (= [1 2 3] (->vec (foldr cons Nil (->λ (list 1 2 3))))))
    (is (= 6 (->int (foldr plus zero (->λ (list one two three))))))))

(deftest foldl-test
  (testing
    (is (= [3 2 1] (->vec (foldl (λ acc h . cons h acc) Nil (->λ (list 1 2 3))))))
    (is (= 6 (->int (foldl plus zero (->λ (list one two three))))))))

(deftest reverse-test
  (testing
    (is (= [3 2 1] (->vec (reverse (->λ (list 1 2 3))))))
    (is (= [] (->vec (reverse Nil))))))

(deftest drop-test
  (testing
    (is (= [3 4 5] (->vec (drop two (->λ (list 1 2 3 4 5))))))
    (is (= [1 2 3] (->vec (drop zero (->λ (list 1 2 3))))))
    (is (= [] (->vec (drop three (->λ (list 1 2 3))))))
    (is (= [] (->vec (drop five (->λ (list 1 2 3))))))))

(deftest nth-test
  (testing
    (is (= 1 (nth zero (->λ (list 1 2 3)))))
    (is (= 3 (nth two (->λ (list 1 2 3)))))))

(deftest take-test
  (testing
    (is (= [] (->vec (take zero (->λ (list 1 2 3))))))
    (is (= [1 2] (->vec (take two (->λ (list 1 2 3 4 5))))))
    (is (= [1 2 3] (->vec (take three (->λ (list 1 2 3))))))
    (is (= [1 2 3] (->vec (take five (->λ (list 1 2 3))))))))

(deftest string-conversion-test
  (testing
    (is (= "hello" (string->str (->λ "hello"))))
    (is (= "" (string->str (->λ ""))))
    (is (= "héllo" (string->str (->λ "héllo"))))
    (is (true? (->bool (Nil? (->λ "")))))))

(deftest string-as-list-test
  (testing "a string is a Church list of char codes, so list operations apply directly"
    (is (= 5 (->int (length (->λ "hello")))))
    (is (= "foobar" (string->str (append (->λ "foo") (->λ "bar")))))
    (is (= "olleh" (string->str (reverse (->λ "hello")))))
    (is (= "hel" (string->str (take three (->λ "hello")))))
    (is (= "lo" (string->str (drop three (->λ "hello")))))
    (is (= 101 (->int (nth one (->λ "hello")))))
    (is (= "ifmmp" (string->str (map succ (->λ "hello")))))))

(deftest I-test
  (testing "I = S K K, the classic SKI derivation of identity"
    (is (= 5 (I 5)))
    (is (= "abc" (I "abc")))))

(deftest B-test
  (testing "B is the Bluebird (composition), an alias for compose"
    (is (= 5 (B ->int succ four)))))

(deftest C-test
  (testing "C is the Cardinal: C f x y = f y x"
    (is (= 2 (->int (C minus three five))))))

(deftest W-test
  (testing "W is the Warbler: W f x = f x x"
    (is (= 6 (->int (W plus three))))))

(deftest T-test
  (testing "T is the Thrush: T x f = f x"
    (is (= 6 (T 5 inc)))))

(deftest M-test
  (testing "M is the Mockingbird (self-application), an alias for ω"
    (is (= 10 (((M K) 5) 10 20)))))

(deftest V-test
  (testing "V is the Vireo (pairing), an alias for pair"
    (is (= 3 ((V 3 4) True)))
    (is (= 4 ((V 3 4) False)))))

(deftest fixed-point-combinators-exist-test
  (testing "Y and Θ are call-by-name fixed-point combinators: correctly defined,
            but diverge if actually applied under Clojure's strict evaluation
            (unlike FP, which is safe to call)"
    (is (fn? Y))
    (is (fn? Θ))))
