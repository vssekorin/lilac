(ns lilac.core-test
  (:require [clojure.test :refer :all]
            [lilac.core :refer :all]))

(def two (succ (succ zero)))
(def three (succ (succ (succ zero))))
(def four (succ (succ (succ (succ zero)))))
(def five (succ (succ (succ (succ (succ zero))))))

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
    (is (= 6 (->int (->λ 6))))))

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
    (is (= (list 'λ 't 'f '. 't) (:def (meta (β-reduction (even? (succ (succ (succ (succ zero))))))))))))
