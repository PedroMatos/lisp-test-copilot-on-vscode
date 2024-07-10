;;; Lets start by loading the test library to write the tests.
;;; We will use the parachute library for this purpose.
;;; Homepage: https://github.com/Shinmera/parachute
(ql:quickload "parachute")

(defpackage :my-package
  (:use :cl :parachute))

(in-package :my-package)

;;; Check that parachute is available.

(define-test test-parachute-is-present
     (true (= 1 1)))

(define-test (test-parachute-is-present too-slow)
             :time-limit 0.5
             (sleep 0.1)
             (is eql 1 1))

;;; Let's use a TDD approach:
;;; 1. Write a test for the function we need to implement.
;;; 2. Run the test and see it fail (since the function is not yet implemented).
;;; 3. Write the minimal amount of code to make the test pass.
;;; 4. Refactor the code while ensuring the test still passes.
;;; 5. Repeat the process for each new feature or function.


;;; Lets compare the performance of two functions: union and union-sorted-list.
;;; union is Lips default function to merge two lists.
;;; union-sorted-list is a function that merges two sorted lists.
;;; We will compare the performance of these two functions.

(define-test union-test-suite)

(define-test (union-test-suite test-union)
             (is equal (list 1 2 3 4 5) (sort (union (list 1 2 3) (list 3 4 5)) '<)))

(define-test (union-test-suite test-union-sorted-list)
             (is equal (list 1 2 3 4 5) (union-sorted-list (list 1 2 3) (list 3 4 5)))
             (is equal (list 1 2 3 4 5) (remove-duplicates (union-sorted-list (list 1 2 2 3) (list 3 4 5))))
             (is equal (list 1 2 3 4 5) (union-sorted-list (list 1 2 3) (list 3 4 5) :key (lambda (x) x) :lessp '<))
             ;; Test with a custom key function
             (is equal (list (cons 99 1) (cons 98 2) (cons 97 3) (cons 95 4) (cons 94 5))
                 (union-sorted-list (list (cons 99 1) (cons 98 2) (cons 97 3))
                                    (list (cons 96 3) (cons 95 4) (cons 94 5)) :key 'cdr :lessp '<)))

;;; Lets now define a test to assure that the union-sorted-list is faster than the union function.

(defun get-random-list (&optional (size 10000))
  (loop repeat size collect (random size)))

(defun measure-runtime (function &optional (times 10))
  (let ((start-time (get-internal-real-time)))
    (loop repeat times do (funcall function))
    (- (get-internal-real-time) start-time)))

(define-test (union-test-suite test-union-sorted-list-is-faster-10000)
             (let ((list1 (sort (get-random-list 10000) '<))
                   (list2 (sort (get-random-list 10000) '<)))
               (let ((union-time (measure-runtime (lambda () (union list1 list2)) 10))
                     (union-sorted-list-time (measure-runtime (lambda () (union-sorted-list list1 list2)) 10)))
                 (format t "~&~%union-time: ~a~&" union-time)
                 (format t "~&union-sorted-list-time: ~a~&~%" union-sorted-list-time)
                 (true (< union-sorted-list-time union-time))
                 (true (< union-sorted-list-time (/ union-time 2))))))

(define-test (union-test-suite test-union-sorted-list-is-faster-1000)
             (let ((list1 (sort (get-random-list 1000) '<))
                   (list2 (sort (get-random-list 1000) '<)))
               (let ((union-time (measure-runtime (lambda () (union list1 list2)) 100))
                     (union-sorted-list-time (measure-runtime (lambda () (union-sorted-list list1 list2)) 100)))
                 (format t "~&~%union-time: ~a~&" union-time)
                 (format t "~&union-sorted-list-time: ~a~&~%" union-sorted-list-time)
                 (true (< union-sorted-list-time union-time))
                 (true (< union-sorted-list-time (/ union-time 2))))))

;;; The union-sorted-list function is implemented as follows:
;;;
;;; This function takes two sorted lists, list1 and list2, and returns a new list
;;; that contains all the elements from both lists, in sorted order. The sorting
;;; order is determined by the 'lessp' function, which defaults to the '<' operator.
;;; The 'key' function can be used to extract a key from each element of the lists
;;; for comparison. By default, it uses the 'identity' function, which returns the
;;; element itself.
;;;
;;; Example usage:
;;; (union-sorted-list '(1 3 5) '(2 4 6)) => (1 2 3 4 5 6)
(defun union-sorted-list (list1 list2 &key (key 'identity) (lessp '<))
  (cond
    ((null list1) list2)
    ((null list2) list1)
    ((funcall lessp (funcall key (first list1)) (funcall key (first list2)))
     (cons (first list1) (union-sorted-list (rest list1) list2 :key key :lessp lessp)))
    ((funcall lessp (funcall key (first list2)) (funcall key (first list1)))
     (cons (first list2) (union-sorted-list list1 (rest list2) :key key :lessp lessp)))
    (t
      (cons (first list1) (union-sorted-list (rest list1) (rest list2) :key key :lessp lessp)))))


(test '(test-parachute-is-present union-test-suite))
