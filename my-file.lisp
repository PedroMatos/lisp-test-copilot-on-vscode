;;; Lets start by loading the test library to write the tests.
;;; We will use the parachute library for this purpose.
;;; Homepage: https://github.com/Shinmera/parachute
(ql:quickload "parachute")

(defpackage :my-package
  (:use :cl :parachute))

(in-package :my-package)

;;; Check that parachute is available.

(define-test test-suite)

(define-test (test-suite test-parachute-time-limit)
             :time-limit 0.2
             (sleep 0.1)
             (is eql 1 1))

#|
Let's use a TDD approach:
|#

(define-test (test-suite test-something)
  ;; Test something to ensure parachute is working.
  (is eql 1 1))


(test '(test-suite))
