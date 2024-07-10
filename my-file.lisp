
;;; Lets start by loading the test library to write the tests.
(ql:quickload "parachute")

(defpackage :my-package
  (:use :cl :parachute))

(in-package :my-package)

;;; Check that parachute is available.

(define-test test-parachute-is-present
     (true (= 1 1)))

(test 'test-parachute-is-present)

