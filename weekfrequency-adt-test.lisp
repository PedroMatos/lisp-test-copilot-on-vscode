

;; Load the Parachute library
(ql:quickload "parachute")

;; Use the Parachute library, shadowing 'report'
(defpackage :weekfrequency-test
  (:use :cl :parachute)
  (:export :weekfrequency-adt-test-suite)
)

(in-package :weekfrequency-test)

;; Define a test suite  
(define-test weekfrequency-adt-test-suite)

(define-test parachute-ok
        :parent weekfrequency-adt-test-suite
    (let ((x 1) (y 1))
        (is = x y)
        (is equal x y)))

;; Run the test
(parachute:test 'weekfrequency-adt-test-suite)