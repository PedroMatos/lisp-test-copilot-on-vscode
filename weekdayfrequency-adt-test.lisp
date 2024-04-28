

;; Load the Parachute library
(ql:quickload "parachute")

;; Use the Parachute library, shadowing 'report'
(defpackage :weekdayfrequency-test
  (:use :cl :parachute :weekdayfrequency)
  (:export :weekdayfrequency-adt-test-suite)
)

(in-package :weekdayfrequency-test)

;; Define a test suite  
(define-test weekdayfrequency-adt-test-suite)

(define-test parachute-ok
        :parent weekdayfrequency-adt-test-suite
    (let ((x 1) (y 1))
        (is = x y)
        (is equal x y)))

;;; Test the weekdayfrequency-adt module constructors
(define-test weekdayfrequency-adt-constructor-test
        :parent weekdayfrequency-adt-test-suite
    (let ((empty-wf1 (make-weekdayfrequency))
          (empty-wf2 (make-weekdayfrequency)))
        (is equalp empty-wf1 empty-wf2))
    (is equalp (make-weekdayfrequency :bit-days #b0000000) (make-weekdayfrequency))
    (is equalp (make-weekdayfrequency :bit-days #b0000001) (make-weekdayfrequency :bit-days #b0000001)))

(define-test weekdayfrequency-adt-from-string-test
        :parent weekdayfrequency-adt-test-suite
    (is equalp (weekdayfrequency-from-string "0") (make-weekdayfrequency))
    (is equalp (weekdayfrequency-from-string "1") (make-weekdayfrequency :bit-days #b0000001))
    (is equalp (weekdayfrequency-from-string "2") (make-weekdayfrequency :bit-days #b0000010))
    (is equalp (weekdayfrequency-from-string "3") (make-weekdayfrequency :bit-days #b0000100))
    (is equalp (weekdayfrequency-from-string "4") (make-weekdayfrequency :bit-days #b0001000))
    (is equalp (weekdayfrequency-from-string "5") (make-weekdayfrequency :bit-days #b0010000))
    (is equalp (weekdayfrequency-from-string "6") (make-weekdayfrequency :bit-days #b0100000))
    (is equalp (weekdayfrequency-from-string "7") (make-weekdayfrequency :bit-days #b1000000))
    (is equalp (weekdayfrequency-from-string "12") (make-weekdayfrequency :bit-days #b0000011))
    (is equalp (weekdayfrequency-from-string "1234567") (make-weekdayfrequency :bit-days #b1111111)))

;; Run the test
(parachute:test 'weekdayfrequency-adt-test-suite)