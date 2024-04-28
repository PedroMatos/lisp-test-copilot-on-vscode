

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

(define-test weekdayfrequency-add-day-test
        :parent weekdayfrequency-adt-test-suite
    (let ((wf (make-weekdayfrequency)))
        (is equalp wf (make-weekdayfrequency :bit-days #b0000000))
        (is equalp (weekdayfrequency-add-day wf +monday+)  (weekdayfrequency-from-string "1"))
        (is equalp (weekdayfrequency-add-day wf +tuesday+)  (weekdayfrequency-from-string "2"))
        (is equalp (weekdayfrequency-add-day wf +wednesday+)  (weekdayfrequency-from-string "3"))
        (is equalp (weekdayfrequency-add-day wf +thursday+)  (weekdayfrequency-from-string "4"))
        (is equalp (weekdayfrequency-add-day wf +friday+)  (weekdayfrequency-from-string "5"))
        (is equalp (weekdayfrequency-add-day wf +saturday+)  (weekdayfrequency-from-string "6"))
        (is equalp (weekdayfrequency-add-day wf +sunday+)  (weekdayfrequency-from-string "7")))
    (let ((wf (weekdayfrequency-from-string "7")))
        (is equalp wf (weekdayfrequency-from-string "7"))
        (is equalp (weekdayfrequency-add-day wf +monday+) (weekdayfrequency-from-string "17"))
        (is equalp (weekdayfrequency-add-day wf +tuesday+) (weekdayfrequency-from-string "27"))
        (is equalp (weekdayfrequency-add-day wf +wednesday+) (weekdayfrequency-from-string "37"))
        (is equalp (weekdayfrequency-add-day wf +thursday+) (weekdayfrequency-from-string "47"))
        (is equalp (weekdayfrequency-add-day wf +friday+) (weekdayfrequency-from-string "57"))
        (is equalp (weekdayfrequency-add-day wf +saturday+) (weekdayfrequency-from-string "67"))
        (is equalp (weekdayfrequency-add-day wf +sunday+) (weekdayfrequency-from-string "7"))
    (let ((wf (weekdayfrequency-from-string "1234567")))
        (is equalp wf (weekdayfrequency-from-string "1234567"))
        (is equalp (weekdayfrequency-add-day wf +monday+) (weekdayfrequency-from-string "1234567"))
        (is equalp (weekdayfrequency-add-day wf +tuesday+) (weekdayfrequency-from-string "1234567"))
        (is equalp (weekdayfrequency-add-day wf +wednesday+) (weekdayfrequency-from-string "1234567"))
        (is equalp (weekdayfrequency-add-day wf +thursday+) (weekdayfrequency-from-string "1234567"))
        (is equalp (weekdayfrequency-add-day wf +friday+) (weekdayfrequency-from-string "1234567"))
        (is equalp (weekdayfrequency-add-day wf +saturday+) (weekdayfrequency-from-string "1234567"))
        (is equalp (weekdayfrequency-add-day wf +sunday+) (weekdayfrequency-from-string "1234567")))))

;; Run the test
(parachute:test 'weekdayfrequency-adt-test-suite)