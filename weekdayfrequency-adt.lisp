#|
ADT Specification: WeekdayFrequency

External Representation:
- Days are represented by digits from 1 (Monday) to 7 (Sunday).
- Weekday sets are represented as integers with each digit corresponding to a specific day, e.g.:
  - 0 represents no days.
  - 135 represents Monday, Wednesday, and Friday.
  - 1234567 represents all days of the week from Monday to Sunday.

Constructors:
- WeekdayFrequency(): Initializes the frequency to 0 (no days).
- WeekdayFrequency(days): Initializes the frequency with the given days, for example, WeekdayFrequency(135) for Monday, Wednesday, and Friday.

Methods:
- addDay(day): Adds a specific day to the frequency.
- removeDay(day): Removes a specific day from the frequency.

Read/Write:
- toString(): Returns the string representation of the frequency, allowing for an easy human-readable format.
- fromString(s): Initializes the frequency from a string by setting the frequency.

Operations:
- union(other): Combines two frequencies into a new WeekdayFrequency that represents the union of both.
- intersection(other): Creates a new WeekdayFrequency that represents the common days in two frequencies.
- shiftForward(): Shifts all days in the frequency forward by one day, adjusting for wraparound (e.g., Sunday becomes Monday).
- shiftBackward(): Shifts all days in the frequency backward by one day, also adjusting for wraparound (e.g., Monday becomes Sunday).

This specification guides the implementation of the ADT in Lisp, with a focus on set-based manipulation and string operations for ease of handling and clarity in operations.
|#

(defpackage :weekdayfrequency
  (:use :cl)
  (:export
    :weekdayfrequency
    :make-weekdayfrequency
    :weekdayfrequency-from-string
    :+monday+
    :+tuesday+
    :+wednesday+
    :+thursday+
    :+friday+
    :+saturday+
    :+sunday+
    :weekdayfrequency-add-day))
  
(in-package :weekdayfrequency)


;;; WeekdayFrequency ADT
;;; To be extra fast and efficient, we will use a binary representation to represent the days of the week frequency.
;;; Each bit will represent a day of the week, with the least significant bit representing Monday and the most significant bit representing Sunday.
;;; For example, the binary number #b0010101 represents Monday, Wednesday, and Friday.
(defstruct weekdayfrequency
  (bit-days #b0000000))

;;; Constants
(defconstant +monday+ #b0000001)
(defconstant +tuesday+ #b0000010)
(defconstant +wednesday+ #b0000100)
(defconstant +thursday+ #b0001000)
(defconstant +friday+ #b0010000)
(defconstant +saturday+ #b0100000)
(defconstant +sunday+ #b1000000)

;;; WeekdayFrequency-from-string
;;; Converts a string representation of a weekday frequency to a WeekdayFrequency object.
;;; The string should contain digits from 1 to 7, representing the days of the week.
;;; Example: "135" represents Monday, Wednesday, and Friday.
(defun weekdayfrequency-from-string (str)
    "Creates a new WeekdayFrequency from a string representation of days."
    (let ((days 0))
      (dolist (c (coerce str 'list))
        (case c
          (#\1 (setf days (logior days +monday+)))
          (#\2 (setf days (logior days +tuesday+)))
          (#\3 (setf days (logior days +wednesday+)))
          (#\4 (setf days (logior days +thursday+)))
          (#\5 (setf days (logior days +friday+)))
          (#\6 (setf days (logior days +saturday+)))
          (#\7 (setf days (logior days +sunday+)))))
      (make-weekdayfrequency :bit-days days)))

(defun weekdayfrequency-add-day (frequency day)
  "Returns a new WeekdayFrequency with the given day added."
  (make-weekdayfrequency :bit-days (logior (weekdayfrequency-bit-days frequency) day)))

