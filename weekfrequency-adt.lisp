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
- addDay(day): Adds a specific day to the frequency. This involves converting the frequency to a string if not already in string form, appending the day, sorting the result, and removing duplicates.
- removeDay(day): Removes a specific day from the frequency. This involves converting the frequency to a string if not already in string form and removing the specified day.

Read/Write:
- toString(): Returns the string representation of the frequency, allowing for an easy human-readable format.
- fromString(s): Initializes the frequency from a string by setting the frequency, ensuring there are no duplicates and that the order is sorted.

Operations:
- union(other): Combines two frequencies into a new WeekdayFrequency that represents the union of both. This operation involves merging and sorting both frequencies and removing any duplicate entries.
- intersection(other): Creates a new WeekdayFrequency that represents the common days in two frequencies. This involves intersecting the days listed in both frequencies.
- shiftForward(): Shifts all days in the frequency forward by one day, adjusting for wraparound (e.g., Sunday becomes Monday).
- shiftBackward(): Shifts all days in the frequency backward by one day, also adjusting for wraparound (e.g., Monday becomes Sunday).

This specification guides the implementation of the ADT in Lisp, with a focus on set-based manipulation and string operations for ease of handling and clarity in operations.
|#

(defpackage :weekfrequency
  (:use :cl)
  (:export :WeekdayFrequency))
  
(in-package :weekfrequency)

