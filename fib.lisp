;;; Calculates the Fibonacci of the nth number.
(defun fibonacci (n)
    (if (<= n 2)
        1
        (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;;; Test fibonacci
(eq (fibonacci 1) 1)
