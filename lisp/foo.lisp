(print (+ (* 10 2) 5))
; 10 * 2 + 5 = 25

(defun sqrt (n) (expt n .5))

(print (sqrt 64))
(print (sqrt 8))

; n(n + 1) / 2
(defun sum (n) (/ (* n (1+ n)) 2))

(print (sum 10))
(print (sum 11))