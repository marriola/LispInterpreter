(setq cache (make-array 1000))
(setf (elt cache 1) 1)
(setf (elt cache 2) 1)

(defun fib-recursive (n)
    (let ((x (elt cache n)))
        (if x
            x
            (setf x (+ (fib-recursive (- n 2)) (fib-recursive (- n 1))))
        )
    )
)

(defun fib-binet (n)
    (let ((root5 (expt 5 .5)))
        (/ (- (expt (+ 1 root5) n) (expt (- 1 root5) n))
           (* (expt 2 n) root5)
        )
    )
)