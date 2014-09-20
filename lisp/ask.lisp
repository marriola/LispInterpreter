(defun ask ()
    (format nil "Tell me your name: ")
    (setq name (read-line 0))

    (format nil "How many times should I say your name? ")
    (setq num (read 0))
	(setq count 1)
    (while
        (format t "{}: Hello, {}!" count name)
        (setq count (1+ count))
        (if (> count num) '(return))
    )
)

(ask)