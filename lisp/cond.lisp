(setq a 1)

(defun testcond (fac)
    (let ((ans *[a, fac]))
        (cond
            ((= a 3) (+ ans 2))
            ((= a 2) (+ ans 1))
            ((= a 1) ans))))