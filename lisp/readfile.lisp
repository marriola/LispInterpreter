(open fib.lisp)
(setq out "")
(while
    (let (ch (read-char 3)) (x (setq out (concatenate 'string out ch)))
        ch))