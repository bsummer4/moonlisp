(! a "testestest")
(! p (λ(x) (print "--" x)))
(p a)
(do (p 3) (p #t) (p #f) (p #nil))
(! t (λ(x y) (λ(z) y)))
(p ((t "x" "y") "z"))
(! aa (if #nil #f #t))
(! bb (if #t #nil #f))
(p (. (tbl a=aa b=bb) "a"))
(os.exit)