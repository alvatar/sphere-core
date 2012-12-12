Appendix A. Test cases for symbol??
The test cases are tried on SCM, Petite Chez Scheme, Scheme48 and
Bigloo.

(display (symbol?? a 'yes #f))
(newline)
(display (symbol?? 34 'yes #f))
(newline)
(display (symbol?? #xff 'yes #f))
(newline)
(display (symbol?? () 'yes #f))
(newline)
(display (symbol?? quote 'yes #f))
(newline)
(symbol?? display (display 'yes) (display #f))
(newline)
(symbol?? (x x x x) (display 'yes) (display #f))
(newline)
(symbol?? (() . 1) (display 'yes) (display #f))
(newline)
(symbol?? (x ... x) (display 'yes) (display #f))
(newline)
(let ((yes 'yes) (no 'no))
  (display (symbol?? yes yes no))
  (newline))

Appendix B. Test cases for id-eq?? and id-eqv??

(display "id-eq??") (newline)

(display (id-eq?? foo foo 'yes #f))
(newline)
(display (id-eq?? foo bar 'yes #f))
(newline)

(let ((foo 'yes))
  (display (id-eq?? foo foo foo #f)))
(newline)

(display "id-eqv??") (newline)
(display (id-eqv?? foo foo 'yes #f))
(newline)
(display (id-eqv?? foo bar 'yes #f))
(newline)

(let ((foo 'yes))
  (display (id-eqv?? foo foo foo #f)))
(newline)

(define-syntax mfoo
  (syntax-rules ()
    ((mfoo tester a)
      (tester foo a 'yes 'no))))

; expected answer: (id-eq??:  no no)
(display
  (list "id-eq??: "
    (mfoo id-eq?? foo)
    (let ((foo 1)) (mfoo id-eq?? foo))))
(newline)

; expected answer: (id-eqv??:  yes no)
(display
  (list "id-eqv??: "
    (mfoo id-eqv?? foo)
    (let ((foo 1)) (mfoo id-eqv?? foo))))
(newline)
