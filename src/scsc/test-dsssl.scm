
(let-syntax ((test (syntax-rules ()
                     ((_ formals body args result)
                      (begin
                        (pp (vector '((lambda formals body) . args) '-> result))
                        (or (and (equal? ((lambda formals body) . args) result)
                                 
                                 (equal? (let ()
                                           (define (t . formals) body)
                                           (t . args)) result))
                            (error `(fail formals '-> ,((lambda formals body) . args)))))))))
  
  (test () 1 () 1)
  (test l l (1 2 3) '(1 2 3))
  (test (a) a (1) 1)
  (test (a . r) `(,a ,r) (1 2 3) '(1 (2 3)))
  (test (a b) `(,a ,b) (1 2) '(1 2))
  (test (#!optional a) a () #f)
  (test (#!optional a) a (1) 1)
  (test (a #!optional b) `(,a ,b) (1) '(1 #f))
  (test (a #!optional b) `(,a ,b) (1 2) '(1 2))
  (test (a #!optional (b 3)) `(,a ,b) (1) '(1 3))
  (test (a #!optional (b 3)) `(,a ,b) (1 4) '(1 4))
  (test (a #!optional b #!rest r) `(,a ,b ,r) (1) '(1 #f ()))
  (test (a #!optional b . r) `(,a ,b ,r) (1) '(1 #f ()))
  (test (a #!optional (b 3) #!rest r) `(,a ,b ,r) (1) '(1 3 ()))
  (test (a #!optional (b 3) . r) `(,a ,b ,r) (1) '(1 3 ()))
  (test (a #!optional (b 3) #!rest r) `(,a ,b ,r) (1 5) '(1 5 ()))
  (test (a #!optional (b 3) . r) `(,a ,b ,r) (1 5) '(1 5 ()))
  (test (a #!optional (b 3) #!rest r) `(,a ,b ,r) (1 5 7) '(1 5 (7)))
  (test (a #!optional (b 3) . r) `(,a ,b ,r) (1 5 7) '(1 5 (7)))
  (test (#!key a) a () #f)
  (test (#!key (a 4)) a () 4)
  (test (#!key a) a (a: 1) 1)
  (test (#!key (a 5)) a (a: 1) 1)
  (test (x #!key a) `(,x ,a) (1) '(1 #f))
  (test (x #!key a) `(,x ,a) (1 a: 3) '(1 3))
  (test (x #!key (a 4)) `(,x ,a) (1) '(1 4))
  (test (x #!key (a 4)) `(,x ,a) (1 a: 2) '(1 2))
  (test (x #!optional o #!key a) `(,x ,o ,a) (1) '(1 #f #f))
  (test (x #!optional o #!key a #!rest r) `(,x ,o ,a ,r) (1 2 3 4) '(1 2 #f (3 4)))
  (test (x #!optional o #!key a . r) `(,x ,o ,a ,r) (1 2 3 4) '(1 2 #f (3 4)))
  (test (x #!optional o #!key a #!rest r) `(,x ,o ,a ,r) (1 2 a: 3 4) '(1 2 3 (4)))
  (test (x #!optional o #!key a . r) `(,x ,o ,a ,r) (1 2 a: 3 4) '(1 2 3 (4)))
  (test (x #!optional o #!key (a 3) #!rest r) `(,x ,o ,a ,r) (1 2 4) '(1 2 3 (4)))
  (test (x #!optional o #!key (a 3) . r) `(,x ,o ,a ,r) (1 2 4) '(1 2 3 (4)))
  (test (x #!optional o #!key (a 3) #!rest r) `(,x ,o ,a ,r) (1 2 a: 4 4) '(1 2 4 (4)))
  (test (x #!optional o #!key (a 3) . r) `(,x ,o ,a ,r) (1 2 a: 4 4) '(1 2 4 (4)))
  (let ((a 1))
    (or (equal? ((lambda (#!optional (b (begin (+ 1 a)))) b)) 2)
        (error 'fail-1)))
  (pp 'pass))