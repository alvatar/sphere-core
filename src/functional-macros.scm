;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Functional programming procedures

;-------------------------------------------------------------------------------
; Function generation
;-------------------------------------------------------------------------------

;;; Defines a function and its associated associative function (that will take
;;; any number of arguments and apply it to the result of the two previous ones)

(define-syntax define-associative-aux
  (syntax-rules ()
    ((_ name f)
     (define-syntax name
       (syntax-rules ()
         ((_ arg1 arg2)
          (f arg1 arg2))
         ((_ arg1 arg2 . rest)
          (name (f arg1 arg2) . rest)))))))

(define-syntax define-associative
  (syntax-rules ()
    ((_ name (f arg1 arg2) body)
     (begin
       (define (f arg1 arg2) body)
       (define-associative-aux name f)))))

;;; Make a generator function
; Try to generalize these:
; 
; (define (power-seq n)
;   (let ((i 0))
;     (lambda ()
;       (set! i (+ 1 i))
;       (expt i n))))
;  
; (define (filter-seq m n)
;   (let* ((s1 (power-seq m)) (s2 (power-seq n))
;           (a 0) (b 0))
;     (lambda ()
;       (set! a (s1))
;       (let loop ()
;   (if (>= a b) (begin
;            (cond ((> a b) (set! b (s2)))
;            ((= a b) (set! a (s1))))
;            (loop))))
;       a)))
;  
; (let loop ((seq (filter-seq 2 3)) (i 0))
;   (if (< i 30)
;     (begin
;       (if (> i 20)
;   (begin
;     (display (seq))
;     (newline))
;   (seq))
;       (loop seq (+ 1 i)))))


;;; Define an automatically curryable function
;;;
;;; (define-curried (foo x y z) (+ x (/ y z))) ;; foo has arity 3
;;; ((foo 3) 1 2) ;; (foo 3) is a procedure with arity 2
;;; ((foo 3 1) 2) ;; (foo 3 2) is a procedure with arity 1

(define-syntax curried
  (syntax-rules ()
    ((_ () body ...)
     (lambda () body ...))
    ((_ (arg) body ...)
     (lambda (arg) body ...))
    ((_ (arg args ...) body ...)
     (lambda (arg . rest)
       (let ((next (curried (args ...) body ...)))
         (if (null? rest)
             next
             (apply next rest)))))))

(define-syntax define-curried
  (syntax-rules ()
    ((_ (name args ...) body ...)
     (define name (curried (args ...) body ...)))))

;;; Curried lambda
;;;
;;; (lambda-curried (x y z) (+ x y z)) =>
;;;   (lambda (x) (lambda (y) (lambda (z) (+ x y z))))
;;; (map map (map (lambda-curried (a b) (* a b)) '(1 2 3)) '((4 5 6) (7 8 9) (10 11 12)))

(define-macro (lambda-curried bindings . body)
  (define (fold-right kons knil lis1)
    (let recur ((lis lis1))
       (if (null? lis) knil
	    (let ((head (car lis)))
	      (kons head (recur (cdr lis)))))))
  (if (null? bindings) `(lambda () ,@body)
    (fold-right (lambda (arg curr-body) `(lambda (,arg) ,curr-body))
	 (cons 'begin body) bindings)))


;;; Macro for memoized function definition (with default key generator)

(define-syntax define-memoized
  (syntax-rules (lambda)
    ((_ (name args ...) body ...)
     (define name
       (letrec ((name (lambda (args ...) body ...)))
         (memoize name))))
    ((_ name (lambda (args ...) body ...))
     (define-memoized (name args ...) body ...))))

;;; Macro for memoized function definition (specifying a key generator)

(define-syntax define-memoized/key-gen
  (syntax-rules ()
	((_ name
       (lambda (args-for-key ...) body-for-key ...)
       (lambda (args ...) body ...))
	 (define name
	   (letrec ((name (lambda (args ...) body ...)))
         (memoize/key-gen
		   (lambda (args-for-key ...) body-for-key ...)
           name))))))



;;TODO!!!
;; r2rs-style currying define.
'(define-syntax define
   (let-syntax ((old-define define))
     (letrec-syntax
	 ((new-define
	   (syntax-rules ()
	     ((_ (var-or-prototype . args) . body)
	      (new-define var-or-prototype (lambda args . body)))
	     ((_ var expr) (old-define var expr)))))
       new-define)))

'(define-syntax define
   (let-syntax ((old-define define))
     (define-syntax new-define
       (syntax-rules ()
	 ((_ (var-or-prototype . args) . body)
	  (new-define var-or-prototype (lambda args . body)))
	 ((_ var expr) (old-define var expr))))
     new-define))

'(let ((multiplier 2))
   (define ((curried-* x) y) (* x y))
   (map (curried-* multiplier) '(3 4 5)))

