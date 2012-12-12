;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Functional programming procedures

(cond-expand
 (gambit
  (declare (standard-bindings)
           (extended-bindings)
           (block)
           (mostly-generic)))
 (else))

;-------------------------------------------------------------------------------
; Functional operators
;-------------------------------------------------------------------------------

;;; U-combinator

(define U
  (lambda (f) (f f)))

;;; Y-combinator

(define Y
  (lambda (X)
    (U (lambda (proc)
         (X (lambda (arg) ((U proc) arg)))))))

;;; The applicative-order imperative y-combinator (by Peter Landin)

(define Y!
  (lambda (f)
    (letrec
        ((h (f (lambda (arg) (h arg)))))
      h)))

;;; Function composition

(define (composer reducer . fns)
  (reducer (lambda (fn chain)
            (lambda args
              (call-with-values (lambda () (apply fn args)) chain)))
          values
          fns))

;;; Compose executing the last function first (it starts from the right side)

;; (define (compose-right . fns)
;;   (apply composer reduce fns))

;; (define compose compose-right) ; TODO: a less general but more optimized implementation for compose?

;;; Compose executing the first function first (it starts from the left side)

;; (define (compose-left . fns)
;;   (apply composer reduce-right fns))

;; (define pipe compose-left)

;;; Negate a function

(define (negate f)
  (lambda args (not (apply f args))))

;;; Adjoin several functions
;;; f1 f2 f3 -> values f1 f2 f3

(define (adjoin . fs)
  (error "Not implemented"))

;-------------------------------------------------------------------------------
; Currying / uncurrying
;-------------------------------------------------------------------------------

;;; Explicit currying of an arbitrary function

(define (curry fun arg1 . args)
  (if (pair? args)
      (let ((all-args (cons arg1 args)))
        (lambda x
          (apply fun (append all-args x))))
      (lambda x
        (apply fun (cons arg1 x)))))

;;; Some helpful curry functions

(define (lambda-eq? x) (curry eq? x))
(define (lambda-eqv? x) (curry eqv? x))
(define (lambda-equal? x) (curry equal? x))

;;; Uncurrying
;;;
;;; (uncurry (lambda (a) (lambda (b) (lambda (c) (+ a b c)))) 5 2 1)

(define (uncurry f . arglist)
  (if (null? arglist) f
    (apply uncurry (f (car arglist)) (cdr arglist))))

;-------------------------------------------------------------------------------
; Memoization
;-------------------------------------------------------------------------------

;;; Function computation memoization specifying a key generation procedure

(define (memoize/key-gen key-gen f)
  (let ((memos '())) ; OPTIMIZE: hash table!
    (lambda args
      (let ((key (apply key-gen args)))
        (apply
          values
          (cond
           ((assoc key memos)
            => cdr)
           (else
             (call-with-values
               (lambda ()
                 (apply f args))
               (lambda results
                 (set! memos ; Put the new result in memos
                   (cons (cons key results)
                         memos))
                 results)))))))))

;;; Function computation memoization with default key generation

(define (memoize f)
  (memoize/key-gen values f))

