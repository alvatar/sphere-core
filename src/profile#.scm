;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Basic profiling

;;; Code based on Phil Dawe's solution

(define %%*timerhash* (make-table))

;;; Reset the timer table

(define (%reset-timer) (set! %%*timerhash* (make-table)))

;;; Core procedure measuring time

(define (%%accum-time name thunk)
  (let* ((timebefore (real-time))
         (res (thunk)))
    (table-set! %%*timerhash* name 
                (let ((current (table-ref %%*timerhash* name '(0 0))))
                  (list (+ (car current) (- (real-time) timebefore))
                        (+ (cadr current) 1))))
    res))

;;; Get the times for timed functions

(define (%get-times)
  (letrec ((quicksort
            (lambda (l gt?)
              (define (split-by l p k)
                (let loop ((low '())
                           (high '())
                           (l l))
                  (cond ((null? l)
                         (k low high))
                        ((p (car l))
                         (loop low (cons (car l) high) (cdr l)))
                        (else
                         (loop (cons (car l) low) high (cdr l))))))
              (if (null? l)
                  '()
                  (split-by (cdr l) 
                            (lambda (x) (gt? x (car l)))
                            (lambda (low high)
                              (append (quicksort low gt?)
                                      (list (car l))
                                      (quicksort high gt?)))))))
           (thunk-name car)
           (thunk-time cadr)
           (thunk-executions caddr))
    (map (lambda (e) (list (thunk-name e) 
                      (* 1000 (thunk-time e))
                      (thunk-executions e)))
         (quicksort (table->list %%*timerhash*)
                    (lambda (a b) (< (thunk-time a)
                                (thunk-time b)))))))

;;; Explicit definition

(define-macro (%define-timed name+args . body)
  (if (list? name+args)
      `(##define ,name+args
         (%%accum-time ',(car name+args)
                       (lambda ()
                         ,@body)))
      `(##define ,name+args ,@body)))

;;; Activate implicit definition

(define-macro (%activate-profiling)
  '(define-macro (define name+args . body)
     (if (list? name+args)
         `(##define ,name+args
            (%%accum-time ',(car name+args)
                          (lambda ()
                            ,@body)))
         `(##define ,name+args ,@body))))
