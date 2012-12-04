;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Operations with lists and values

;;; Values to list

(define-syntax values->list
  (syntax-rules ()
    ((_ producer)
     (call-with-values
         (lambda () producer)
       (lambda v (apply list v))))))

;;; List to values

(define-syntax list->values
  (syntax-rules ()
    ((_ l)
     (apply values l))))

;;; Pair to 2 values

(define-syntax pair->values
  (syntax-rules ()
    ((_ ?pair)
     (values (car ?pair)
             (cdr ?pair)))))

;;; All values pairs must satisfy the given 2-predicate

(define-syntax pred2?+
  (syntax-rules ()
    ((_ ?pred ?a ?b)
     (let ((la (values->list ?a))
           (lb (values->list ?b)))
      (let recur ((la la)
                  (lb lb))
        (cond
         ((null? la) (if (null? lb) #t #f))
         ((null? lb) (if (null? la) #t #f))
         (else
          (and (?pred (car la) (car lb))
               (recur (cdr la)
                      (cdr lb))))))))))

;;; All values pairs must satisfy eq?

(define-syntax eq?+
  (syntax-rules ()
    ((_ ?a ?b)
     (pred2?+ eq? ?a ?b))))

;;; All values pairs must satisfy eqv?

(define-syntax eqv?+
  (syntax-rules ()
    ((_ ?a ?b)
     (pred2?+ eqv? ?a ?b))))

;;; All values pairs must satisfy equal?

(define-syntax equal?+
  (syntax-rules ()
    ((_ ?a ?b)
     (pred2?+ equal? ?a ?b))))

;;; Number of values produced

(define-syntax values-length
  (syntax-rules ()
    ((_ producer)
     (call-with-values
         (lambda () producer)
       (lambda v (length v))))))

;;; Extract only the nth-value from a function returning multiple values

(define-syntax values-ref
  (syntax-rules ()
    ((_ n producer)
     (call-with-values
         (lambda () producer)
       (lambda v (list-ref v n))))))

