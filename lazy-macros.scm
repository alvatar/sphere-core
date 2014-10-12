;;!!! SRFI-45
;; Primitives for Expressing Iterative Lazy Algorithms
;; Reference implementation by André van Tonder

;;! lazy
(define-syntax lazy
  (syntax-rules ()
    ((lazy exp)
     (box (cons 'lazy (lambda () exp))))))

;;! delay
(define-syntax delay
  (syntax-rules ()
    ((delay exp) (lazy (eager exp)))))

