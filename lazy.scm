;;!!! SRFI-45
;; Primitives for Expressing Iterative Lazy Algorithms
;; Reference implementation by André van Tonder

;;! eager
(define (eager x)
  (box (cons 'eager x)))

;;! force
(define (force promise)
  (let ((content (unbox promise)))
    (case (car content)
      ((eager) (cdr content))
      ((lazy)  (let* ((promise* ((cdr content)))        
                      (content  (unbox promise)))
                 (if (not (eqv? (car content) 'eager))
                     (begin (set-car! content (car (unbox promise*)))
                            (set-cdr! content (cdr (unbox promise*)))
                            (set-box! promise* content)))
                 (force promise))))))
