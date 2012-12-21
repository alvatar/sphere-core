;; Copyright (C) Richard Kelsey, Michael Sperber (2002). All Rights Reserved.

;; Provided by Gambit natively:
;; with-exception-handler
;; raise

;;! guard
;; Examples
;; (guard (condition
;;         (else
;;          (display "condition: ")
;;          (write condition)
;;          (newline)
;;          'exception))
;;        (+ 1 (raise 'an-error)))
;; PRINTS: condition: an-error
;; => exception
;;
;; (guard (condition
;;         (else
;;          (display "something went wrong")
;;          (newline)
;;          'dont-care))
;;        (+ 1 (raise 'an-error)))
;; PRINTS: something went wrong
;; => dont-care
;;
;; (guard (condition
;;         ((assq 'a condition) => cdr)
;;         ((assq 'b condition)))
;;        (raise (list (cons 'a 42))))
;; => 42
;;
;; (guard (condition
;;         ((assq 'a condition) => cdr)
;;         ((assq 'b condition)))
;;        (raise (list (cons 'b 23))))
;; => (b . 23)
(define-syntax guard
  (letrec-syntax
      ((guard-aux
        (syntax-rules (else =>)
          ((guard-aux reraise (else result1 result2 ...))
           (begin result1 result2 ...))
          ((guard-aux reraise (test => result))
           (let ((temp test))
             (if temp 
                 (result temp)
                 reraise)))
          ((guard-aux reraise (test => result) clause1 clause2 ...)
           (let ((temp test))
             (if temp
                 (result temp)
                 (guard-aux reraise clause1 clause2 ...))))
          ((guard-aux reraise (test))
           test)
          ((guard-aux reraise (test) clause1 clause2 ...)
           (let ((temp test))
             (if temp
                 temp
                 (guard-aux reraise clause1 clause2 ...))))
          ((guard-aux reraise (test result1 result2 ...))
           (if test
               (begin result1 result2 ...)
               reraise))
          ((guard-aux reraise (test result1 result2 ...) clause1 clause2 ...)
           (if test
               (begin result1 result2 ...)
               (guard-aux reraise clause1 clause2 ...))))))
    (syntax-rules ()
      ((_ (var clause ...) e1 e2 ...)
       ((call-with-current-continuation
         (lambda (guard-k)
           (with-exception-handler
            (lambda (condition)
              ((call-with-current-continuation
                (lambda (handler-k)
                  (guard-k
                   (lambda ()
                     (let ((var condition)) ; clauses may SET! var
                       (guard-aux (handler-k (lambda ()
                                               (raise condition)))
                                  clause ...))))))))
            (lambda ()
              (call-with-values
                  (lambda () e1 e2 ...)
                (lambda args
                  (guard-k (lambda ()
                             (apply values args))))))))))))))

