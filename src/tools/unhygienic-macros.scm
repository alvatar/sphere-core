;;; Copyright (c) 2014, Alvaro Castro-Castilla. All rights reserved.
;;; Unhygienic macro generation tools for developers

(include "../internal/tiny.scm")

(define (define-macro->rsc-macro-transformer form)
  (let ((declaration (cadr form))
        (body (caddr form))
        (args-counter 0))
    `(define-syntax ,(car declaration)
       (rsc-macro-transformer
        (lambda (form env)
          (let ,(map (lambda (arg)
                       (set! args-counter (+ 1 args-counter))
                       `(,arg (list-ref form ,args-counter)))
                     (cdr declaration))
            ,body))))))

(define (process-file-macros processor file)
  (map
   (lambda (form) (processor form))
   (filter
    (lambda (form) (or (eq? (car form) 'define-macro)
                  (eq? (car form) '##define-macro))) 
    (with-input-from-file file
      (lambda () (read-all))))))

