;; Macros in the Sake environment are unhygienic, because we don't load the
;; hygienic macros expander
(define-macro (aif . args)
  (let ((it (car args))
        (arg1 (cadr args))
        (rest-args (cddr args)))
    (case (length rest-args)
      ((1)
       `(let ((,it ,arg1))
          (if ,it
              ,(car rest-args)
              #f)))
      ((2)
       `(let ((,it ,arg1))
          (if ,it
              ,(car rest-args)
              ,(cadr rest-args))))
      ((3)
       `(let ((,it ,(car rest-args)))
          (if ,(arg1 ,it)
              (cadr rest-args)
              (caddr rest-args))))
      (else
       (error "Too many arguments passed to unhygienic anaphoric if")))))

(define-macro (when . args)
  (let ((condition (car args))
        (forms (cdr args)))
    `(and ,condition (begin ,@forms))))

(define-macro (unless . args)
  (let ((condition (car args))
        (forms (cdr args)))
    `(or ,condition (begin ,@forms))))

