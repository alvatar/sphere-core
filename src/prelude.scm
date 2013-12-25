(##define-macro (check-arg pred val caller)
  `(let ((pred ,pred)
	 (val ,val)
	 (caller ',caller))
     (if (pred val)
	 val
         (error (string-append (object->string ',pred) " check failed with value "
                               (object->string val)
                               " in: "
                               (object->string ',caller))))))
;;------------------------------------------------------------------------------

;;!! Macro utils

;;!! Define functions for usage in low-level macros (first method)
;; (define^ (f ... ) ... )

(##define-macro (eval-in-macro-environment . exprs)
  (if (pair? exprs)
      (eval (if (null? (cdr exprs)) (car exprs) (cons 'begin exprs))
            (interaction-environment))
      #f))
(##define-macro (eval-in-macro-environment-no-result . exprs)
  `(eval-in-macro-environment
    ,@exprs
    '(begin)))

(##define-macro (define^ . args)
  (let ((pattern (car args))
        (body (cdr args)))
    `(eval-in-macro-environment-no-result
      (##define ,pattern ,@body))))


;;! symbol->keyword
(define^ (symbol->keyword s)
  (string->keyword (symbol->string s)))

;;! keyword->symbol
(define^ (keyword->symbol k)
  (string->symbol (keyword->string k)))

;;! Anything to symbol
(define^ (->symbol o)
  (string->symbol (object->string o)))

;;! Anything to keyword
(define^ (->keyword o)
  (string->keyword (object->string o)))

;;!! Define functions for usage in low-level macros (second method)
;; Insert your defines inside the following macros:
;; (at-expand-time-and-runtime
;;   (define ... )
;;   ... )
;; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2009-August/003781.html

;;! Define for both expand time and runtime
(##define-macro (at-expand-time-and-runtime . exprs)
  (let ((l `(begin ,@exprs)))
    (eval l)
    l))

;;! Define for expand time
(##define-macro (at-expand-time . expr)
  (eval (cons 'begin expr)))

