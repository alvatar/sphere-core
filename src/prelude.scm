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


;; ;;! Macro expander for define*.
;; (##define-macro (define* pattern . body)
;;   (if (pair? pattern)
;;       `(define ,(car pattern)
;;          (lambda* ,(cdr pattern) ,@body))
;;       `(define ,pattern ,@body)))

;; ;;! Macro expander for lambda*.
;; (##define-macro (lambda* formals . body)
;;   (define (parse-formals formals)
;;     ; (define (symbol-has-colon? s)
;;       ; (let ((str (symbol->string s)))
;;         ; (char=? (string-ref str (- (string-length str) 1)) #\:)))
;;     ; (define (variable? x) (and (symbol? x)
;;                                ; (not (symbol-has-colon? x))))
;;     (define variable? symbol?)
;;     ; (define (keyword? x) (and (symbol? x)
;;                               ; (symbol-has-colon? x)))
;;     (define (required-positional? x)
;;       (variable? x))
;;     (define (optional-positional? x)
;;       (and (pair? x)
;;            (pair? (cdr x))
;;            (null? (cddr x))
;;            (variable? (car x))))
;;     (define (required-named? x)
;;       (and (pair? x)
;;            (pair? (cdr x))
;;            (null? (cddr x))
;;            (keyword? (car x))
;;            (variable? (cadr x))))
;;     (define (optional-named? x)
;;       (and (pair? x)
;;            (pair? (cdr x))
;;            (pair? (cddr x))
;;            (null? (cdddr x))
;;            (keyword? (car x))
;;            (variable? (cadr x))))
;;     (define (named? x)
;;       (or (required-named? x)
;;           (optional-named? x)))
;;     (define (duplicates? lst)
;;       (cond ((null? lst)
;;              #f)
;;             ((memq (car lst) (cdr lst))
;;              #t)
;;             (else
;;              (duplicates? (cdr lst)))))
;;     (define (parse-positional-section lst cont)
;;       (let loop1 ((lst lst) (rev-reqs '()))
;;         (if (and (pair? lst)
;;                  (required-positional? (car lst)))
;;             (loop1 (cdr lst) (cons (car lst) rev-reqs))
;;             (let loop2 ((lst lst) (rev-opts '()))
;;               (if (and (pair? lst)
;;                        (optional-positional? (car lst)))
;;                   (loop2 (cdr lst) (cons (car lst) rev-opts))
;;                   (cont lst (cons (reverse rev-reqs) (reverse rev-opts))))))))
;;     (define (parse-named-section lst cont)
;;       (let loop ((lst lst) (rev-named '()))
;;         (if (and (pair? lst)
;;                  (named? (car lst)))
;;             (loop (cdr lst) (cons (car lst) rev-named))
;;             (cont lst (reverse rev-named)))))
;;     (define (parse-rest lst
;;                         positional-before-named?
;;                         positional-reqs/opts
;;                         named)
;;       (if (null? lst)
;;           (parse-end positional-before-named?
;;                      positional-reqs/opts
;;                      named
;;                      #f)
;;           (if (variable? lst)
;;               (parse-end positional-before-named?
;;                          positional-reqs/opts
;;                          named
;;                          lst)
;;               (error "syntax error in formal parameter list"))))
;;     (define (parse-end positional-before-named?
;;                        positional-reqs/opts
;;                        named
;;                        rest)
;;       (let ((positional-reqs (car positional-reqs/opts))
;;             (positional-opts (cdr positional-reqs/opts)))
;;         (let ((vars
;;                (append positional-reqs
;;                        (map car positional-opts)
;;                        (map cadr named)
;;                        (if rest (list rest) '())))
;;               (keys
;;                (map car named)))
;;           (cond ((duplicates? vars)
;;                  (error "duplicate variable in formal parameter list"))
;;                 ((duplicates? keys)
;;                  (error "duplicate keyword in formal parameter list"))
;;                 (else
;;                  (list positional-before-named?
;;                        positional-reqs
;;                        positional-opts
;;                        named
;;                        rest))))))
;;     (define (parse lst)
;;       (if (and (pair? lst)
;;                (named? (car lst)))
;;           (parse-named-section
;;            lst
;;            (lambda (lst named)
;;              (parse-positional-section
;;               lst
;;               (lambda (lst positional-reqs/opts)
;;                 (parse-rest lst
;;                             #f
;;                             positional-reqs/opts
;;                             named)))))
;;           (parse-positional-section
;;            lst
;;            (lambda (lst positional-reqs/opts)
;;              (parse-named-section
;;               lst
;;               (lambda (lst named)
;;                 (parse-rest lst
;;                             #t
;;                             positional-reqs/opts
;;                             named)))))))
;;     (parse formals))
;;   (define (expand-lambda* formals body)
;;     (define (process-named named)
;;       (if (null? (cddr named))
;;           (cadr named)
;;           (cdr named)))
;;     (define (expand positional-before-named?
;;                     positional-reqs
;;                     positional-opts
;;                     named
;;                     rest)
;;       (if (not (null? named)) (error "Named parameters are not supported by syntax-case extension in Gambit"))
;;       (if (and (null? positional-opts) (null? named))
;;           `(lambda ,(append positional-reqs (or rest '())) ,@body)
;;           `(##lambda (,@positional-reqs
;;                       ,@(if positional-opts `(#!optional ,@positional-opts) '())
;;                       ,@(if named `(#!key ,@(map process-named named)))
;;                       ,@(if rest (list #!rest rest) '()))
;;              ,@body)))
;;     (apply expand (parse-formals formals)))
;;   (expand-lambda* formals body))
