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

;;!! Macro Hacks

;; This nasty hack substitutes the '() for ()
;; It turns out that Gambit uses () for argument lists, which is not an acceptable
;; syntax for most syntax-rules expanders
(define-macro (c-lambda #!rest body)
  `(##c-lambda ,@(map (lambda (f) (if (and (pair? f)
                                      (pair? (cdr f))
                                      (eq? (cadr f) '()))
                                 '()
                                 f))
                      body)))

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


;;------------------------------------------------------------------------------

;;!! Optional positional and named parameters in SchemeSpheres

;;! Macro expander for lambda*.
;; .author Álvaro Castro-Castilla, based on SRFI-89 by Marc Feeley
(define-macro (lambda* formals . body)
  (define (keyword->symbol k)
    (string->symbol (keyword->string k)))
  (define (variable? x) (symbol? x))
  (define (required-positional? x)
    (variable? x))
  (define (optional-positional? x)
    (and (pair? x)
         (pair? (cdr x))
         (null? (cddr x))
         (variable? (car x))))
  (define (named-with-default? x)
    (and (pair? x)
         (pair? (cdr x))
         (null? (cddr x))
         (keyword? (car x))))
  (define (named-without-default? x)
    (and (pair? x)
         (null? (cdr x))
         (keyword? (car x))))
  (define (named? x)
    (or (named-with-default? x)
        (named-without-default? x)))
  (define (parse-formals formals)
    (define (duplicates? lst)
      (cond ((null? lst)
             #f)
            ((memq (car lst) (cdr lst))
             #t)
            (else
             (duplicates? (cdr lst)))))
    (define (parse-positional-section lst cont)
      (let loop1 ((lst lst) (rev-reqs '()))
        (if (and (pair? lst)
                 (required-positional? (car lst)))
            (loop1 (cdr lst) (cons (car lst) rev-reqs))
            (let loop2 ((lst lst) (rev-opts '()))
              (if (and (pair? lst)
                       (optional-positional? (car lst)))
                  (loop2 (cdr lst) (cons (car lst) rev-opts))
                  (cont lst (cons (reverse rev-reqs) (reverse rev-opts))))))))
    (define (parse-named-section lst cont)
      (let loop ((lst lst) (rev-named '()))
        (if (and (pair? lst)
                 (named? (car lst)))
            (loop (cdr lst) (cons (car lst) rev-named))
            (cont lst (reverse rev-named)))))
    (define (parse-rest lst
                        positional-before-named?
                        positional-reqs/opts
                        named)
      (if (null? lst)
          (parse-end positional-before-named?
                     positional-reqs/opts
                     named
                     #f)
          (if (variable? lst)
              (parse-end positional-before-named?
                         positional-reqs/opts
                         named
                         lst)
              (error "syntax error in formal parameter list"))))
    (define (parse-end positional-before-named?
                       positional-reqs/opts
                       named
                       rest)
      (let ((positional-reqs (car positional-reqs/opts))
            (positional-opts (cdr positional-reqs/opts)))
        (let ((vars
               (append positional-reqs
                       (map car positional-opts)
                       (map car named)
                       (if rest (list rest) '())))
              (keys
               (map car named)))
          (cond ((duplicates? vars)
                 (error "duplicate variable in formal parameter list"))
                ((duplicates? keys)
                 (error "duplicate keyword in formal parameter list"))
                (else
                 (list positional-before-named?
                       positional-reqs
                       positional-opts
                       named
                       rest))))))

    (define (parse lst)
      (if (and (pair? lst)
               (named? (car lst)))
          (parse-named-section
           lst
           (lambda (lst named)
             (parse-positional-section
              lst
              (lambda (lst positional-reqs/opts)
                (parse-rest lst
                            #f
                            positional-reqs/opts
                            named)))))
          (parse-positional-section
           lst
           (lambda (lst positional-reqs/opts)
             (parse-named-section
              lst
              (lambda (lst named)
                (parse-rest lst
                            #t
                            positional-reqs/opts
                            named)))))))
    (parse formals))
  (define (expand-lambda* formals body)
    (define (range lo hi)
      (if (< lo hi)
          (cons lo (range (+ lo 1) hi))
          '()))
    (define (expand positional-before-named?
                    positional-reqs
                    positional-opts
                    named
                    rest)
      (let ((form
             `(lambda (,@positional-reqs
                  ,@(if (null? positional-opts)
                        '()
                        (cons '#!optional positional-opts))
                  ,@(if (null? named)
                        '()
                        (cons '#!key (map
                                      (lambda (x)
                                        (cond ((named-with-default? x)
                                               (cons (keyword->symbol (car x))
                                                     (cdr x)))
                                              ((named-without-default? x)
                                               (keyword->symbol (car x)))
                                              (else "error generating named parameters")))
                                      named)))
                  ,@(if rest
                        (list '#!rest rest)
                        '()))
                ,@body)))
        ;;(pp form)
        form))
    (apply expand (parse-formals formals)))
  (expand-lambda* formals body))

;;!! Macro expander for define*.
(define-macro (define* pattern . body)
  (if (pair? pattern)
      `(define ,(car pattern)
         (lambda* ,(cdr pattern) ,@body))
      `(define ,pattern ,@body)))

