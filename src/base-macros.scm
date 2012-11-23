;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Basic syntax extensions for Scheme R5RS

;-------------------------------------------------------------------------------
; Macro environment
;-------------------------------------------------------------------------------

(define-macro (eval-in-macro-environment . exprs)
  (if (pair? exprs)
      (eval (if (null? (cdr exprs)) (car exprs) (cons 'begin exprs))
            (interaction-environment))
      #f))

(define-macro (eval-in-macro-environment-no-result . exprs)
  `(eval-in-macro-environment
    ,@exprs
    '(begin)))

(define-macro (define^ pattern . body)
  `(eval-in-macro-environment-no-result
    (##define ,pattern ,@body)))

(define-macro (at-expand-time-and-runtime . exprs)
  (let ((l `(begin ,@exprs)))
    (eval l)
    l))

(define-macro (at-expand-time . expr)
  (eval (cons 'begin expr)))

(define^ (macro-expand expr)
  expr)

;-------------------------------------------------------------------------------
; Syntax extensions
;-------------------------------------------------------------------------------

;;; Mutable increment
(define-macro (++! x) `(set! ,x (fx+ 1 ,x)))

;;; Read-only increment
(define-macro (++ x) `(fx+ 1 ,x))

;;; Mutable decrement
(define-macro (--! x) `(set! ,x (fx- ,x 1)))

;;; Read-only decrement
(define-macro (-- x) `(fx- ,x 1))

;;; Unhygienic anaphoric if
(define-macro (uif arg1 . rest-args)
  (case (length rest-args)
    ((1)
     `(let ((?it ,arg1))
        (if ?it
            ,(car rest-args)
            #f)))
    ((2)
     `(let ((?it ,arg1))
        (if ?it
            ,(car rest-args)
            ,(cadr rest-args))))
    ((3)
     `(let ((?it ,(car rest-args)))
        (if ,(arg1 ?it)
            (cadr rest-args)
            (caddr rest-args))))
    (else
     (error "Too many arguments passed to unhygienic anaphoric if"))))

;;; Hygienic anaphoric if
(define-macro (aif it arg1 . rest-args)
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
     (error "Too many arguments passed to unhygienic anaphoric if"))))

;;; R5RS standard states that an if with only one branch returns an unspecified
;;; value if the test is false. This macro places an #f automatically
(define-macro (when condition . stmts)
  `(and ,condition (begin ,@stmts)))

;;; unless
(define-macro (unless condition . stmts)
  `(or ,condition (begin ,@stmts)))

;;; Execute a sequence of forms and return the result of the _first_ one.
;;; Typically used to evaluate one or more forms with side effects and
;;; return a value that must be computed before
(define-macro (begin0 form . forms)
  (let ((var (gensym)))
    `(let ((,var ,form)) ,@forms ,var)))

;;; push!
(define-macro (push! list obj)
  `(set! ,list (cons ,obj ,list)))

;;; string-null?
(define-macro (string-null? str) `(zero? (string-length ,str)))

; Like let* but allowing for multiple-value bindings
(define-macro (let-values* bindings . body)
  (if (null? bindings) (cons 'begin body)
      (apply (lambda (vars initializer)
               (let ((cont 
                      (cons 'let-values* 
                            (cons (cdr bindings) body))))
                 (cond
                  ((not (pair? vars)) ; regular let case, a single var
                   `(let ((,vars ,initializer)) ,cont))
                  ((null? (cdr vars))  ; single var, see the prev case
                   `(let ((,(car vars) ,initializer)) ,cont))
                  (else                 ; the most generic case
                   `(call-with-values (lambda () ,initializer)
                      (lambda ,vars ,cont))))))
             (car bindings))))

;;; Pretty-print for values, returning values too
(define-macro (pv form)
  `(call-with-values
     (lambda () ,form)
     (lambda args
       (for-each pp args)
       (apply values args))))

;;; Pretty-print for values, pause execution after (for debugging)
(define-macro (ps form)
  `(call-with-values
     (lambda () ,form)
     (lambda args
       (for-each pp args)
       (step)
       (apply values args))))

;;; Standard error output
(define-macro (cerr . args)
  `(display ,args (current-error-port)))

;;; Letcc macro (hoping and skipping)
;; (define-syntax let/cc
;;   (syntax-rules ()
;;     ((_ c . body)
;;      (call-with-current-continuation
;;        (lambda (c) . body)))))
    
;;; Do a fixed number of times

;; (define-syntax dotimes
;;   (syntax-rules ()
;;     ((_ (var n res) . body)
;;      (do ((limit n)
;;           (var 0 (+ var 1)))
;;          ((>= var limit) res)
;;        . body))
;;     ((_ (var n) . body)
;;      (do ((limit n)
;;           (var 0 (+ var 1)))
;;          ((>= var limit))
;;        . body))))

;;; Define values allows sharing state between functions
;; UNTESTED
;; (define-values (inc dec reset)
;;   (let ((state 0))
;;     (define (inc)  (set! state (+ state 1)) state)
;;     (define (dec)  (set! state (- state 1)) state)
;;     (define (reset)(set! state 0)           state)
;;     (values inc dec reset)))

;; (define-syntax define-values
;;   (syntax-rules ()
;;     ((_ "gentmp" (tmp ...) () (var ...) expr)
;;      (begin (define var (undefined)) ...
;;             (receive (tmp ...) expr
;;                      (set! var tmp) ...
;;                      (undefined))))
;;     ((_ "gentmp" (tmp ...) (v v2 ...) (var ...) expr)
;;      (define-values "gentmp" (tmp ... tmp1) (v2 ...) (var ...) expr))
;;     ((_ (var  ...) expr)
;;      (define-values "gentmp" () (var ...) (var ...) expr))
;;     ((_ . else)
;;      (syntax-error "malformed define-values" (define-values . else)))))


;; Code by Oleg Kiselyov
; assert the truth of an expression (or of a sequence of expressions)
;
; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).
; The user may explicitly specify the expressions whose
; values are to be printed upon assertion failure -- as ?r-exp that
; follow the identifier 'report:'
; Typically, ?r-exp is either a variable or a string constant.
; If the user specified no ?r-exp, the values of variables that are
; referenced in ?expr will be printed upon the assertion failure.

(define-macro (assert expr . others)
    ;; given the list of expressions or vars,
  ;; make the list appropriate for cerr
  (define (make-print-list prefix lst)
    (cond
     ((null? lst) '())
     ((symbol? (car lst))
      (cons #\newline
            (cons (list 'quote (car lst))
                  (cons ": " (cons (car lst) (make-print-list #\newline (cdr lst)))))))
     (else 
      (cons prefix (cons (car lst) (make-print-list "" (cdr lst)))))))
  ;; return the list of all unique "interesting"
  ;; variables in the expr. Variables that are certain
  ;; to be bound to procedures are not interesting.
  (define (vars-of expr)
    (let loop ((expr expr) (vars '()))
      (cond
       ((not (pair? expr)) vars)        ; not an application -- ignore
       ((memq (car expr) 
              '(quote let let* letrec let-values* lambda cond quasiquote
                      case define do assert))
        vars)                     ; won't go there
       (else                      ; ignore the head of the application
        (let inner ((expr (cdr expr)) (vars vars))
          (cond 
           ((null? expr) vars)
           ((symbol? (car expr))
            (inner (cdr expr)
                   (if (memq (car expr) vars) vars (cons (car expr) vars))))
           (else
            (inner (cdr expr) (loop (car expr) vars)))))))))
  (cond
   ((null? others)                      ; the most common case
    `(or ,expr (begin (cerr "failed assertion: " ',expr "\n" "bindings"
                            ,@(make-print-list #\newline (vars-of expr)) "\n")
                      (error "assertion failure"))))
   ((eq? (car others) 'report:)         ; another common case
    `(or ,expr (begin (cerr "failed assertion: " ',expr
                            ,@(make-print-list #\newline (cdr others)) "\n")
                      (error "assertion failure"))))
   ((not (memq 'report: others))
    `(or (and ,expr ,@others)
         (begin (cerr "failed assertion: " '(,expr ,@others) "\n" "bindings"
                      ,@(make-print-list #\newline
                                         (vars-of (cons 'and (cons expr others)))) "\n")
                (error "assertion failure"))))
   (else                        ; report: occurs somewhere in 'others'
    (let loop ((exprs (list expr)) (reported others))
      (cond
       ((eq? (car reported) 'report:)
        `(or (and ,@(reverse exprs))
             (begin (cerr "failed assertion: " ',(reverse exprs)
                          ,@(make-print-list #\newline (cdr reported)) "\n")
                    (error "assertion failure"))))
       (else (loop (cons (car reported) exprs) (cdr reported))))))))
    
(define-macro (assure exp error-msg) `(assert ,exp report: ,error-msg))

;-------------------------------------------------------------------------------
; Utils
;-------------------------------------------------------------------------------

;;; Append strings, keywords or symbols into a symbol
(define^ (macro-append . args)
  (string->symbol
   (apply string-append (map object->string args))))

;;; symbol->keyword
(define^ (symbol->keyword s)
  (string->keyword (symbol->string s)))

;;; keyword->symbol
(define^ (keyword->symbol k)
  (string->symbol (keyword->string k)))

;;; Anything to symbol
(define^ (->symbol o)
  (string->symbol (object->string o)))

;;; Anything to keyword
(define^ (->keyword o)
  (string->keyword (object->string o)))
