;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Basic syntax extensions for Scheme R5RS

;;! Mutable increment
;; Equivalent low-level macro:
;; (define-macro (++! x) `(set! ,x (fx+ 1 ,x)))
(define-syntax ++!
  (syntax-rules ()
    ((_ x)
     (set! x (+ 1 x)))))

;;! Read-only increment
;; Equivalent low-level macro:
;; (define-macro (++ x) `(fx+ 1 ,x))
(define-syntax ++
  (syntax-rules ()
    ((_ x)
     (+ 1 x))))

;;! Mutable decrement
;; Equivalent low-level macro:
;; (define-macro (--! x) `(set! ,x (fx- ,x 1)))
(define-syntax --!
  (syntax-rules ()
    ((_ x)
     (set! x (- x 1)))))

;;! Read-only decrement
;; Equivalent low-level macro:
;; (define-macro (-- x) `(fx- ,x 1))
(define-syntax --
  (syntax-rules ()
    ((_ x)
     (- x 1))))

;;! Hygienic anaphoric if
;; Equivalent low-level macro:
;; (##define-macro (aif . args)
;;   (let ((it (car args))
;;         (arg1 (cadr args))
;;         (rest-args (cddr args)))
;;    (case (length rest-args)
;;      ((1)
;;       `(let ((,it ,arg1))
;;          (if ,it
;;              ,(car rest-args)
;;              #f)))
;;      ((2)
;;       `(let ((,it ,arg1))
;;          (if ,it
;;              ,(car rest-args)
;;              ,(cadr rest-args))))
;;      ((3)
;;       `(let ((,it ,(car rest-args)))
;;          (if ,(arg1 ,it)
;;              (cadr rest-args)
;;              (caddr rest-args))))
;;      (else
;;       (error "Too many arguments passed to unhygienic anaphoric if")))))
(define-syntax aif
  (syntax-rules ()
    ((_ var expr iftrue)
     (let ((var expr))
       (if var
           iftrue
           #f)))
    ((_ var expr iftrue iffalse)
     (let ((var expr))
       (if var
           iftrue
           iffalse)))
    ((_ var pred expr iftrue iffalse)
     (let ((var expr))
       (if (pred var)
           iftrue
           iffalse)))))

;;! when
;; R5RS standard states that an if with only one branch returns an unspecified
;; value if the test is false. This macro places an #f automatically
;; Equivalent low-level macro:
;; (##define-macro (when . args)
;;   (let ((condition (car args))
;;         (forms (cdr args)))
;;     `(and ,condition (begin ,@forms))))
(define-syntax when
  (syntax-rules ()
    ((_ ?condition . ?stmts)
     (and ?condition (begin . ?stmts)))))

;;! unless
;; The opposite of when
;; Equivalent low-level macro:
;; (##define-macro (unless . args)
;;   (let ((condition (car args))
;;         (forms (cdr args)))
;;     `(or ,condition (begin ,@forms))))
(define-syntax unless
  (syntax-rules ()
    ((_ ?test ?form . ?forms)
     (if ?test #f (begin ?form . ?forms)))))

;;! begin0
;; Execute a sequence of forms and return the result of the _first_ one.
;; Typically used to evaluate one or more forms with side effects and
;; return a value that must be computed before
;; Equivalent low-level macro:
;; (##define-macro (begin0 . args)
;;   (let ((form1 (car args))
;;         (rest-forms (cdr args))
;;         (var (gensym)))
;;     `(let ((,var ,form1)) ,@rest-forms ,var)))
(define-syntax begin0
  (syntax-rules ()
    ((_ form form1 ... ) 
     (let ((val form)) form1 ... val))))

;;! push!
;; Prepend an ITEM to a LIST, like a Lisp macro PUSH an ITEM can be an
;; expression, but ls must be a VAR
;; Equivalent low-level macro:
;; (##define-macro (push! list obj)
;;   `(set! ,list (cons ,obj ,list)))
(define-syntax push!
  (syntax-rules ()
    ((_ item ls)
     (set! ls (cons item ls)))))

;;! string-null?
;; Equivalent low-level macro:
;; (##define-macro (string-null? str) `(zero? (string-length ,str)))
(define-syntax string-null?
  (syntax-rules ()
    ((_ str)
     (zero? (string-length str)))))

;;! Like let* but allowing for multiple-value bindings. From SRFI-11
;; Equivalent low-level macro:
;; (define-macro (let-values* . args)
;;   (let ((bindings (car args))
;;         (body (cadr args)))
;;    (if (null? bindings) (cons 'begin body)
;;        (apply (lambda (vars initializer)
;;                 (let ((cont 
;;                        (cons 'let-values* 
;;                              (cons (cdr bindings) body))))
;;                   (cond
;;                    ((not (pair? vars)) ; regular let case, a single var
;;                     `(let ((,vars ,initializer)) ,cont))
;;                    ((null? (cdr vars)) ; single var, see the prev case
;;                     `(let ((,(car vars) ,initializer)) ,cont))
;;                    (else                ; the most generic case
;;                     `(call-with-values (lambda () ,initializer)
;;                        (lambda ,vars ,cont))))))
;;               (car bindings)))))
(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () . bodies) (begin . bodies))
    ((let*-values (((var) initializer) . rest) . bodies)
     (let ((var initializer))           ; a single var optimization
       (let*-values rest . bodies)))
    ((let*-values ((vars initializer) . rest) . bodies)
     (call-with-values (lambda () initializer) ; the most generic case
       (lambda vars (let*-values rest . bodies))))))

;;! Pretty-print for values, returning values too
;; Equivalent low-level macro:
;; (##define-macro (pv form)
;;   `(call-with-values
;;        (lambda () ,form)
;;      (lambda args
;;        (for-each pp args)
;;        (apply values args))))
(define-syntax pv
  (syntax-rules ()
    ((_ ?form)
     (call-with-values
         (lambda () ?form)
       (lambda args
         (for-each pp args)
         (apply values args))))))

;;! Pretty-print for values, pause execution after (for debugging)
;; Equivalent low-level macro:
;; (##define-macro (ps form)
;;   `(call-with-values
;;        (lambda () ,form)
;;      (lambda args
;;        (for-each pp args)
;;        (step)
;;        (apply values args))))
(define-syntax ps
  (syntax-rules ()
    ((_ ?form)
     (call-with-values
         (lambda () ?form)
       (lambda args
         (for-each pp args)
         (step)
         (apply values args))))))

;;! Letcc macro (hoping and skipping)
;; (##define-macro (let/cc . args)
;;   `(call-with-current-continuation
;;     (lambda (,(car args)) ,@(cdr args))))
(define-syntax let/cc
  (syntax-rules ()
    ((_ c . body)
     (call-with-current-continuation
      (lambda (c) . body)))))

;;! Do a fixed number of times
(define-syntax dotimes
  (syntax-rules ()
    ((_ (var n res) . body)
     (do ((limit n)
          (var 0 (+ var 1)))
         ((>= var limit) res)
       . body))
    ((_ (var n) . body)
     (do ((limit n)
          (var 0 (+ var 1)))
         ((>= var limit))
       . body))))


;;!! SRFI-2 AND-LET*: an AND with local bindings, a guarded LET* special form

;;! and-let*
;; Implemented in syntax-rules by Álvaro Castro-Castilla
(define-syntax and-let*
  (syntax-rules ()
    ((_ ())
     #t)
    ((_ () ?body ...)
     (begin ?body ...))
    ((_ ((?expr)))
     ?expr)
    ((_ ((?var ?expr)))
     ?expr)
    ((_ (?expr))
     ?expr)
    ((_ ((?expr) ?clauses ...))
     (let ((var ?expr))
       (if var (and-let* (?clauses ...)) var)))
    ((_ ((?var ?expr) ?clauses ...))
     (let ((?var ?expr))
       (if ?var (and-let* (?clauses ...)) ?var)))
    ((_ ((?var ?expr) ?clauses ...) ?body ...)
     (let ((?var ?expr))
       (if ?var (and-let* (?clauses ...) ?body ...) #f)))
    ((_ ((?expr) ?clauses ...) ?body ...)
     (if ?expr (and-let* (?clauses ...) ?body ...) #f))
    ((_ (?var ?clauses ...) ?body ...)
     (if ?var (and-let* (?clauses ...) ?body ...) #f))))


;;!! SRFI-5 A compatible let form with signatures and rest arguments

;; SRFI-5 Reference implementation
;; Copyright (C) Andy Gaynor (1999). All Rights Reserved.
;; Modifications
;; - Rewritten let-loop as a local syntax definition
;; Copyright (c) Álvaro Castro-Castilla (2012). All Rights Reserved.

(define-syntax let-rest
  (let-syntax
      ((let-loop
        (syntax-rules ()
          ;; Standard binding: destructure and loop.
          ((_ name ((var0 val0) binding ...) (var ...     ) (val ...     ) body)
           (let-loop name (            binding ...) (var ... var0) (val ... val0) body))
          ;; Rest binding, no name: use standard-let, listing the rest values.
          ;; Because of let's first clause, there is no "no bindings, no name" clause.
          ((_ #f (rest-var rest-val ...) (var ...) (val ...) body)
           (let ((var val) ... (rest-var (list rest-val ...))) . body))
          ;; Or call a lambda with a rest parameter on all values.
          ;; ((lambda (var ... . rest-var) . body) val ... rest-val ...))
          ;; Or use one of several other reasonable alternatives.
          ;; No bindings, name: call a letrec'ed lambda.
          ((_ name () (var ...) (val ...) body)
           ((letrec ((name (lambda (var ...) . body)))
              name)
            val ...))
          ;; Rest binding, name: call a letrec'ed lambda.
          ((_ name (rest-var rest-val ...) (var ...) (val ...) body)
           ((letrec ((name (lambda (var ... . rest-var) . body)))
              name)
            val ... rest-val ...)))))
    (syntax-rules ()
      ;; No bindings: use standard-let.
      ((_ () body ...)
       (let () body ...))
      ;; Or call a lambda.
      ;; ((lambda () body ...))
      ;; All standard bindings: use standard-let.
      ((_ ((var val) ...) body ...)
       (let ((var val) ...) body ...))
      ;; Or call a lambda.
      ;; ((lambda (var ...) body ...) val ...)
      ;; One standard binding: loop.
      ;; The all-standard-bindings clause didn't match,
      ;; so there must be a rest binding.
      ((_ ((var val) . bindings) body ...)
       (let-loop #f bindings (var) (val) (body ...)))
      ;; Signature-style name: loop.
      ((_ (name binding ...) body ...)
       (let-loop name (binding ...) () () (body ...)))
      ;; defun-style name: loop.
      ((_ name bindings body ...)
       (let-loop name bindings () () (body ...))))))


;;!! SRFI-11 Syntax for receiving multiple values

;;! let-values
;; Code by Lars T Hansen
(define-syntax let-values
  (syntax-rules ()
    ((let-values (?binding ...) ?body0 ?body1 ...)
     (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))
    ((let-values "bind" () ?tmps ?body)
     (let ?tmps ?body))
    ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
     (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))
    ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
     (call-with-values 
         (lambda () ?e0)
       (lambda ?args
         (let-values "bind" ?bindings ?tmps ?body))))
    ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))
    ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (call-with-values
         (lambda () ?e0)
       (lambda (?arg ... . x)
         (let-values "bind" ?bindings (?tmp ... (?a x)) ?body))))))

;;! let*-values
;; Code by Lars T Hansen
(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () ?body0 ?body1 ...)
     (begin ?body0 ?body1 ...))
    ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
     (let-values (?binding0)
       (let*-values (?binding1 ...) ?body0 ?body1 ...)))))


;;!! SRFI-16 Syntax for procedures of variable arity

;; Copyright (C) Lars T Hansen (1999). All Rights Reserved.
;; This code is in the public domain.

;;! case-lambda
(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda 
      (?a1 ?e1 ...) 
      ?clause1 ...)
     (lambda args
       (let ((l (length args)))
         (case-lambda "CLAUSE" args l 
                      (?a1 ?e1 ...)
                      ?clause1 ...))))
    ((case-lambda "CLAUSE" ?args ?l 
                  ((?a1 ...) ?e1 ...) 
                  ?clause1 ...)
     (if (= ?l (length '(?a1 ...)))
         (apply (lambda (?a1 ...) ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l 
                      ?clause1 ...)))
    ((case-lambda "CLAUSE" ?args ?l
                  ((?a1 . ?ar) ?e1 ...) 
                  ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l 1 (?a1 . ?ar) (?ar ?e1 ...) 
                  ?clause1 ...))
    ((case-lambda "CLAUSE" ?args ?l 
                  (?a1 ?e1 ...)
                  ?clause1 ...)
     (let ((?a1 ?args))
       ?e1 ...))
    ((case-lambda "CLAUSE" ?args ?l)
     (error "Wrong number of arguments to CASE-LAMBDA."))
    ((case-lambda "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
                  ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...) 
                  ?clause1 ...))
    ((case-lambda "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...) 
                  ?clause1 ...)
     (if (>= ?l ?k)
         (apply (lambda ?al ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l 
                      ?clause1 ...)))))

;;!! SRFI-26 Notation for Specializing Parameters without Currying

;; Sebastian.Egner@philips.com, 5-Jun-2002.
;; adapted from the posting by Al Petrofsky <al@petrofsky.org>
;; placed in the public domain.
;; Modifications
;; - Made internal syntaxes private with letrec
;; Copyright (c) Álvaro Castro-Castilla (2012). All Rights Reserved.

;;! cut
(define-syntax cut
  ;; (srfi-26-internal-cut slot-names combination . se)
  ;;   transformer used internally
  ;;     slot-names  : the internal names of the slots
  ;;     combination : procedure being specialized, followed by its arguments
  ;;     se          : slots-or-exprs, the qualifiers of the macro
  (letrec-syntax
      ((srfi-26-internal-cut
        (syntax-rules (<> <...>)
          ;; construct fixed- or variable-arity procedure:
          ;;   (begin proc) throws an error if proc is not an <expression>
          ((srfi-26-internal-cut (slot-name ...) (proc arg ...))
           (lambda (slot-name ...) ((begin proc) arg ...)))
          ((srfi-26-internal-cut (slot-name ...) (proc arg ...) <...>)
           (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))
          ;; process one slot-or-expr
          ((srfi-26-internal-cut (slot-name ...)   (position ...)      <>  . se)
           (srfi-26-internal-cut (slot-name ... x) (position ... x)        . se))
          ((srfi-26-internal-cut (slot-name ...)   (position ...)      nse . se)
           (srfi-26-internal-cut (slot-name ...)   (position ... nse)      . se)))))
    (syntax-rules ()
      ((cut . slots-or-exprs)
       (srfi-26-internal-cut () () . slots-or-exprs)))))

;;! cute
(define-syntax cute
  ;; (srfi-26-internal-cute slot-names nse-bindings combination . se)
  ;;   transformer used internally
  ;;     slot-names     : the internal names of the slots
  ;;     nse-bindings   : let-style bindings for the non-slot expressions.
  ;;     combination    : procedure being specialized, followed by its arguments
  ;;     se             : slots-or-exprs, the qualifiers of the macro
  (letrec-syntax
      ((srfi-26-internal-cute
        (syntax-rules (<> <...>)
          ;; If there are no slot-or-exprs to process, then:
          ;; construct a fixed-arity procedure,
          ((srfi-26-internal-cute
            (slot-name ...) nse-bindings (proc arg ...))
           (let nse-bindings (lambda (slot-name ...) (proc arg ...))))
          ;; or a variable-arity procedure
          ((srfi-26-internal-cute
            (slot-name ...) nse-bindings (proc arg ...) <...>)
           (let nse-bindings (lambda (slot-name ... . x) (apply proc arg ... x))))
          ;; otherwise, process one slot:
          ((srfi-26-internal-cute
            (slot-name ...)         nse-bindings  (position ...)   <>  . se)
           (srfi-26-internal-cute
            (slot-name ... x)       nse-bindings  (position ... x)     . se))
          ;; or one non-slot expression
          ((srfi-26-internal-cute
            slot-names              nse-bindings  (position ...)   nse . se)
           (srfi-26-internal-cute
            slot-names ((x nse) . nse-bindings) (position ... x)       . se)))))
    (syntax-rules ()
      ((cute . slots-or-exprs)
       (srfi-26-internal-cute () () () . slots-or-exprs)))))


;; Utility macro for checking arguments
;; Macro in compilation-prelude to make it easy to define in debug/release modes
;; Original (as function)
;; (define (check-arg pred val caller)
;;   (let lp ((val val))
;;     (if (pred val) val (lp (error "Bad argument" val pred caller)))))
;; (define-syntax check-arg
;;   (syntax-rules ()
;;     ((_ ?pred ?val ?caller)
;;      (if (?pred ?val)
;;          #t
;;          (error (string-append (object->string '?pred) " check failed with value "
;;                                (object->string ?val)
;;                                " in: "
;;                                (object->string '?caller)))))))

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

;-------------------------------------------------------------------------------
; Low-level macros
;-------------------------------------------------------------------------------

;; (##define-macro (eval-in-macro-environment . exprs)
;;   (if (pair? exprs)
;;       (eval (if (null? (cdr exprs)) (car exprs) (cons 'begin exprs))
;;             (interaction-environment))
;;       #f))

;; (##define-macro (eval-in-macro-environment-no-result . exprs)
;;   `(eval-in-macro-environment
;;     ,@exprs
;;     '(begin)))

;; (##define-macro (define^ . args)
;;   (let ((pattern (car args))
;;         (body (cdr args)))
;;     `(eval-in-macro-environment-no-result
;;       (##define ,pattern ,@body))))

;; (##define-macro (at-expand-time-and-runtime . exprs)
;;   (let ((l `(begin ,@exprs)))
;;     (eval l)
;;     l))

;; (##define-macro (at-expand-time . expr)
;;   (eval (cons 'begin expr)))

;; (define^ (macro-expand expr)
;;   expr)

;;; Unhygienic anaphoric if
;; (define-macro (uif . args)
;;   (let ((arg1 (car args))
;;         (rest-args (cdr args)))
;;     (case (length rest-args)
;;       ((1)
;;        `(let ((?it ,arg1))
;;           (if ?it
;;               ,(car rest-args)
;;               #f)))
;;       ((2)
;;        `(let ((?it ,arg1))
;;           (if ?it
;;               ,(car rest-args)
;;               ,(cadr rest-args))))
;;       ((3)
;;        `(let ((?it ,(car rest-args)))
;;           (if ,(arg1 ?it)
;;               (cadr rest-args)
;;               (caddr rest-args))))
;;       (else
;;        (error "Too many arguments passed to unhygienic anaphoric if")))))
