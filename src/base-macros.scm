;;; Copyright (c) 2013, Alvaro Castro-Castilla. All rights reserved.
;;; Base syntax extensions for Scheme Spheres

;;------------------------------------------------------------------------------

;;!! Fundamental Macros

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
       command ...)
     (letrec
         ((loop
           (lambda (var ...)
             (if test
                 (begin
                   (if #f #f)
                   expr ...)
                 (begin
                   command
                   ...
                   (loop (do "step" var step ...)
                         ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))


;;------------------------------------------------------------------------------

;;!! SRFIs


;;! SRFI-2 AND-LET*: an AND with local bindings, a guarded LET* special form
;; Ported to syntax-rules by Álvaro Castro-Castilla
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


;;! SRFI-11 Syntax for receiving multiple values
;; Copyright (C) Lars T Hansen (1999). All Rights Reserved.

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

;;! let-values
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
(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () ?body0 ?body1 ...)
     (begin ?body0 ?body1 ...))
    ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
     (let-values (?binding0)
       (let*-values (?binding1 ...) ?body0 ?body1 ...)))))


;;! SRFI-16 Syntax for procedures of variable arity
;; Copyright (C) Lars T Hansen (1999). All Rights Reserved.
;; This code is in the public domain.
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

;;! SRFI-26 Notation for Specializing Parameters without Currying
;; Sebastian.Egner@philips.com, 5-Jun-2002.
;; adapted from the posting by Al Petrofsky <al@petrofsky.org>
;; placed in the public domain.
;; Modifications
;; - Made internal syntaxes private with letrec
;; Copyright (c) Álvaro Castro-Castilla (2012). All Rights Reserved.
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


;;! SRFI-31 A special form rec for recursive evaluation
;; Copyright (C) Dr. Mirko Luedde (2002). All Rights Reserved.
(define-syntax rec
  (syntax-rules ()
    ((rec (?name . ?variables) . ?body)
     (letrec ((?name (lambda ?variables . ?body))) ?name))
    ((rec ?name ?expression)
     (letrec ((?name ?expression)) ?name))))

;;! SRFI-61 A more general cond clause
(define-syntax cond
  (letrec-syntax
      ((cond/maybe-more
        (syntax-rules ()
          ((cond/maybe-more test consequent)
           (if test
               consequent))
          ((cond/maybe-more test consequent clause ...)
           (if test
               consequent
               (cond clause ...))))))
    (syntax-rules (=> else)
      ((_ (else else1 else2 ...))
       ;; the (if #t (begin ...)) wrapper ensures that there may be no
       ;; internal definitions in the body of the clause.  R5RS mandates
       ;; this in text (by referring to each subform of the clauses as
       ;; <expression>) but not in its reference implementation of cond,
       ;; which just expands to (begin ...) with no (if #t ...) wrapper.
       (if #t (begin else1 else2 ...)))
      ((_ (test => receiver) more-clause ...)
       (let ((t test))
         (cond/maybe-more t
                          (receiver t)
                          more-clause ...)))
      ((_ (generator guard => receiver) more-clause ...)
       (call-with-values (lambda () generator)
         (lambda t
           (cond/maybe-more (apply guard    t)
                            (apply receiver t)
                            more-clause ...))))
      ((_ (test) more-clause ...)
       (let ((t test))
         (cond/maybe-more t t more-clause ...)))
      ((_ (test body1 body2 ...) more-clause ...)
       (cond/maybe-more test
                        (begin body1 body2 ...)
                        more-clause ...)))))

;;! SRFI-71
;; Reference implementation of SRFI-71 (srfi-let/*/rec)
;; Copyright (c) Álvaro Castro-Castilla (2012). All Rights Reserved.
;; Sebastian.Egner@philips.com, 20-May-2005, PLT 208
;; Macros used internally are named i:<something>.
;; Abbreviations for macro arguments:
;;   bs  - <binding spec>
;;   b   - component of a binding spec (values, <variable>, or <expression>)
;;   v   - <variable>
;;   vr  - <variable> for rest list
;;   x   - <expression>
;;   t   - newly introduced temporary variable
;;   vx  - (<variable> <expression>)
;;   rec - flag if letrec is produced (and not let)
;;   cwv - call-with-value skeleton of the form (x formals)
;;         (call-with-values (lambda () x) (lambda formals /payload/))
;;         where /payload/ is of the form (let (vx ...) body1 body ...).
;; Remark (*):
;;   We bind the variables of a letrec to i:undefined since there is
;;   no portable (R5RS) way of binding a variable to a values that
;;   raises an error when read uninitialized.

(define-syntax srfi-letrec*             ; -> srfi-letrec
  (syntax-rules ()
    ((srfi-letrec* () body1 body ...)
     (srfi-letrec () body1 body ...))
    ((srfi-letrec* (bs) body1 body ...)
     (srfi-letrec (bs) body1 body ...))
    ((srfi-letrec* (bs1 bs2 bs ...) body1 body ...)
     (srfi-letrec (bs1) (srfi-letrec* (bs2 bs ...) body1 body ...)))))

(define-syntax srfi-letrec              ; -> i:let
  (syntax-rules ()
    ((srfi-letrec ((b1 b2 b ...) ...) body1 body ...)
     (i:let "bs" #t () () (body1 body ...) ((b1 b2 b ...) ...)))))

(define-syntax srfi-let* ; -> srfi-let
  (syntax-rules ()
    ((srfi-let* () body1 body ...)
     (srfi-let () body1 body ...))
    ((srfi-let* (bs) body1 body ...)
     (srfi-let (bs) body1 body ...))
    ((srfi-let* (bs1 bs2 bs ...) body1 body ...)
     (srfi-let (bs1) (srfi-let* (bs2 bs ...) body1 body ...)))))

(define-syntax srfi-let                 ; -> i:let or i:named-let
  (syntax-rules ()
    ((srfi-let ((b1 b2 b ...) ...) body1 body ...)
     (i:let "bs" #f () () (body1 body ...) ((b1 b2 b ...) ...)))
    ((srfi-let tag ((b1 b2 b ...) ...) body1 body ...)
     (i:named-let tag () (body1 body ...) ((b1 b2 b ...) ...)))))

(define-syntax i:let
  (let-syntax ((r5rs-let let)
               (r5rs-letrec letrec))
    (syntax-rules (values)
      ;; (i:let "bs" rec (cwv ...) (vx ...) body (bs ...))
      ;;   processes the binding specs bs ... by adding call-with-values
      ;;   skeletons to cwv ... and bindings to vx ..., and afterwards
      ;;   wrapping the skeletons around the payload (let (vx ...) . body).
      ;; no more bs to process -> wrap call-with-values skeletons
      ((i:let "bs" rec (cwv ...) vxs body ())
       (i:let "wrap" rec vxs body cwv ...))
      ;; recognize form1 without variable -> dummy binding for side-effects
      ((i:let "bs" rec cwvs (vx ...) body (((values) x) bs ...))
       (i:let "bs" rec cwvs (vx ... (dummy (begin x #f))) body (bs ...)))
      ;; recognize form1 with single variable -> just extend vx ...
      ((i:let "bs" rec cwvs (vx ...) body (((values v) x) bs ...))
       (i:let "bs" rec cwvs (vx ... (v x)) body (bs ...)))
      ;; recognize form1 without rest arg -> generate cwv
      ((i:let "bs" rec cwvs vxs body (((values v ...) x) bs ...))
       (i:let "form1" rec cwvs vxs body (bs ...) (x ()) (values v ...)))
      ;; recognize form1 with rest arg -> generate cwv
      ((i:let "bs" rec cwvs vxs body (((values . vs) x) bs ...))
       (i:let "form1+" rec cwvs vxs body (bs ...) (x ()) (values . vs)))
      ;; recognize form2 with single variable -> just extend vx ...
      ((i:let "bs" rec cwvs (vx ...) body ((v x) bs ...))
       (i:let "bs" rec cwvs (vx ... (v x)) body (bs ...)))
      ;; recognize form2 with >=2 variables -> transform to form1
      ((i:let "bs" rec cwvs vxs body ((b1 b2 b3 b ...) bs ...))
       (i:let "form2" rec cwvs vxs body (bs ...) (b1 b2) (b3 b ...)))
      ;; (i:let "form1" rec cwvs vxs body bss (x (t ...)) (values v1 v2 v ...))
      ;;   processes the variables in v1 v2 v ... adding them to (t ...)
      ;;   and producing a cwv when finished. There is not rest argument.
      ((i:let "form1" rec (cwv ...) vxs body bss (x ts) (values))
       (i:let "bs" rec (cwv ... (x ts)) vxs body bss))
      ((i:let "form1" rec cwvs (vx ...) body bss (x (t ...)) (values v1 v ...))
       (i:let "form1" rec cwvs (vx ... (v1 t1)) body bss (x (t ... t1)) (values v ...)))
      ;; (i:let "form1+" rec cwvs vxs body bss (x (t ...)) (values v ... . vr))
      ;;   processes the variables in v ... . vr adding them to (t ...)
      ;;   and producing a cwv when finished. The rest arg is vr.
      ((i:let "form1+" rec cwvs (vx ...) body bss (x (t ...)) (values v1 v2 . vs))
       (i:let "form1+" rec cwvs (vx ... (v1 t1)) body bss (x (t ... t1)) (values v2 . vs)))
      ((i:let "form1+" rec (cwv ...) (vx ...) body bss (x (t ...)) (values v1 . vr))
       (i:let "bs" rec (cwv ... (x (t ... t1 . tr))) (vx ... (v1 t1) (vr tr)) body bss))
      ((i:let "form1+" rec (cwv ...) (vx ...) body bss (x ()) (values . vr))
       (i:let "bs" rec (cwv ... (x tr)) (vx ... (vr tr)) body bss))
      ;; (i:let "form2" rec cwvs vxs body bss (v ...) (b ... x))
      ;;   processes the binding items (b ... x) from form2 as in
      ;;   (v ... b ... x) into ((values v ... b ...) x), i.e. form1.
      ;;   Then call "bs" recursively.
      ((i:let "form2" rec cwvs vxs body (bs ...) (v ...) (x))
       (i:let "bs" rec cwvs vxs body (((values v ...) x) bs ...)))
      ((i:let "form2" rec cwvs vxs body bss (v ...) (b1 b2 b ...))
       (i:let "form2" rec cwvs vxs body bss (v ... b1) (b2 b ...)))
      ;; (i:let "wrap" rec ((v x) ...) (body ...) cwv ...)
      ;;   wraps cwv ... around the payload generating the actual code.
      ;;   For letrec this is of course different than for let.
      ((i:let "wrap" #f vxs body)
       (r5rs-let vxs . body))
      ((i:let "wrap" #f vxs body (x formals) cwv ...)
       (call-with-values
           (lambda () x)
         (lambda formals (i:let "wrap" #f vxs body cwv ...))))
      ((i:let "wrap" #t vxs body)
       (r5rs-letrec vxs . body))
      ((i:let "wrap" #t ((v t) ...) body cwv ...)
       (r5rs-let ((v 'undefined) ...)  ; (*)
                 (i:let "wraprec" ((v t) ...) body cwv ...)))
      ;; (i:let "wraprec" ((v t) ...) body cwv ...)
      ;;   generate the inner code for a letrec. The variables v ...
      ;;   are the user-visible variables (bound outside), and t ... 
      ;;   are the temporary variables bound by the cwv consumers.
      ((i:let "wraprec" ((v t) ...) (body ...))
       (begin (set! v t) ... (r5rs-let () body ...)))
      ((i:let "wraprec" vxs body (x formals) cwv ...)
       (call-with-values
           (lambda () x)
         (lambda formals (i:let "wraprec" vxs body cwv ...)))))))

(define-syntax i:named-let
  (let-syntax ((r5rs-let let))
    (syntax-rules (values)
      ;; (i:named-let tag (vx ...) body (bs ...))
      ;;   processes the binding specs bs ... by extracting the variable
      ;;   and expression, adding them to vx and turning the result into
      ;;   an ordinary named let.
      ((i:named-let tag vxs body ())
       (r5rs-let tag vxs . body))    
      ((i:named-let tag (vx ...) body (((values v) x) bs ...))
       (i:named-let tag (vx ... (v x)) body (bs ...)))
      ((i:named-let tag (vx ...) body ((v x) bs ...))
       (i:named-let tag (vx ... (v x)) body (bs ...))))))

;;! Values to list
(define-syntax values->list
  (syntax-rules ()
    ((_ x)
     (call-with-values (lambda () x) list))))

;;! Values to vector
(define-syntax values->vector
  (syntax-rules ()
    ((_ x)
     (call-with-values (lambda () x) vector))))

;; Extra SRFI-71-related macros

;;! All values pairs must satisfy the given 2-predicate
(define-syntax pred2?+
  (syntax-rules ()
    ((_ ?pred ?a ?b)
     (let ((la (values->list ?a))
           (lb (values->list ?b)))
       (let recur ((la la)
                   (lb lb))
         (cond
          ((null? la) (if (null? lb) #t #f))
          ((null? lb) (if (null? la) #t #f))
          (else
           (and (?pred (car la) (car lb))
                (recur (cdr la)
                       (cdr lb))))))))))

;;! All values pairs must satisfy eq?
(define-syntax eq?+
  (syntax-rules ()
    ((_ ?a ?b)
     (pred2?+ eq? ?a ?b))))

;;! All values pairs must satisfy eqv?
(define-syntax eqv?+
  (syntax-rules ()
    ((_ ?a ?b)
     (pred2?+ eqv? ?a ?b))))

;;! All values pairs must satisfy equal?
(define-syntax equal?+
  (syntax-rules ()
    ((_ ?a ?b)
     (pred2?+ equal? ?a ?b))))

;;! Number of values produced
(define-syntax values-length
  (syntax-rules ()
    ((_ producer)
     (call-with-values
         (lambda () producer)
       (lambda v (length v))))))

;;! Extract only the nth-value from a function returning multiple values
(define-syntax values-ref
  (syntax-rules ()
    ((_ n producer)
     (call-with-values
         (lambda () producer)
       (lambda v (list-ref v n))))))

;;! SRFI-87 => in case clauses
;; Included in Alexpander for native availability

(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else => result))
     (result key))
    ((case key
       ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))


;;------------------------------------------------------------------------------

;;!! Utility Macros

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

;;! Anaphoric if (unhygienic)
;; Note: the caveat that it can't handle single symbol consequents
;; (easy to fix... add two rules to the four argument part of %subst)
;; .author pelpel
;; origin: http://c2.com/cgi/wiki?DefineSyntax
(define-syntax aif!!
  (letrec-syntax
      ((%reverse
        (syntax-rules ()
          ((_ () <result>) <result>)
          ((_ (<hd> . <tl>) <result>)
           (%reverse <tl> (<hd> . <result>)))))
       (%subst  
        (syntax-rules ()
          ;; 1. Three argument form: substitute <new> for all occurrences of <old>
          ;; in <form> 
          ((_ <new> <old> <form>)
           (letrec-syntax
               ((f (syntax-rules (<old>)
                     ;; (1) Substitution complete, reverse the result.
                     ((_ () <result>) (%reverse <result> ()))
                     ;; (2) recurse into sublists (deferred)
                     ((_ ((<hd> . <tl>) . <rest>) <res>)
                      (f <rest> ((f (<hd> . <tl>) ()) . <res>)))
                     ;; (3) These two rules does (substitute <new> <old> ls)
                     ((_ (<old> . <tl>) <res>)
                      (f <tl> (<new> . <res>)))
                     ((_ (<hd> . <tl>) <res>)
                      (f <tl> (<hd> . <res>))))))
             (f <form> ())))
          ;; 2. Four argument form: substitute <new> for all occurrences of <old> in
          ;; <form> but those inside of sublists (<but> ...).  Useful for defining
          ;; macros that can be nested.
          ((_ <new> <old> <form> <but>)
           (letrec-syntax
               ((f (syntax-rules (<old> <but>)
                     ((_ () <result>) (%reverse <result> ()))
                     ;; (4) ignore (<but> ...)
                     ((_ ((<but> . <tl>) . <rest>) <res>)
                      (f <rest> ((<but> . <tl>) . <res>)))
                     ((_ ((<hd> . <tl>) . <rest>) <res>)
                      (f <rest> ((f (<hd> . <tl>) ()) . <res>)))
                     ((_ (<old> . <tl>) <res>)
                      (f <tl> (<new> . <res>)))
                     ((_ (<hd> . <tl>) <res>)
                      (f <tl> (<hd> . <res>))))))
             (f <form> ()))))))
    (syntax-rules ()
      ((_ <condition> <consequent>)
       (let ((temp <condition>))
         (if temp
             (%subst temp it <consequent> aif))))
      ((_ <condition> <consequent> <alternative>)
       (let ((temp <condition>))
         (if temp
             (%subst temp it <consequent> aif)
             <alternative>))))))


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
