;;!! Assert macros

;;! Assert the truth of an expression (or of a sequence of expressions)
;; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;; If (and ?expr ?expr ...) evaluates to anything but #f, the result
;; is the value of that expression.
;; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
;; The error message will show the failed expressions, as well
;; as the values of selected variables (or expressions, in general).
;; The user may explicitly specify the expressions whose
;; values are to be printed upon assertion failure -- as ?r-exp that
;; follow the identifier 'report:'
;; Typically, ?r-exp is either a variable or a string constant.
;; If the user specified no ?r-exp, the values of variables that are
;; referenced in ?expr will be printed upon the assertion failure.
;; Based on code by Oleg Kiselyov

;; rsc-macro-transformer version (works with Alexpander)
(cond-expand
 (optimize
  (define-syntax assert
    (syntax-rules ()
      ((_ . ?forms) #f))))
 (else
  (define-macro (assert . args)
    ;; given the list of expressions or vars,
    ;; make the list appropriate for cerr
    (let ((expr (car args))
          (others (cdr args)))
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
           ((not (pair? expr)) vars)    ; not an application -- ignore
           ((memq (car expr) 
                  '(quote let let* letrec let-values* lambda cond quasiquote
                          case define do assert))
            vars)                 ; won't go there
           (else                  ; ignore the head of the application
            (let inner ((expr (cdr expr)) (vars vars))
              (cond 
               ((null? expr) vars)
               ((symbol? (car expr))
                (inner (cdr expr)
                       (if (memq (car expr) vars) vars (cons (car expr) vars))))
               (else
                (inner (cdr expr) (loop (car expr) vars)))))))))
      (cond
       ((null? others)                  ; the most common case
        `(or ,expr (begin (cerr "failed assertion: " ',expr "\n" "bindings"
                                ,@(make-print-list #\newline (vars-of expr)) "\n")
                          (error "assertion failure"))))
       ((eq? (car others) 'report:)     ; another common case
        `(or ,expr (begin (cerr "failed assertion: " ',expr
                                ,@(make-print-list #\newline (cdr others)) "\n")
                          (error "assertion failure"))))
       ((not (memq 'report: others))
        `(or (and ,expr ,@others)
             (begin (cerr "failed assertion: " '(,expr ,@others) "\n" "bindings"
                          ,@(make-print-list #\newline
                                             (vars-of (cons 'and (cons expr others)))) "\n")
                    (error "assertion failure"))))
       (else                    ; report: occurs somewhere in 'others'
        (let loop ((exprs (list expr)) (reported others))
          (cond
           ((eq? (car reported) 'report:)
            `(or (and ,@(reverse exprs))
                 (begin (cerr "failed assertion: " ',(reverse exprs)
                              ,@(make-print-list #\newline (cdr reported)) "\n")
                        (error "assertion failure"))))
           (else (loop (cons (car reported) exprs) (cdr reported)))))))))
  (define-macro (assure exp error-msg) `(assert ,exp report: ,error-msg))))

;; syntax-rules version
;; (define-syntax assert
;;   (let-syntax ((cerr
;;                 (syntax-rules ()
;;                   ((cerr . ?forms)
;;                    (for-each (lambda (f) (if (procedure? f)
;;                                         (f (current-error-port))
;;                                         (display f (current-error-port))))
;;                              (list . ?forms))))))
;;     (syntax-rules ()
;;       ((assert _expr . _others)
;;        (letrec-syntax
;;            ((write-report
;;              (syntax-rules ()
;;                ;; given the list of expressions or vars,
;;                ;; create a cerr form
;;                ((_ exprs prologue)
;;                 (k!reverse () (cerr . prologue)
;;                            (write-report* ! exprs #\newline)))))
;;             (write-report*
;;              (syntax-rules ()
;;                ((_ rev-prologue () prefix)
;;                 (k!reverse () ("\n" . rev-prologue) (k!id !)))
;;                ((_ rev-prologue (x . rest) prefix)
;;                 (symbol?? x
;;                           (write-report* (x ": " 'x #\newline . rev-prologue) 
;;                                          rest #\newline)
;;                           (write-report* (x prefix . rev-prologue) rest "")))))
;;             ;; return the list of all unique "interesting"
;;             ;; variables in the expr. Variables that are certain
;;             ;; to be bound to procedures are not interesting.
;;             (vars-of 
;;              (syntax-rules (!)
;;                ((_ vars (op . args) (k-head ! . k-args))
;;                 (id-memv?? op 
;;                            (quote let let* letrec let*-values lambda cond quasiquote
;;                                   case define do assert)
;;                            (k-head vars . k-args) ; won't go inside
;;                            ;; ignore the head of the application
;;                            (vars-of* vars args (k-head ! . k-args))))
;;                ;; not an application -- ignore
;;                ((_ vars non-app (k-head ! . k-args)) (k-head vars . k-args))))
;;             (vars-of*
;;              (syntax-rules (!)
;;                ((_ vars () (k-head ! . k-args)) (k-head vars . k-args))
;;                ((_ vars (x . rest) k)
;;                 (symbol?? x
;;                           (id-memv?? x vars
;;                                      (vars-of* vars rest k)
;;                                      (vars-of* (x . vars) rest k))
;;                           (vars-of vars x (vars-of* ! rest k))))))
;;             (do-assert
;;              (syntax-rules (report:)
;;                ((_ () expr)             ; the most common case
;;                 (do-assert-c expr))
;;                ((_ () expr report: . others) ; another common case
;;                 (do-assert-c expr others))
;;                ((_ () expr . others)
;;                 (do-assert (expr and) . others))
;;                ((_ exprs)
;;                 (k!reverse () exprs (do-assert-c !)))
;;                ((_ exprs report: . others)
;;                 (k!reverse () exprs (do-assert-c ! others)))
;;                ((_ exprs x . others)
;;                 (do-assert (x . exprs) . others))))
;;             (do-assert-c
;;              (syntax-rules ()
;;                ((_ exprs)
;;                 (or exprs
;;                     (begin (vars-of () exprs
;;                                     (write-report ! 
;;                                                   ("failed assertion: " 'exprs "\n" "bindings")))
;;                            (error "assertion failure"))))
;;                ((_ exprs others)
;;                 (or exprs
;;                     (begin (write-report others
;;                                          ("failed assertion: " 'exprs))
;;                            (error "assertion failure")))))))
;;          (do-assert () _expr . _others))))))
;; (define-syntax assure
;;   (syntax-rules ()
;;     ((assure exp error-msg) (assert exp report: error-msg))))

;; rsc-macro-transformer version
;; (define-syntax assert
;;     ;; given the list of expressions or vars,o make the list appropriate for assertion-errors-display
;;     (rsc-macro-transformer
;;      (lambda (form env)
;;        (let* ((args (cdr form))
;;               (expr (car args))
;;               (others (cdr args)))
;;          (define (make-print-list prefix lst)
;;            (cond
;;             ((null? lst) '())
;;             ((symbol? (car lst))
;;              (cons #\newline
;;                    (cons (list 'quote (car lst))
;;                          (cons ": " (cons (car lst) (make-print-list #\newline (cdr lst)))))))
;;             (else
;;              (cons prefix (cons (car lst) (make-print-list "" (cdr lst)))))))
;;          ;; return the list of all unique "interesting"
;;          ;; variables in the expr. Variables that are certain
;;          ;; to be bound to procedures are not interesting.
;;          (define (vars-of expr)
;;            (let loop ((expr expr) (vars '()))
;;              (cond
;;               ((not (pair? expr)) vars) ; not an application -- ignore
;;               ((memq (car expr) 
;;                      '(quote let let* letrec let-values* lambda cond quasiquote
;;                              case define do assert))
;;                vars)              ; won't go there
;;               (else               ; ignore the head of the application
;;                (let inner ((expr (cdr expr)) (vars vars))
;;                  (cond 
;;                   ((null? expr) vars)
;;                   ((symbol? (car expr))
;;                    (inner (cdr expr)
;;                           (if (memq (car expr) vars) vars (cons (car expr) vars))))
;;                   (else
;;                    (inner (cdr expr) (loop (car expr) vars)))))))))
;;          (cond
;;           ((null? others)               ; the most common case
;;            `(or ,expr (begin (assertion-errors-display "failed assertion: " ',expr "\n" "-- bindings --"
;;                                                        ,@(make-print-list #\newline (vars-of expr)) "\n")
;;                              (error "assertion failure"))))
;;           ((eq? (car others) 'report:)  ; another common case
;;            `(or ,expr (begin (assertion-errors-display "failed assertion: " ',expr
;;                                                        ,@(make-print-list #\newline (cdr others)) "\n")
;;                              (error "assertion failure"))))
;;           ((not (memq 'report: others))
;;            `(or (and ,expr ,@others)
;;                 (begin (assertion-errors-display "failed assertion: " '(,expr ,@others) "\n" "-- bindings --"
;;                                                  ,@(make-print-list #\newline
;;                                                                     (vars-of (cons 'and (cons expr others)))) "\n")
;;                        (error "assertion failure"))))
;;           (else                 ; report: occurs somewhere in 'others'
;;            (let loop ((exprs (list expr)) (reported others))
;;              (cond
;;               ((eq? (car reported) 'report:)
;;                `(or (and ,@(reverse exprs))
;;                     (begin (assertion-errors-display "failed assertion: " ',(reverse exprs)
;;                                                      ,@(make-print-list #\newline (cdr reported)) "\n")
;;                            (error "assertion failure"))))
;;               (else (loop (cons (car reported) exprs) (cdr reported)))))))))))

