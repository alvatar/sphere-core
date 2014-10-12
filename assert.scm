;;!!! Assertion procedures
;; .author Álvaro Castro Castilla
;; All rights reserved, 2014

;; Typed assertions:
;; 
;; (define-type condition extender: define-type-of-condition)
;; (define-type-of-condition compound (contitions read-only:))
;; (define-type-of-condition serious extender: define-type-of-serious)
;; (define-type-of-serious violation extender: define-type-of-violation)
;; (define-type-of-violation assertion constructor: make-assertion-violation)
;; (define-type-of-condition who constructor: make-who-condition (who read-only:))
;; (define-type-of-condition message constructor: make-message-condition (message read-only:))
;; (define-type-of-condition irritants constructor: make-irritants-condition (irritants read-only:))
;; (define condition
;;   (lambda conditions
;;     (make-compound (apply append (map simple-conditions conditions))))) 
;; (define simple-conditions
;;   (lambda (condition)
;;     (cond
;;      ((compound? condition) (compound-conditions condition))
;;      (else (list condition)))))
;; (define (assertion-violation who message . irritants)
;;   (raise 
;;     (if who
;;     (condition (make-who-condition who) 
;;         (make-message-condition message) 
;;         (make-irritants-condition irritants)
;;         (make-assertion-violation))
;;     (condition (make-message-condition message) 
;;         (make-irritants-condition irritants)
;;         (make-assertion-violation)))))


(define-library (https://github.com/alvatar/spheres assert)

  (export assert
          assure
          assertion-violation
          assertion-errors-display)
  (import (gambit))

  (begin

(define-syntax cerr
  (syntax-rules ()
    ((cerr . ?forms)
     (for-each (lambda (f) (if (procedure? f)
                          (f (current-error-port))
                          (display f (current-error-port))))
               (list . ?forms)))))

(define-syntax write-report
  (syntax-rules ()
    ;; given the list of expressions or vars,
    ;; create a cerr form
    ((_ exprs prologue)
     (k!reverse () (cerr . prologue)
                (write-report* ! exprs #\newline)))))

(define-syntax write-report*
  (syntax-rules ()
    ((_ rev-prologue () prefix)
     (k!reverse () ("\n" . rev-prologue) (k!id !)))
    ((_ rev-prologue (x . rest) prefix)
     (symbol?? x
               (write-report* (x ": " 'x #\newline . rev-prologue) 
                              rest #\newline)
               (write-report* (x prefix . rev-prologue) rest "")))))

(define-syntax vars-of 
  (syntax-rules (!)
    ;; return the list of all unique "interesting"
    ;; variables in the expr. Variables that are certain
    ;; to be bound to procedures are not interesting.
    ((_ vars (op . args) (k-head ! . k-args))
     (id-memv?? op 
                (quote let let* letrec let*-values lambda cond quasiquote
                       case define do assert)
                (k-head vars . k-args)  ; won't go inside
                ;; ignore the head of the application
                (vars-of* vars args (k-head ! . k-args))))
    ;; not an application -- ignore
    ((_ vars non-app (k-head ! . k-args)) (k-head vars . k-args))))

(define-syntax vars-of*
  (syntax-rules (!)
    ((_ vars () (k-head ! . k-args)) (k-head vars . k-args))
    ((_ vars (x . rest) k)
     (symbol?? x
               (id-memv?? x vars
                          (vars-of* vars rest k)
                          (vars-of* (x . vars) rest k))
               (vars-of vars x (vars-of* ! rest k))))))

(define-syntax do-assert
  (syntax-rules (report:)
    ((_ () expr)                        ; the most common case
     (do-assert-c expr))
    ((_ () expr report: . others)       ; another common case
     (do-assert-c expr others))
    ((_ () expr . others)
     (do-assert (expr and) . others))
    ((_ exprs)
     (k!reverse () exprs (do-assert-c !)))
    ((_ exprs report: . others)
     (k!reverse () exprs (do-assert-c ! others)))
    ((_ exprs x . others)
     (do-assert (x . exprs) . others))))

(define-syntax do-assert-c
  (syntax-rules ()
    ((_ exprs)
     (or exprs
         (begin (vars-of
                 () exprs
                 (write-report ! 
                               ("failed assertion: " 'exprs "\n" "bindings")))
                (error "assertion failure"))))
    ((_ exprs others)
     (or exprs
         (begin (write-report others
                              ("failed assertion: " 'exprs))
                (error "assertion failure"))))))

(define-syntax assert
  (syntax-rules ()
    ((assert _expr . _others)
     (do-assert () _expr . _others))))

(define-syntax assure
  (syntax-rules ()
    ((assure exp error-msg)
     (assert exp report: error-msg))))
    
;; List-based assertion
(define (assertion-violation who message . irritants)
  (raise
   (if who
       `(assertion-violation in: ,who message: ,message irritants: ,@irritants)
       `(assertion-violation message: ,message irritants: ,@irritants))))

(define (assertion-errors-display . args)
  (for-each (lambda (x)
              (if (procedure? x)
                  (x)
                  (display x (current-error-port))))
            args))
))
