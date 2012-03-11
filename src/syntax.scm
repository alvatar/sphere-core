;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Unhygienic anaphoric if

;; (define-macro (uif arg1 . rest-args)
;;   (case (length rest-args)
;;     ((1)
;;      `(let ((?it ,arg1))
;;         (if ?it
;;             ,(car rest-args)
;;             #f)))
;;     ((2)
;;      `(let ((?it ,arg1))
;;         (if ?it
;;             ,(car rest-args)
;;             ,(cadr rest-args))))
;;     ((3)
;;      `(let ((?it ,(car rest-args)))
;;         (if ,(arg1 ?it)
;;             (cadr rest-args)
;;             (caddr rest-args))))
;;     (else
;;      (error "too many arguments passed to unhygienic anaphoric if"))))

;;; R5RS standard states that an if with only one branch returns an unspecified
;;; value if the test is false. This macro places an #f automatically

;; (define-syntax when
;;   (syntax-rules ()
;;     ((_ ?pred ?form . ?forms)
;;      (if ?pred (begin ?form . ?forms) #f))))

;;; Unless

;; (define-syntax unless
;;   (syntax-rules ()
;;     ((_ ?test ?form . ?forms)
;;      (if ?test #f (begin ?form . ?forms)))))
(define-macro (unless ?test ?form1 . ?form+)
  `(if ,?test
       #f
       (begin ,?form1 . ,?form+)))

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

;;; Begin returning the value of the first expression

;; (define-syntax begin0
;;   (syntax-rules ()
;;     ((_ ?expr0 ?expr1 ...)
;;      (let ((return ?expr0))
;;        ?expr1 ...
;;        return))))

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
