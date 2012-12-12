;;; Copyright (c) 2012 by Álvaro Castro Castilla / Estevo Castro. All Rights Reserved.
;;; Basic contracts functionality

;;; Expand checks only if debug cond-expand feature is defined
(cond-expand
 (debug
;;; Input only contracts
  (define-syntax define/i
    (syntax-rules ()
      ((_ (?proc ?args ...) (?i-contracts ...) . ?exprs)
       (define (?proc ?args ...)
         (if (and (?i-contracts ?args) ...)
             (begin . ?exprs)
             (raise
              (list "input contracts not satisfied"
                    ?proc)))))))

;;; Output only contracts
  (define-syntax define/o
    (syntax-rules ()
      ((_ (?proc args ...) (?o-contracts ...) . ?exprs)
       (define (?proc args ...)
         (call-with-values
             (lambda () . ?exprs)
           (lambda vals
             (if (null? (cdr vals))
                 (let ((res (begin . ?exprs))) ; faster path
                   (if (?o-contracts ... res)
                       res
                       (raise
                        (list "output contracts not satisfied"
                              ?proc))))
                 (let recur ((contr (list ?o-contracts ...))
                             (check-vals vals))
                   (cond
                    ((and (null? contr) (not (null? check-vals)))
                     (error "number of output contracts doesn't match the number of output values"))
                    ((null? check-vals)
                     (apply values vals))
                    (else
                     (if ((car contr) (car check-vals))
                         (recur (cdr contr) (cdr check-vals))
                         (raise
                          (list "output contracts not satisfied"
                                ?proc)))))))))))))

;;; Input/output contracts (allows using -> as a separator as an alternate syntax)
  (define-syntax define/io
    (syntax-rules (->)
      ((_ (?proc ?args ...) ((?i-contracts ...) (?o-contracts ...)) . ?exprs)
       (define·io (?proc ?args ...) ((?i-contracts ...) -> (?o-contracts ...)) . ?exprs))
      ((_ (?proc ?args ...) ((?i-contracts ...) -> (?o-contracts ...)) . ?exprs)
       (define (?proc ?args ...)
         (if (and (?i-contracts ?args) ...)
             (let ((res (begin . ?exprs)))
               (call-with-values
                   (lambda () . ?exprs)
                 (lambda vals
                   (if (null? (cdr vals))
                       (let ((res (begin . ?exprs))) ; faster path
                         (if (?o-contracts ... res)
                             res
                             (raise
                              (list "output contracts not satisfied"
                                    ?proc))))
                       (let recur ((contr (list ?o-contracts ...))
                                   (check-vals vals))
                         (cond
                          ((and (null? contr) (not (null? check-vals)))
                           (error "number of output contracts doesn't match the number of output values"))
                          ((null? check-vals)
                           (apply values vals))
                          (else
                           (if ((car contr) (car check-vals))
                               (recur (cdr contr) (cdr check-vals))
                               (raise
                                (list "output contracts not satisfied"
                                      ?proc))))))))))
             (raise
              (list "input contracts not satisfied"
                    ?proc))))))))
 (else
  (define-syntax define/i
    (syntax-rules ()
      ((_ (?def ...) (?i-contracts ...) ?exprs ...)
       (define (?def ...) ?exprs ...))))
  (define-syntax define/o
    (syntax-rules ()
      ((_ (?def ...) (?o-contracts ...) ?exprs ...)
       (define (?def ...) ?exprs ...))))
  (define-syntax define/io
    (syntax-rules ()
      ((_ (?def ...) ((?i-contracts ...) (?o-contracts ...)) ?exprs ...)
       (define (?def ...) ?exprs ...))))))
