;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Macros for building complex hygienic macros

;;; Syntax error
(define-syntax syntax-error
  (syntax-rules ()
    ((_) (0))))

;;; Check if given syntax element is an identifier
;;; (if-identifier x 'yes 'no) => yes
;;; (if-identifier 9 'yes 'no) => no
;;; By Al Petrofsky
(define-syntax if-identifier
  (syntax-rules ()
    ((if-identifier (x . y) consequent alternate) alternate)
    ((if-identifier #(x ...) consequent alternate) alternate)
    ((if-identifier x consequent alternate)
     (let-syntax
         ((test (syntax-rules ()
                  ((test x c a) c)
                  ((test y c a) a))))
       (test foo consequent alternate)))))

;; For the macro id-eq??, two identifiers are equivalent if only if they
;; have the same color, or to put it differently, are two occurrences of
;; the same identifier. In other words, the two identifiers must be
;; inter-changeable at macro-expand time. This is the strictest notion of
;; equivalence.
;; By Oleg Kiselyov
(define-syntax id-eq??
  (syntax-rules ()
    ((id-eq?? id b kt kf)
      (let-syntax
        ((id (syntax-rules ()
               ((id) kf)))
         (ok (syntax-rules ()
               ((ok) kt))))
        (let-syntax
          ((test (syntax-rules ()
                   ((_ b) (id)))))
          (test ok))))))

;; For a macro id-eqv??, the identifiers are equivalent if
;; they refer to the same binding (or both identifiers are unbound and
;; have the same spelling). Thus macro id-eqv?? can find two identifiers
;; equivalent even if they have different colors. The last two test cases
;; show the distinction.
;; By Oleg Kiselyov
(define-syntax id-eqv??
  (syntax-rules ()
    ((id-eqv?? a b kt kf)
      (let-syntax
        ((test (syntax-rules (a)
              ((test a) kt)
              ((test x) kf))))
        (test b)))))
