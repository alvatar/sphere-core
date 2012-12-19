;;; Macros for building complex hygienic macros

;;! Syntax error
(define-syntax syntax-error
  (syntax-rules ()
    ((_) (0))))

;;! Check if given syntax element is an identifier
;; (if-identifier x 'yes 'no) => yes
;; (if-identifier 9 'yes 'no) => no
;; By Al Petrofsky
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

;;! Check if two identifiers are two occurrences of the same identifier
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

;;! Check if two identifiers refer to the same binding
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

;;! Macro tracer
;; Given your macro:
;; (define-syntax test
;;   (syntax-rules ()
;;     ((test name a1 ...) 
;;      (define (name a1 ...) (apply * (list a1 ...))))))
;; we re-write it as follows:
;; (define-syntax test
;;   (syntax-rules ()
;;     ((test name a1 ...) 
;;      (mtrace 
;;       (define (name a1 ...) (apply * (list a1 ...)))))))
;; that is, just enclose the template of the clause of interest in mtrace
;; Here's a more interesting example, of a tracing of a recursive macro,
;; letrec -- taken verbatim from R5RS. We renamed it into rletrec, to
;; avoid name clashes:
;; (define-syntax rletrec1
;;   (syntax-rules ()
;;     ((_ ((var1 init1) ...) body ...)
;;      (rletrec "generate temp names"
;;        (var1 ...)
;;        ()
;;        ((var1 init1) ...)
;;        body ...))
;;     ((_ "generate temp names"
;;        ()
;;        (temp1 ...)
;;        ((var1 init1) ...)
;;        body ...)
;;      (let ((var1 #f) ...)
;;        (let ((temp1 init1) ...)
;;          (set! var1 temp1)
;;          ...
;;          body ...)))
;;     ((_ "generate temp names"
;;        (x y ...)
;;        (temp ...)
;;        ((var1 init1) ...)
;;        body ...)
;;      (rletrec "generate temp names"
;;        (y ...)
;;        (newtemp temp ...)
;;        ((var1 init1) ...)
;;        body ...))))
;; (define-syntax rletrec
;;   (syntax-rules ()
;;     ((rletrec . args)
;;      (mtrace (rletrec1 . args)))))
(define-syntax mtrace
  (syntax-rules ()
    ((mtrace x)
     (begin 
      (display "Trace: ") (write 'x) (newline)
      x))))

