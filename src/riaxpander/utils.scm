(define gambit-keyword? keyword?)

;;; I'm not really sure what I'm doing here, but this looks vaguely
;;; plausible.

;(##namespace
; ("riaxpander#"
;  riaxpander:expand
;  riaxpander:expand-toplevel
;  riaxpander:install
;
;  name?
;  name->symbol
;  symbol->name
;  datum->syntax
;  syntax->datum
;  make-syntactic-closure
;  make-syntactic-closures
;  close-syntax
;  close-syntax*
;  ))

(##declare
  (standard-bindings)
;  (extended-bindings)
; (block)
)

(define classify-error error)
(define syntax-error error)
(define (warning . content)
  (pp content (current-output-port)))

(define (append-reverse list tail)
  (if (pair? list)
      (append-reverse (cdr list) (cons (car list) tail))
      tail))

(define (append-map procedure list)
  (if (pair? list)
      (append (procedure (car list))
              (append-map procedure (cdr list)))
      '()))

(define (reduce combiner identity list)
  (if (pair? list)
      (let loop ((result (car list)) (list (cdr list)))
        (if (pair? list)
            (loop (combiner (car list) result) (cdr list))
            result))
      identity))

(define (last list)
  (let ((tail (cdr list)))
    (if (pair? tail)
        (last tail)
        (car list))))

(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (error "null-list?: argument out of domain" l))))

(define (car+cdr pair)
  (values (car pair) (cdr pair)))

(define (%cars+cdrs lists)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
        (if (pair? lists)
	    (receive (list other-lists) (car+cdr lists)
	      (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
		  (receive (a d) (car+cdr list)
		    (receive (cars cdrs) (recur other-lists)
		      (values (cons a cars) (cons d cdrs))))))
	    (values '() '()))))))

(define (filter-map f lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
	(receive (cars cdrs) (%cars+cdrs lists)
	  (if (pair? cars)
	      (cond ((apply f cars) => (lambda (x) (cons x (recur cdrs))))
		    (else (recur cdrs))) ; Tail call in this arm.
	      '())))
	    
      ;; Fast path.
      (let recur ((lis lis1))
	(if (null-list? lis) lis
	    (let ((tail (recur (cdr lis))))
	      (cond ((f (car lis)) => (lambda (x) (cons x tail)))
		    (else tail)))))))
