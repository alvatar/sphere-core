;;------------------------------------------------------------------------------
;; Minimal toolkit for avoiding dependencies but still enjoy some goodies

;; The accumulator represents the rightmost value to tack onto the end of
;; the list, after you've finished recursing down it.
(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

;; The accumulator represents the completed calculation for the leftmost
;; part of the list. Tail-recursive, more efficient than foldr.
(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define (filter pred lst)
  (foldr (lambda (x y) (if (pred x) (cons x y) y))
         '()
         lst))

(define (curry func arg1)
  (lambda (arg) (apply func (cons arg1 arg))))

(define (compose f g)
  (lambda (arg) (f (apply g arg))))

(define (complement f)
  (lambda args (not (apply f args))))

;; Non-tail recursive
(define (quicksort l gt?)
  (if (null? l)
      '()
      (append (quicksort (filter (lambda (x) (gt? (car l) x)) (cdr l)) gt?)
              (list (car l))
              (quicksort (filter (lambda (x) (not (gt? (car l) x))) (cdr l)) gt?))))
