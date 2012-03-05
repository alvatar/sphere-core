;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests for geometry package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (srfi 64-test))
(import ../src/lists)

;-------------------------------------------------------------------------------
(test-begin "list" 21)
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; List/values
;-------------------------------------------------------------------------------

(test-assert
 "eqv?+"
 (let ((str-location "asdf")
       (proc-location (lambda () 'asdf)))
   (eqv?+ (values #t #f 'a #\a 1 0.1 8/9 '() str-location proc-location)
          (values #t #f 'a #\a 1 0.1 8/9 '() str-location proc-location))))

(test-assert
 "eq?+"
 (let ((str-location "asdf")
       (proc-location (lambda () 'asdf)))
   (eq?+ (values #t #f 'a '() str-location proc-location)
         (values #t #f 'a '() str-location proc-location))))

(test-assert
 "equal?+"
 (equal?+ (values #t #f 'a '(a) 1 0.1 8/9 '() "asdf")
          (values #t #f 'a '(a) 1 0.1 8/9 '() "asdf")))

(test-assert
 "pred2?+"
 (pred2?+ >
          (values 2 3)
          (values 1 2)))

;-------------------------------------------------------------------------------
; Map/fold variants
;-------------------------------------------------------------------------------

(test-equal
 "map-cond with only one list (explicit identifiers)"
 (map-cond (a)
           (((number? a)
             (number->string a))
            (else
             (string? a)))
           (list "a" 'b 0 1 2 'z 6))
 (list #t #f "0" "1" "2" #f "6"))

(test-equal
 "map-cond with default 'else' (explicit identifiers)"
 (map-cond (a b)
           (((number? b) (* a b))
            ((or (string? a) (string? b)) 'one-string))
           (list 'r 1 2 3 4 0)
           (list 'b 4 5 6 7 'r))
 (list #f 4 10 18 28 #f))

(test-equal
 "map-cond with default 'else' and returning simple values (explicit identifiers)"
 (map-cond (a b)
           (((number? b) a)
            ((or (string? a) (string? b)) 'one-string))
           (list 'r 1 2 3 4 0)
           (list 'b 4 5 6 7 'r))
 (list #f 1 2 3 4 #f))

(test-equal
 "map-cond with given 'else' (explicit identifiers)"
 (map-cond (a b)
           (((number? b) (* a b))
            ((or (string? a) (string? b)) 'one-string)
            (else a))
           (list 'r 1 2 3 4 0)
           (list 'b 4 5 6 7 "s"))
 (list 'r 4 10 18 28 'one-string))

(test-error
 "map-cond with else at the beginning (explicit identifiers)"
 (test-read-eval-string
  "(map-cond (a) ((else (string? a))
                    ((number? a) (number->string a)) )
            (list 'a 'b 0 1 2 'z 6))"))

(test-error
 "map-cond with else not at the end (explicit identifiers)"
 (test-read-eval-string
  "(map-cond (a) (((number? a) (number->string a))
                    (else (string? a))((number? a) (number->string a))
                    ((number? a) (number->string a)))
            (list 'a 'b 0 1 2 'z 6))"))

(test-equal
 "map-cond with only one list (explicit identifiers)"
 (map-cond (a) (((number? a) (number->string a))
                (else (string? a)))
           (list "a" 'b 0 1 2 'z 6))
 (list #t #f "0" "1" "2" #f "6"))

(test-equal
 "map-cond without else clause (implicit identifiers)"
 (map-cond (((lambda (a b) (number? a)) (lambda (a b) (+ a b)))
            ((lambda (a b) (and (string? a) (string? b))) (lambda (a b) 'was-string)))
           (list 'r 1 1 1 1 "s")
           (list 'b 1 2 3 4 "s"))
 (list #f 2 3 4 5 'was-string))

(test-equal
 "map-cond with else clause (implicit identifiers)"
 (map-cond (((lambda (a b) (number? a)) (lambda (a b) (+ a b)))
            ((lambda (a b) (and (symbol? a) (symbol? b))) (lambda (a b) 'was-symbol))
            (else (lambda (a b) 'was-other)))
           (list 'r 0 1 2 4 "s")
           (list 'b 5 6 7 8 "s"))
 (list 'was-symbol 5 7 9 12 'was-other))

(test-error
 "map-cond with else at the beginning (implicit identifiers)"
 (test-read-eval-string
  "(map-cond ((else string?)
                (number? number->string))
               (list 'a 'b 0 1 2 'z 6))"))

(test-error
 "map-cond with else not at the end (implicit identifiers)"
 (test-read-eval-string
  "(map-cond ((string? string->number)
                (else string?)
                (number? number->string))
               (list 'a 'b 0 1 2 'z 6))"))

(test-equal
 "map-cond with selectors"
 (map-cond ((a <- car)
            (b <- cdr))
           (((< 1 a) a)
            ((zero? a) b))
           (list (cons 1 2) (cons 2 3) (cons 3 4) (cons 0 5)))
 (list #f 2 3 5))

(test-equal
 "map-cond with selectors"
 (map-cond ((a <- car)
            (b <- cdr))
           (((< 1 a) a)
            ((zero? a) b)
            (else 'bingo))
           (list (cons 1 2) (cons 2 3) (cons 3 4) (cons 0 5)))
 (list 'bingo 2 3 5))

;-------------------------------------------------------------------------------
; Map/fold variants
;-------------------------------------------------------------------------------

(test-assert
 "map/values"
 (and
  (equal?+
   (map/values (lambda (x y z) (values x y z)) '(a 1) '(b 2) '(c 3))
   (values '(a b c) '(1 2 3)))
  (equal?+
   (map/values (lambda (x y) (cons x y)) '(a b c) '(1 2 3))
   (values '((a . 1)) '((b . 2)) '((c . 3))))))

(test-assert
 "fold/values"
 (and
  (equal?+
   (fold/values (lambda (x a b) (values (cons (+ 1 x) a) (cons x b))) '(() ()) '(1 2 3 4 5))
   (values '(6 5 4 3 2) '(5 4 3 2 1)))
  (equal?+
   (fold/values (lambda (x a b) (values (cons (car x) a) (cons (cadr x) b))) '(() ()) '((a 1) (b 2) (c 3)))
   (values '(c b a) '(3 2 1)))))

(test-assert
 "demux"
 (and
  (equal?+
   (demux (lambda (x) (values (car x) (cadr x))) '((a 1) (b 2) (c 3)))
   (values '(a b c) '(1 2 3)))
  (equal?+
   (demux (lambda (x y) (values y (cadr x))) '((a 1) (b 2) (c 3)) '(1 2 3 4))
   (values '(1 2 3) '(1 2 3)))))

(test-assert
 "apply/values"
 (and
  (equal?+
   (apply/values (lambda (x y) (cons x y)) (values 'a 'b 'c 'd) (values 1 2 3 4))
   (values '(a . 1) '(b . 2) '(c . 3) '(d . 4)))
  (equal?+
   (apply/values (lambda (x) (* x x)) (values 1 2 3 4))
   (values 1 4 9 16))))

;-------------------------------------------------------------------------------
(test-end "list")
;-------------------------------------------------------------------------------
 