;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;; Ported from Tiny Talk. Copyright (c) 2008 by Kenneth A Dicke

(import (srfi 48-format-strings
              64-test)
        ../src/functional
        ../src/prototype
        ../src/value-prototype
        ../src/syntax)

;-------------------------------------------------------------------------------
(test-begin "scheme values prototypes")
;-------------------------------------------------------------------------------

(define (vowel? c) (and (char? c) (memq c (string->list "aeiou")) #t))
(define not-vowel? (negate vowel?))
(define (id x) x) ;; identity

(test-equal ($ join "abc" "def") "abcdef")
(test-equal ($ join (vector 1 2 3) (vector 'a 'b 'c)) (vector 1 2 3 'a 'b 'c))
(test-equal ($ join (vector 1 2 3) (vector 'a 'b 'c)) (vector 1 2 3 'a 'b 'c))

(test-assert ($ every "aeiou" vowel?))
(test-assert ($ every (string->list "aeiou") vowel?))
(test-assert ($ every (list->vector (string->list "aeiou")) vowel?))
(test-equal ($ every "aeiouz" vowel?) #f)
(test-equal ($ every (string->list "aeiouz") vowel?) #f)
(test-equal ($ every (list->vector (string->list "aeiouz")) vowel?) #f)
(test-assert ($ any (list->vector (string->list "aeiou")) vowel?))
(test-equal ($ any (list->vector (string->list "aeiou")) not-vowel?) #f)
(test-assert ($ any (string->list "aeiouz") not-vowel?))
(test-equal ($ any (string->list "aeiou") not-vowel?) #f)

;-------------------------------------------------------------------------------
(test-end "scheme values prototypes")
;-------------------------------------------------------------------------------

;; (add-test-suite 'tt-plus) ;; default-setup-thunk default-teardown-thunk)

;; (add-equal-test 'tt-plus "abcdef" [$ join "abc" "def"])
;; (add-equal-test 'tt-plus (vector 1 2 3 'a 'b 'c)
;;                 [$ join (vector 1 2 3) (vector 'a 'b 'c)])
;; (add-equal-test 'tt-plus (list 1 2 3 'a 'b 'c)
;;                 [$ join '(1 2 3) '(a b c)])
;; (add-eq-test 'tt-plus #t [$ every? "aeiou" vowel?])
;; (add-eq-test 'tt-plus #t [$ every? (string->list "aeiou") vowel?])
;; (add-eq-test 'tt-plus #t [$ every? (list->vector (string->list "aeiou")) vowel?])
;; (add-eq-test 'tt-plus #f [$ every? "aeixou" vowel?])
;; (add-eq-test 'tt-plus #f [$ every? (string->list "aeixou") vowel?])
;; (add-eq-test 'tt-plus #f [$ every? (list->vector (string->list "aeixou")) vowel?])
;; (add-eq-test 'tt-plus #t [$ any? (list->vector (string->list "aeiou")) vowel?])
;; (add-eq-test 'tt-plus #f [$ any? (list->vector (string->list "aeiou")) nv?])
;; (add-eq-test 'tt-plus #t [$ any? (string->list "aeixou") nv?])
;; (add-eq-test 'tt-plus #f [$ any? (string->list "aeiou" ) nv?])
;; (add-eq-test 'tt-plus #t [$ any? "aeixou" nv?])
;; (add-eq-test 'tt-plus #f [$ any? "aeiou"  nv?])
;; (add-eq-test 'tt-plus #t [$ any? "aeixou" nv?])
;; (add-test 'tt-plus "ths s  lttl strng"
;;           [$ reject  "this is a little string" vowel?] string=?)
;; (add-test 'tt-plus "ths s  lttl strng"
;;           [$ collect "this is a little string" nv?]
;;           =?)
;; (add-test 'tt-plus (string->list "ths s  lttl strng")
;;           [$ reject (string->list "this is a little string") vowel?]
;;           equal?)
;; (add-test 'tt-plus (string->list "ths s  lttl strng")
;;           [$ collect (string->list "this is a little string") nv?]
;;           =?)
;; (add-test 'tt-plus (list->vector (string->list "ths s  lttl strng"))
;;           [$ reject  (list->vector (string->list "this is a little string")) vowel?]
;;           =?)
;; (add-test 'tt-plus (list->vector (string->list "ths s  lttl strng"))
;;           [$ collect (list->vector (string->list "this is a little string")) nv?]
;;           =?)
;; (add-test 'tt-plus "123"          [$ map "123"          id] =?)
;; (add-test 'tt-plus (vector 1 2 3) [$ map (vector 1 2 3) id] =?)
;; (add-test 'tt-plus (list   1 2 3) [$ map (list   1 2 3) id] =?)
;; (add-test 'tt-plus "23"         [$ slice "012345"             2 4] =?)
;; (add-test 'tt-plus (vector 2 3) [$ slice (vector 0 1 2 3 4 5) 2 4] =?)
;; (add-test 'tt-plus (list   2 3) [$ slice (list   0 1 2 3 4 5) 2 4] =?)
;; (add-test 'tt-plus 6 [$ length "012345"] =?)
;; (add-test 'tt-plus 6 [$ length (vector 0 1 2 3 4 5)] =?)
;; (add-test 'tt-plus 6 [$ length (list   0 1 2 3 4 5)] =?)
;; (add-test 'tt-plus #\3 [$ iref "012345"             3] =?)
;; (add-test 'tt-plus   3 [$ iref (vector 0 1 2 3 4 5) 3] =?)
;; (add-test 'tt-plus   3 [$ iref (list   0 1 2 3 4 5) 3] =?)
;; (add-test 'tt-plus "123"          [$ shallow-clone "123"] =?)
;; (add-test 'tt-plus (vector 1 2 3) [$ shallow-clone (vector 1 2 3)] =?)
;; (add-test 'tt-plus (list   1 2 3) [$ shallow-clone (list   1 2 3)] =?)

