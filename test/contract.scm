;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

(import (srfi 64-test)
        ../src/lists
        ../src/contract)

;-------------------------------------------------------------------------------
(test-begin "contracts" 9)
;-------------------------------------------------------------------------------

(define·i (inputA a b) (string? string?)
  'ok)

(define·i (inputB a b) (number? (lambda (x) (and (number? x) (> x 1))))
  'ok)

(define·o (outputA a b) ((lambda (x) (> x 9)))
  (* a b))

(define·o (outputB a b) (number? string?)
  (values (* a b)
          "cervantes"))

(define·io (ioA a b) ((number? number?)
                      ((lambda (x) (and (number? x) (> x 9)))
                       symbol?))
  (values (* a b) 'ok))

(define·io (ioB a b) ((number? number?) -> ((lambda (x) (and (number? x) (> x 9)))
                                            symbol?))
  (values (* a b) 'ok))

(test-equal "input ok" (inputA "say" "hello") 'ok)
(test-equal "input ok" (inputB 2 2) 'ok)
(test-error "input bad" (inputB 2 0))
(test-equal "output ok" (outputA 4 4) 16)
(test-error "output bad" (outputA 2 2))
(test-assert "input ok - output ok"
             (equal?+ (ioA 4 4)
                      (values 16 'ok)))
(test-error "input ok - output bad" (ioA 2 2))
(test-error "input bad - output ?" (ioA 2 "hey!"))
(test-error "input ok - output bad" (ioB 2 2))

;-------------------------------------------------------------------------------
(test-end "contracts")
;------------------------------------------------------------------------------
