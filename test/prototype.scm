;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;; Ported from Tiny Talk. Copyright (c) 2008 by Kenneth A Dicke

(import (srfi 48-format-strings
              64-test)
        ../src/functional
        ../src/prototype
        ../src/syntax)


(define-prototype-check point?)

(define (new-point #!key (x 0) (y 0))
  ;; 2d, exact-integer values for x,y
  ;; Intent is for pixel position graphics
  (unless (and (integer? x)
               (integer? y))
          (error 'new-point "x and y must be integers" x y))
  (object ((x x) (y y))
          ;;methods
          [(point? self) #t] ;; Yes, this is a point!
          [(name self) 'point]
          [(->string self) (format "(new-point x: ~a y: ~a)" [$ x self] [$ y self])]
          [(add self other)
           (cond
            ((point?  other) (new-point x: (+ [$ x self] [$ x other])
                                        y: (+ [$ y self] [$ y other])))
            ((number? other) (new-point x: (+ [$ x self] other)
                                        y: (+ [$ y self] other)))
            (else (error 'point:add "Can't add self to other" self other)))]
          [(=? self other)
           (unless (point? other)
                   (error 'point:=?
                          "Don't know how compare point to non-point"
                          self other))
           (and (= [$ x self] [$ x other]) (= [$ y self] [$ y other]))]
          [(<? self other)
           (unless (point? other)
                   (error 'point:<?
                          "Don't know how compare point to non-point"
                          self other))
           (and (< [$ x self] [$ x other]) (< [$ y self] [$ y other]))]
          [(>? self other)
           (unless (point? other)
                   (error 'point:>?
                          "Don't know how compare point to non-point"
                          self other))
           (and (> [$ x self] [$ x other]) (> [$ y self] [$ y other]))]
          [(<=? self other)
           (unless (point? other)
                   (error 'point:<=?
                          "Don't know how compare point to non-point"
                          self other))
           (and (<= [$ x self] [$ x other]) (<= [$ y self] [$ y other]))]
          [(>=? self other)
           (unless (point? other)
                   (error 'point:>=?
                          "Don't know how compare point to non-point"
                          self other))
           (and (>= [$ x self] [$ x other]) (>= [$ y self] [$ y other]))]
          [(min-point self other)
           (unless (point? other)
                   (error 'point:min-point
                          "Requires two points"
                          self other))
           (new-point x: (min [$ x self] [$ x other])
                      y: (min [$ y self] [$ y other]))]))

;-------------------------------------------------------------------------------
(test-begin "prototypes")
;-------------------------------------------------------------------------------

(define p0 (new-point x:  0 y:   0))
(define p1 (new-point x: 23 y:  14))
(define p2 (new-point x:  7 y: 123))
(define p3 (new-point x: 17 y:   5))
(define p4 (new-point x: 20 y:  14))
(define (=? a b) ($ =? a b))

($ add-method! p1 'cool? (lambda (self) #t))

(test-equal "(point? 3)" (point? 3) #f)
(test-assert "point:=?" ($ =? p1 (new-point x: ($ x p1) y: ($ y p1))))
(test-equal "point:=? p1 p2" ($ =? p1 p2) #f)
(test-assert "(point? p1)" (point? p1))
(test-error "incorrect arguments with =?" ($ =? p1 3))
(test-error "non-existing method" ($ non-existing-message p1 3))
(test-assert "=?" ($ =? (new-point x: 30 y: 137) ($ add p1 p2)))
(test-assert "<=?" ($ <=? p0 p1))
(test-assert "<?" ($ <? p0 p1))
(test-equal "<?" ($ <? p4 p0) #f)
(test-assert ">=?" ($ >=? p1 p4))
(test-assert "min-point" ($ =? (new-point x: 7 y: 5) ($ min-point p2 p3)))
(test-error "non-existing added method" ($ cool? p0))
(test-assert "added method" ($ cool? p1))

($ add-method! p1 'cool? (lambda (self) #t))
(define p5 ($ shallow-clone p2))

(test-equal "->string" (->string p5) "(new-point x: 7 y: 123)")
(test-equal "get" ($ x p3) 17)
(test-equal "set" ($ x p3 4) 4)
(test-equal "get after set" ($ x p3) 4)

;-------------------------------------------------------------------------------
(test-end "prototypes")
;-------------------------------------------------------------------------------
