;;; Copyright (c) 2013-2014, Alvaro Castro-Castilla. All rights reserved.
;;; Basic functions for Scheme Spheres

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))

;;!! Return the type of the parameter
;; .parameter Any Scheme object
(define (type object)
  (cond
   ((##structure? object) (##structure-type object))
   ((pair? object) 'pair)
   ((vector? object) 'vector)
   ((table? object) 'table)
   ((symbol? object) 'symbol)
   ((boolean? object) 'boolean)
   ((char? object) 'char)
   ((integer? object) 'integer)
   ((rational? object) 'rational)
   ((real? object) 'real)
   ((complex? object) 'complex)
   ((string? object) 'string)
   ((null? object) 'null)
   ((eof-object? object) 'eof-object)
   ((keyword? object) 'keyword)
   ((eq? object (void)) 'void)
   (else 'unknown)))

;;! symbol->keyword
(define^ (symbol->keyword s)
  (string->keyword (symbol->string s)))

;;! keyword->symbol
(define^ (keyword->symbol k)
  (string->symbol (keyword->string k)))

;;! Anything to symbol
(define^ (->symbol o)
  (string->symbol (object->string o)))

;;! Anything to keyword
(define^ (->keyword o)
  (string->keyword (object->string o)))
