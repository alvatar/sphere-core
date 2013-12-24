;;; Copyright (c) 2013, Alvaro Castro-Castilla. All rights reserved.
;;; Basic functions for Scheme Spheres


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


