;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Basic profiling

;;; Explicit definition

(##define-macro (%define-timed name+args . body)
  (if (list? name+args)
      `(##define ,name+args
         (%%accum-time ',(car name+args)
                       (lambda ()
                         ,@body)))
      `(##define ,name+args ,@body)))

;;; Activate implicit definition

(##define-macro (%activate-profiling)
  '(define-macro (define name+args . body)
     (if (list? name+args)
         `(##define ,name+args
            (%%accum-time ',(car name+args)
                          (lambda ()
                            ,@body)))
         `(##define ,name+args ,@body))))
