;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Prelude

(define ##current-expander 'alexpander)
(let ((ofile "~~spheres/core/lib/alexpander.o1")
      (scmfile "~~spheres/core/src/alexpander.scm"))
  (if (file-exists? ofile)
      (load ofile)
      (begin (println "--- Loading source version of Alexpander") (load scmfile))))
(include "~~spheres/core/src/sphere#.scm")
