;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Prelude

(define ##current-expander 'alexpander)
(case ##current-expander
  ((alexpander)
   (println "*** INFO -- Macro expander: Alexpander")
   (load "~~spheres/base/src/alexpander.scm"))
  ((syntax-case)
   (println "*** INFO -- Macro expander: Portable syntax-case")
   (load "~~lib/syntax-case"))
  ((gambit-default)
   (println "*** INFO -- Macro expander: Gambit default"))
  (else
    (error "Unknown macro expander")))
(include "~~spheres/base/src/sphere#.scm")
