(with-exception-catcher
 (lambda (e)
   (if (unbound-global-exception? e)
       (begin (info/color 'brown "Bootstrapping Base")
              (load "src/alexpander.scm"))))
 (lambda () ##current-expander))
(include "src/sphere#.scm")
(include "src/sake-extensions.scm")

(define modules
  '(ffi))

(define prelude-system-path "~~spheres/prelude#.scm")

(define-task compile ()
  (for-each (lambda (m) (sake:compile-c-to-o (sake:compile-to-c m))) modules))

(define-task test ()
  (sake:test-all))

(define-task clean ()
  (sake:default-clean))

(define-task install ()
  ;; Install compiled module files
  (for-each sake:install-compiled-module modules)
  (sake:install-system-sphere)
  ;; Install prelude directly in the spheres directory
  (copy-file "src/prelude#.scm" prelude-system-path))

(define-task uninstall ()
  (sake:uninstall-system-sphere)
  (delete-file prelude-system-path))

(define-task all (compile install)
  'all)
