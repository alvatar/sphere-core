(include "src/internal/base-macros.scm")
(include "src/internal/sphere-macros.scm")

(define modules '((base: ffi)
                  (base: repl-server)
                  (base: debug/debuggee)))

(define prelude-system-path "~~spheres/prelude-macros.scm")

(define-task compile ()
  ;; Compile both with and without debugging options
  (for-each (lambda (m)
              (sake:compile-c-to-o (sake:compile-to-c m))
              (sake:compile-c-to-o (sake:compile-to-c
                                    m
                                    version: '(debug)
                                    compiler-options: '(debug))))
            modules))

(define-task clean ()
  (sake:default-clean))

(define-task install ()
  ;; Install prelude directly in the spheres directory
  (copy-file "src/prelude-macros.scm"
             prelude-system-path)
  ;; Install compiled module files
  (for-each (lambda (m)
              (sake:install-compiled-module m)
              (sake:install-compiled-module m version: '(debug)))
            modules)
  (sake:install-system-sphere))

(define-task uninstall ()
  (sake:uninstall-system-sphere)
  (delete-file prelude-system-path))

(define-task all (compile install)
  'all)
