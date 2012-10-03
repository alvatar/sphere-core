(include "src/internal/syntax-extensions#.scm")
(include "src/internal/sphere#.scm")
(%include sake: utils#)

(define modules '((base: ffi)
                  (base: repl-server)
                  (base: debug/debuggee)))

(define prelude-system-path "~~spheres/prelude#.scm")

(define-task compile ()
  ;; Compile both with and without debugging options
  (for-each (lambda (m)
              (sake:compile-c-file (sake:generate-c-file m))
              (sake:compile-c-file (sake:generate-c-file
                                    m
                                    version: '(debug)
                                    compiler-options: '(debug))))
            modules))

(define-task clean ()
  (delete-file (default-build-directory))
  (delete-file (default-lib-directory)))

(define-task install ()
  ;; Install prelude
  (copy-file "src/prelude#.scm"
             prelude-system-path)
  ;; Install compiled module files
  (for-each (lambda (m)
              (sake:install-compiled-module m)
              (sake:install-compiled-module m version: '(debug)))
            modules)
  (sake:install-system-sphere (%current-sphere)))

(define-task uninstall ()
  (delete-file (%sphere-system-path (%current-sphere)))
  (delete-file prelude-system-path))

(define-task all (compile install)
  'all)
