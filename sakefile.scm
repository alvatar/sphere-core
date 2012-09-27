(include "src/prelude#.scm")
(%include sake: utils#)

(define lib-name "base")
(define install-dir (string-append "~~" lib-name))

(define modules '((base: ffi)
                  (base: repl-server)
                  (base: debug/debuggee)))

(define-task init ()
  (make-directory (current-build-directory))
  (make-directory install-dir))

(define-task clean (init)
  (delete-file (default-build-directory))
  (delete-file (default-lib-directory)))

(define-task compile (init)
  ;; Compile both with and without debugging options
  (for-each (lambda (m)
              (sake:compile-c-file (sake:generate-c-file m))
              (sake:compile-c-file (sake:generate-c-file m options: '(debug))))
            modules))

(define-task install (compile)
  ;; Install prelude
  (copy-file (string-append "src/prelude#.scm") "~~base/prelude#.scm")
  ;; Prepare library
  (make-directory (default-lib-directory))
  ;; Install compiled module files
  (for-each (lambda (m)
              (sake:install-compiled-module m)
              (sake:install-compiled-module m features: '(debug)))
            modules))

(define-task uninstall ()
  (delete-file install-dir))

(define-task all (compile)
  'compile)
