(define lib-directory "lib/")
(define lib-name "ffi")
(define lib-suffix ".o1")

(define-task init ()
  (make-directory (current-build-directory)))

(define-task clean (init)
  (delete-file (current-build-directory))
  (delete-file lib-directory))

(define-task compile (init)
  (gambit-compile-file
   "module.scm"
   output: (string-append (current-build-directory) lib-name lib-suffix)))

(define-task install (compile)
  (make-directory lib-directory)
  (delete-file (string-append lib-directory lib-name lib-suffix))
  (copy-file (string-append (current-build-directory) lib-name lib-suffix)
             (string-append lib-directory lib-name lib-suffix)))

(define-task all (compile install)
  '(compile and install))
