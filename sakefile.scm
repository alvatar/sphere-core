(define lib-directory "lib/")
(define lib-name "ffi")
(define lib-suffix ".o1")
(define c-suffix ".c")

(define-task init ()
  (make-directory (current-build-directory)))

(define-task clean (init)
  (delete-file (current-build-directory))
  (delete-file lib-directory))

(define-task compile (init)
  (gambit-compile-file
   "module.scm"
   output: (string-append (current-build-directory) lib-name lib-suffix)))

(define-task compile-to-c (init)
  (gambit-eval-here
   `(begin
      (compile-file-to-target
       "module.scm"
       output: ,(string-append (current-build-directory) lib-name c-suffix)))))

(define-task install (compile compile-to-c)
  (make-directory lib-directory)
  (delete-file (string-append lib-directory lib-name lib-suffix))
  (delete-file (string-append lib-directory lib-name c-suffix))
  (copy-file (string-append (current-build-directory) lib-name lib-suffix)
             (string-append lib-directory lib-name lib-suffix))
  (copy-file (string-append (current-build-directory) lib-name c-suffix)
             (string-append lib-directory lib-name c-suffix)))

(define-task all (compile install)
  '(compile and install))
