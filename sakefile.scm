(define-task init ()
  (make-directory (current-build-directory)))

(define-task clean (init)
  (delete-file (current-build-directory))
  (delete-file "lib/"))

(define-task compile (init)
  (gambit-compile-file
   "module.scm"
   output: (string-append (current-build-directory) "ffi.o1")))

(define-task install (compile)
  (make-directory "lib")
  (delete-file "lib/ffi.o1")
  (copy-file (string-append (current-build-directory) "ffi.o1") "lib/ffi.o1"))

(define-task all (compile)
  '(compile and install))

