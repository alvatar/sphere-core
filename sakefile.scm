(define lib-directory "lib/")
(define lib-suffix ".o1")
(define c-suffix ".c")
(define modules '("ffi"
                  "repl-server"))

(define-task init ()
  (make-directory (current-build-directory)))

(define-task clean (init)
  (delete-file (current-build-directory))
  (delete-file lib-directory))

(define-task compile (init)
  (for-each
   (lambda (m)
     (let ((module-maker (string-append (current-build-directory) "make-module-" m ".scm")))
       ;; Used for appending external preludes to modules
       (call-with-output-file
           module-maker
         (lambda (file)
           (display
            (string-append
             ;"(include  \"../src/" m "#.scm\")\n"
             "(include  \"../src/" m ".scm\")")
            file)))
       ;; Compile to object
       (gambit-compile-file
        module-maker
        output: (string-append (current-build-directory) m lib-suffix))
       ;; Compile to C
       (gambit-eval-here
        `(begin
           (compile-file-to-target
            ,module-maker
            output: ,(string-append (current-build-directory) m c-suffix))))
       (delete-file module-maker)))
   modules))

(define-task install (compile)
  (make-directory lib-directory)
  (for-each
   (lambda (m)
     (copy-file (string-append (current-build-directory) m lib-suffix)
                (string-append lib-directory m lib-suffix))
     (copy-file (string-append (current-build-directory) m c-suffix)
                (string-append lib-directory m c-suffix)))
   modules))

(define-task all (compile install)
  '(compile and install))
