;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Utilities and procedures to be used within sakefiles (needs scheme-base installed)

;;; Parallel for-each, suitable mainly for parallel compilation, which spawns external
;;; processes

(define (sake:parallel-for-each f l #!key (max-thread-number 2))
  (let ((pending-elements l)
        (elements-mutex (make-mutex))
        (results '())
        (results-mutex (make-mutex)))
    (let ((main-thread (current-thread))
          (add-to-results! (lambda (r)
                             (mutex-lock! results-mutex)
                             (set! results (cons r results))
                             (mutex-unlock! results-mutex))))
      (let recur ((n 0)
                  (thread-pool '()))
        (if (< n max-thread-number)
            (recur (++ n)
                   (cons (thread-start!
                          (make-thread
                           (lambda ()
                             (with-exception-catcher
                              (lambda (e) (thread-send main-thread e))
                              (lambda ()
                                (let recur ((n 0))
                                  (mutex-lock! elements-mutex)
                                  (if (null? pending-elements)
                                      (begin (mutex-unlock! elements-mutex)
                                             'finished-thread)
                                      (let ((next (car pending-elements)))
                                        (set! pending-elements (cdr pending-elements))
                                        (mutex-unlock! elements-mutex)
                                        (add-to-results! (f next))
                                        (recur (++ n))))))))))
                         thread-pool)))
        (for-each thread-join! thread-pool)
        (let read-messages ()
          (let ((m (thread-receive 0 'finished)))
            (unless (eq? m 'finished)
                    (pp m)
                    (read-messages))))))
    (reverse results)))

;;; Generate a unique C file from a module or a file
;;; Returns the path of the generated file
;;; version: generate module version with specific features (compiler options, cond-expand...)

(define (sake:compile-to-c module-or-file
                           #!key
                           (library-name "")
                           (version '())
                           (cond-expand-features '())
                           (compiler-options '())
                           (syntax-case #t)
                           (output #f)
                           (verbose #f))
  (or (file-exists? (default-build-directory))
      (make-directory (default-build-directory)))
  (if (string? module-or-file)
      (error "unimplemented")
      (let* ((module module-or-file)
             (version (if (null? version) (%module-version module) version))
             (input-file (string-append (default-src-directory) (%module-filename-scm module)))
             (tmp-file (string-append (default-build-directory)
                                      "_%_"
                                      (%module-flat-name module)
                                      (default-scm-extension)))
             (output-file (or output
                              (string-append (current-build-directory)
                                             (%module-filename-c module version: version)))))
        (info "compiling module to C -- "
              (%module-sphere module)
              ": "
              (%module-id module)
              (if (null? version) "" (string-append " version: " (object->string version))))
        (let* ((generate-cond-expand-code
                (lambda (features)
                  `(define-syntax cond-expand
                     (syntax-rules (and or not else ,@features)
                       ((cond-expand) (syntax-error "Unfulfilled cond-expand"))
                       ((cond-expand (else body ...))
                        (begin body ...))
                       ((cond-expand ((and) body ...) more-clauses ...)
                        (begin body ...))
                       ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
                        (cond-expand
                         (req1
                          (cond-expand
                           ((and req2 ...) body ...)
                           more-clauses ...))
                         more-clauses ...))
                       ((cond-expand ((or) body ...) more-clauses ...)
                        (cond-expand more-clauses ...))
                       ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
                        (cond-expand
                         (req1
                          (begin body ...))
                         (else
                          (cond-expand
                           ((or req2 ...) body ...)
                           more-clauses ...))))
                       ((cond-expand ((not req) body ...) more-clauses ...)
                        (cond-expand
                         (req
                          (cond-expand more-clauses ...))
                         (else body ...)))
                       ,@(map
                          (lambda (cef)
                            `((cond-expand (,cef body ...) more-clauses ...)
                              (begin body ...)))
                          features)
                       ((cond-expand (feature-id body ...) more-clauses ...)
                        (cond-expand more-clauses ...))))))
               ;; (header-module (%module-header module))
               ;; Include # header if available
               ;; ,@(if header-module
               ;;       `((##namespace (,(symbol->string (%module-id header-module))))
               ;;         (##include "~~lib/gambit#.scm"))
               ;;       '())
               ;; Include dependencies
               ;; ,@(map (lambda (m) `(eval '(include ,(string-append (%module-path-src m) (%module-filename-scm m)))))
               ;;        (if header-module
               ;;            (append (%module-dependencies-to-include module) (list header-module))
               ;;            (%module-dependencies-to-include module)))
               ;; filter-map and include the available headers
               ;; ,@((lambda (f l)
               ;;      (let recur ((l l))
               ;;        (if (null? l) '()
               ;;            (aif result (f (car l))
               ;;                 (cons result (recur (cdr l)))
               ;;                 (recur (cdr l))))))
               ;;    (lambda (m)
               ;;      (aif module-header (%module-header m)
               ;;           `(eval '(include ,(string-append (%module-path-src module-header)
               ;;                                            (%module-filename-scm module-header))))))
               ;;    (%module-dependencies-to-load module))
               ;; ,@(if header-module '((##namespace (""))) '())
               (wrapper-code `(,(if syntax-case
                                    (generate-cond-expand-code (cons 'compile-to-c cond-expand-features))
                                    (map (lambda (f) `(define-cond-expand-feature ,f)) cond-expand-features))
                               ,@(map (lambda (m) `(include ,(string-append (%module-path-src m) (%module-filename-scm m))))
                                      (%module-dependencies-to-include module))
                               (include ,(string-append (%module-path-src module) (%module-filename-scm module)))))
               (compiler-code `(,(if syntax-case
                                     '(load "~~lib/syntax-case")
                                     ;; '()
                                     (error "sake:compile-to-c implemented only for syntax-case"))
                                ;; ,(if syntax-case
                                ;;      `(eval ',(generate-cond-expand-code cond-expand-features))
                                ;;      ;; (map (lambda (f) `(define-cond-expand-feature ,f)) cond-expand-features)
                                ;;      (error "sake:compile-to-c implemented only for syntax-case"))
                                ,@(map (lambda (m) `(eval '(include ,(string-append (%module-path-src m) (%module-filename-scm m)))))
                                       (%module-dependencies-to-include module))
                                (compile-file-to-target
                                 ,tmp-file
                                 output: ,output-file
                                 options: ',compiler-options))))
          (call-with-output-file
              tmp-file
            (lambda (f)
              (for-each (lambda (c) (pp c f)) wrapper-code)))
          (if verbose
              (begin (println "Module wrapper code:")
                     (pp wrapper-code)
                     (println "Compiler-code")
                     (pp compiler-code)))
          (unless (= 0
                     (gambit-eval-here
                      compiler-code
                      verbose: #f))
                  (error "error generating C file")))
        output-file)))

;;; Compile a C file

(define (sake:compile-c-to-o c-file
                             #!key
                             (output (path-strip-extension c-file))
                             (cc-options "")
                             (ld-options "")
                             (delete-c #f))
  (info "compiling C file to o -- "
        c-file)
  (unless (= 0
             (gambit-eval-here
              `((compile-file ,c-file output: ,output cc-options: ,cc-options ld-options: ,ld-options))))
          (error "error compiling C file"))
  (if delete-c
      (delete-file c-file recursive: #t)))


;;; Compile to o, for dynamic loading, takes care of introducing 'compile-to-o cond-expand feature
(define (sake:compile-to-o module
                           #!key
                           (library-name "")
                           (version '())
                           (cond-expand-features '())
                           (compiler-options '())
                           (cc-options "")
                           (ld-options "")
                           (output #f)
                           (verbose #f))
  (info "compiling module to o -- "
        (%module-sphere module)
        ": "
        (%module-id module)
        (if (null? version) "" (string-append " version: " (object->string version))))
  (let ((file-already-existed?
         (file-exists? (string-append (current-build-directory)
                                      (%module-filename-c module version: version))))
        (c-file (sake:compile-to-c
                 module
                 library-name: library-name
                 version: version
                 cond-expand-features: (cons 'compile-to-o cond-expand-features)
                 compiler-options: compiler-options
                 verbose: verbose)))
    (sake:compile-c-to-o
     c-file
     output: (unless output (path-strip-extension c-file))
     cc-options: cc-options
     ld-options: ld-options
     delete-c: (not file-already-existed?))))

(define (sake:merge-modules modules #!key (output "merged-modules.scm"))
  (let ((output-path (string-append (current-build-directory) output)))
    (call-with-output-file
        output-path
      (lambda (file)
        (display
         (apply
          string-append
          (map (lambda (m) (string-append "(include \""
                                     (current-source-directory)
                                     (%module-filename-scm m)
                                     "\")\n"))
               modules))
         file)))
    output-path))

;;; Install o and/or C file in the lib/ directory

(define (sake:install-compiled-module m
                                      #!key
                                      (version '())
                                      (omit-o #f)
                                      (omit-c #f))
  (or (file-exists? (default-lib-directory))
      (make-directory (default-lib-directory)))
  (or omit-o
      (copy-file (string-append (default-build-directory) (%module-filename-o m version: version))
                 (string-append (default-lib-directory) (%module-filename-o m version: version))))
  (or omit-c
      (copy-file (string-append (default-build-directory) (%module-filename-c m version: version))
                 (string-append (default-lib-directory) (%module-filename-c m version: version)))))

;;; Clean all default generated files and directories

(define (sake:default-clean)
  (delete-file (current-build-directory) recursive: #t)
  (delete-file (default-lib-directory) recursive: #t))

;;; Install all the files in lib/ in the system directory for the library

(define (sake:install-system-sphere #!optional (sphere (%current-sphere)))
  (delete-file (%sphere-system-path sphere) recursive: #t)
  (make-directory (%sphere-system-path sphere))
  (make-directory (string-append (%sphere-system-path sphere) (default-src-directory)))
  (make-directory (string-append (%sphere-system-path sphere) (default-lib-directory)))
  (copy-files (fileset dir: (default-src-directory) recursive: #f)
              (string-append (%sphere-system-path sphere)
                             (default-src-directory)))
  (copy-files (fileset dir: (default-lib-directory) recursive: #f)
              (string-append (%sphere-system-path sphere)
                             (default-lib-directory)))
  (copy-files '("config.scm")
              (%sphere-system-path sphere)))

;;; Uninstall all the files from the system installation

(define (sake:uninstall-system-sphere #!optional (sphere (%current-sphere)))
  (delete-file (%sphere-system-path sphere) recursive: #t))
