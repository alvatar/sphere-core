;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Utilities and procedures to be used within sakefiles (needs scheme-base installed)

;;! Generate a unique C file from a module or a file
;; Shouldn't be used directly, better use sake#compile-module
;; .argument version: generate module version with specific features (compiler options, cond-expand...)
;; .returns the path of the generated file
(##define (sake#compile-to-c module-or-file
                             #!key
                             (cond-expand-features '())
                             (compiler-options '())
                             (version compiler-options)
                             (expander 'riaxpander)
                             (output #f)
                             (verbose #f))
  (or (file-exists? (default-build-directory))
      (make-directory (default-build-directory)))
  (let ((module (if (string? module-or-file)
                    (err "Handling of module as file is unimplemented")
                    module-or-file)))
    (%check-module module 'sake#compile-to-c)
    (let* ((header-module (%module-header module))
           (macros-module (%module-macros module))
           (version (if (null? version) (%module-version module) version))
           (input-file (string-append (%sphere-path (%module-sphere module))
                                      (default-src-directory)
                                      (%module-filename-scm module)))
           (intermediate-file (string-append (default-build-directory)
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
                `((define-syntax syntax-error
                    (syntax-rules ()
                      ((_) (0))))
                  (define-syntax cond-expand
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
                       (cond-expand more-clauses ...))))))))
        (define filter-map (lambda (f l)
                             (let recur ((l l))
                               (if (null? l) '()
                                   (let ((result (f (car l))))
                                     (if result
                                         (cons result (recur (cdr l)))
                                         (recur (cdr l))))))))
        (case expander
          ((riaxpander)
           (let ((compiling-without-riaxpander? (not (null? (%module-shallow-dependencies-to-prelude module)))))
             (let ((compilation-environment-code
                    (if compiling-without-riaxpander?
                        ;; No Riaxpander: no compilation environment
                        '()
                        ;; Riaxpander: Add cond-expand-code and import macros
                        `(,@(generate-cond-expand-code (cons 'compile-to-c cond-expand-features))
                          ,@(map (lambda (m) `(##include-module-and-dependencies ',m ',(if verbose '(verbose) '())))
                                 ;; Import shallow dependencies, as dependencies should already be expanded
                                 (append (%module-shallow-dependencies-to-include module)
                                         ;; (apply append (map %module-shallow-dependencies-to-include
                                         ;;                    (%module-shallow-dependencies-to-load module)))
                                         (if header-module (list header-module) '())
                                         ;; (if macros-module (list macros-module) '())
                                         ))))))
               (if verbose
                   (if compiling-without-riaxpander?
                       (info/color 'green "bypassing the Macro expander, as the module uses prelude")
                       (info/color 'green "expanding macros with Riaxpander")))
               (if (and (not compiling-without-riaxpander?) verbose)
                   (begin
                     (info/color 'light-green "compilation environment code:")
                     (for-each pp compilation-environment-code)))
               ;; Eval compilation code in current environment
               ;; This has been removed in favor of running the code directly in the spawned GSC instance
               ;; (for-each eval compilation-environment-code)
               (let* ((input-code (with-input-from-file input-file read-all))
                      (intermediate-code
                       (if compiling-without-riaxpander?
                           ;; Without Riaxpander
                           `(,@(map (lambda (f)
                                      `(define-cond-expand-feature ,f))
                                    (cons 'compile-to-c cond-expand-features))
                             ,@(map (lambda (p)
                                      `(##include ,(string-append
                                                    (%module-path-src p)
                                                    (%module-filename-scm p))))
                                    ;; Dependencies here are not deep, as they should be already compiled
                                    (%module-shallow-dependencies-to-prelude module))
                             ,@input-code)
                           ;; With Riaxpander
                           `( ;; Compile-time cond-expand-features
                             ,@(map (lambda (f)
                                      `(define-cond-expand-feature ,f))
                                    (cons 'compile-to-c cond-expand-features))
                             ;; If there is a header module set up proper namespace
                             ,@(if header-module
                                   `((##namespace (,(%module-namespace module))))
                                   '())
                             ,@(if header-module
                                   '((##include "~~lib/gambit#.scm"))
                                   '())
                             ;; Include custom compilation preludes defined in config.scm
                             ,@(map (lambda (p)
                                      `(##include ,(string-append
                                                    (%module-path-src p)
                                                    (%module-filename-scm p))))
                                    ;; Dependencies here are not deep, as they should be already compiled
                                    (%module-shallow-dependencies-to-prelude module))
                             ;; Include load dependencies' headers if they have
                             ,@(filter-map
                                (lambda (m) (let ((module-header (%module-header m)))
                                         (and module-header
                                              `(##include ,(string-append
                                                            (%module-path-src module-header)
                                                            (%module-filename-scm module-header))))))
                                ;; Dependencies here are not deep, as they should be already compiled
                                (%module-shallow-dependencies-to-load module))
                             ;; Include header module if we have one
                             ,@(if header-module
                                   `((##include ,(string-append
                                                  (%module-path-src header-module)
                                                  (%module-filename-scm header-module))))
                                   '())
                             ,@input-code))))
                 ;; Verbose compilation
                 (if verbose
                     (begin (info/color 'light-green "code to be compiled:")
                            (for-each pp intermediate-code)))
                 ;; Write code in intermediate file
                 (call-with-output-file
                     intermediate-file
                   (lambda (f) (for-each (lambda (expr) (pp expr f)) intermediate-code)))
                 ;; Compile
                 (or (zero?
                      (gambit-eval-here
                       `(,@compilation-environment-code
                         (or (compile-file-to-target
                              ,intermediate-file
                              output: ,output-file
                              options: ',compiler-options)
                             (exit 1)))
                       flags-string: (if compiling-without-riaxpander? "-f" "")))
                     (err "error compiling generated C file"))))))
          ((gambit)
           (err "Gambit expander workflow not implemented"))
          (else (err "Unknown expander"))))
      output-file)))

;;! Compile a C file generated by Gambit
;; Shouldn't be used directly, better use sake#compile-module
(##define (sake#compile-c-to-o c-file
                               #!key
                               (output (path-strip-extension c-file))
                               (cc-options "")
                               (ld-options "")
                               (delete-c #f))
  (if ((newer-than? output) c-file)
    (begin
      (info "compiling C file to o -- " c-file)
      (or (zero?
           (gambit-eval-here
            `((compile-file ,c-file output: ,output cc-options: ,cc-options ld-options: ,ld-options))))
          (err "error compiling C file"))))

  (if delete-c
      (delete-file c-file recursive: #t)))

;;! Compile to o in one step, through a C intermediary file
(##define (sake#compile-module module
                               #!key
                               (cond-expand-features '())
                               (compiler-options '())
                               (version compiler-options)
                               (expander 'riaxpander)
                               c-output-file
                               o-output-file
                               override-cc-options
                               override-ld-options
                               verbose
                               delete-c)
  (let ((scm-path (string-append
                    (%module-path-src module)
                    (%module-filename-scm module)))
        (default-path (string-append
                        (%module-path-lib module)
                        (%module-filename-c module)))
        (c-file #f))
    (if (not ((newer-than? default-path) scm-path))
        (set! c-file default-path)
        (set! c-file (sake#compile-to-c module
                                   cond-expand-features: cond-expand-features
                                   compiler-options: compiler-options
                                   version: version
                                   expander: expander
                                   output: c-output-file
                                   verbose: verbose)))
    (sake#compile-c-to-o c-file
                         output:
                         (or o-output-file (path-strip-extension c-file))
                         cc-options:
                         (or override-cc-options
                             (%process-cc-options (%module-shallow-dependencies-cc-options module)))
                         ld-options:
                         (or override-ld-options
                             (%process-ld-options (%module-shallow-dependencies-ld-options module)))
                         delete-c: delete-c)))

;;! Compile to exe
(##define (sake#compile-to-exe exe-name
                               modules
                               #!key
                               (version '())
                               (cond-expand-features '())
                               (compiler-options '())
                               override-cc-options
                               override-ld-options
                               (output (string-append (current-build-directory) exe-name))
                               (strip #t)
                               (verbose #f))
  (let ((cc-options (or override-cc-options
                        (%process-cc-options (apply append (map %module-deep-dependencies-cc-options modules)))))
        (ld-options (or override-ld-options
                        (%process-ld-options (apply append (map %module-deep-dependencies-ld-options modules))))))

    (info "compiling modules to exe: ")
    (for-each (lambda (m) (info "    * " (object->string m) "  -> " (object->string (%module-normalize m))))
              modules)
    (let ((c-files (apply
                    append
                    (map (lambda (m)
                           (info "The following dependencies for \033[00;32m"
                                 (object->string (%module-normalize m))
                                 "\033[00m will be linked:")
                           (append (map (lambda (mdep)
                                          (info "    * " (object->string mdep) "")
                                          ;; First try with the default path
                                          (let ((scm-path (string-append
                                                            (%module-path-src mdep)
                                                            (%module-filename-scm mdep)))
                                                (default-path (string-append
                                                                (%module-path-lib mdep)
                                                                (%module-filename-c mdep))))
                                            (if (not ((newer-than? default-path) scm-path))
                                                default-path
                                                (let ((local-path (string-append
                                                                   (current-build-directory)
                                                                   (%module-filename-c mdep))))
                                                  (if (not ((newer-than? local-path) scm-path))
                                                      local-path
                                                      (begin
                                                        (info "Compiling deferred dependency "
                                                              (object->string (%module-normalize mdep)))
                                                        (sake#compile-to-c mdep
                                                                           version: version
                                                                           cond-expand-features: cond-expand-features
                                                                           compiler-options: compiler-options
                                                                           verbose: verbose)))))))
                                        (%module-deep-dependencies-to-load m))
                                   (list (sake#compile-to-c
                                          m
                                          version: version
                                          cond-expand-features: cond-expand-features
                                          compiler-options: compiler-options
                                          verbose: verbose))))
                         modules))))
      (gambit-eval-here
       `((let* ((link-file (link-incremental ',c-files))
                (gcc-cli (string-append ,(c-compiler)
                                        " " ,@(map (lambda (f) (string-append f " ")) c-files)
                                        " " link-file
                                        " -o" ,output
                                        " -I" (path-expand "~~include") " "
                                        ,cc-options
                                        " -L" (path-expand "~~lib") " -lgambc -lm -ldl -lutil "
                                        ,ld-options)))
           (if (not link-file) (err "error generating link file"))
           (if ,verbose (begin (pp link-file) (pp gcc-cli)))
           (shell-command gcc-cli)
           (if ,strip (shell-command ,(string-append "strip " output)))
           (delete-file link-file)))
       flags-string: "-f"))))

;;! Make a module that includes a set of modules
(##define (sake#generate-includer modules #!key (output "merged-modules.scm"))
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

;;! Substitute (include <>) by the code in the referenced file
;; Merges the included files recursively, as S-expressions, but respects
;; the input-file as text, leaving the 
(define (sake#expand-includes input-file output-file)
  (define (do-expansion form)
    (map** (lambda (e) (if (and (pair? e) (eq? (car e) 'include))
                      (begin
                        (current-directory (path-directory (path-expand (cadr e))))
                        (do-expansion `(begin ,@(with-input-from-file (path-strip-directory (cadr e)) read-all))))
                      e))
           form))
  (let* ((str-list (call-with-input-file input-file (lambda (f) (read-all f read-char))))
         (str (list->string str-list))
         (str-len (string-length str)))
    (with-output-to-file
        output-file
      (lambda ()
        (for-each
         write-char
         (let recur ((i 0)
                     (str-rest str-list))
           (cond ((null? str-rest)
                  '())
                 ((and (char=? (car str-rest) #\()
                       (string=? "include " (substring str (+ i 1) (+ i 9)))) ; find include form
                  (receive (continue-position filename)
                           (let parse-filename ((j (+ i 9))
                                                (chars '())
                                                (status 'searching-first))
                             (case status
                               ((searching-first)
                                (if (and (char=? (string-ref str j) #\")
                                         (not (char=? (string-ref str (- j 1)) #\\))) ; make sure the " is not escaped
                                    (parse-filename (+ j 1) chars 'reading)
                                    (parse-filename (+ j 1) chars 'searching-first)))
                               ((reading)
                                (if (and (char=? (string-ref str j) #\")
                                         (not (char=? (string-ref str (- j 1)) #\\))) ; idem
                                    (parse-filename (+ j 1) chars 'search-next-parenthesis)
                                    (parse-filename (+ j 1)
                                                    (cons (string-ref str j)
                                                          chars)
                                                    'reading)))
                               ((search-next-parenthesis)
                                (if (char=? (string-ref str j) #\))
                                    (values (+ j 1)
                                            (list->string (reverse chars)))
                                    (parse-filename (+ j 1) chars 'search-next-parenthesis)))))
                           (append
                            (parameterize
                             ((current-directory (path-directory (path-expand input-file))))
                             (string->list
                              (with-output-to-string
                                '()
                                (lambda () (for-each (lambda (form) (pp form (current-output-port)))
                                                (do-expansion (with-input-from-file filename read-all)))))))
                            (recur continue-position
                                   (drop str-rest (- continue-position i))))))
                 (else
                  (cons (car str-rest)
                        (recur (+ i 1) (cdr str-rest)))))))))))

;;! Install o and/or C file in the lib/ directory
(##define (sake#make-module-available m
                                      #!key
                                      (versions '(()))
                                      (omit-o #f)
                                      (omit-c #f))
  (or (file-exists? (default-lib-directory))
      (make-directory (default-lib-directory)))
  (for-each
   (lambda (version)
     (or omit-o
         (copy-file (string-append (default-build-directory) (%module-filename-o m version: version))
                    (string-append (default-lib-directory) (%module-filename-o m version: version))))
     (or omit-c
         (copy-file (string-append (default-build-directory) (%module-filename-c m version: version))
                    (string-append (default-lib-directory) (%module-filename-c m version: version)))))
   versions))

;;! Test all files in test/
(##define (sake#test-all)
  (for-each (lambda (f)
              (gambit-eval-here
               `((eval '(expander:include ,f)))))
            (fileset dir: "test/"
                     test: (f-and (extension=? ".scm")
                                  (f-not (ends-with? "#.scm")))
                     recursive: #t)))

;;! Test a file
(##define (sake#test module)
  (cond
   ((string? module)
    (if (file-exists? module)
        (gambit-eval-here
         `((eval '(expander:include ,module))))
        (err "Testing file doesn't exist")))
   ((%module? module)
    (%check-module module 'sake#test)
    (gambit-eval-here
     `((eval '(expander:include ,(string-append "test/"
                                                (%module-filename-scm module)))))))
   (else
    (err "Bad testing module description: file path or module"))))

;;! Clean all default generated files and directories
(##define (sake#default-clean)
  (delete-file (current-build-directory) recursive: #t)
  (delete-file (default-lib-directory) recursive: #t))

;;! Install all the files in lib/ in the system directory for the library
(##define (sake#install-sphere-to-system #!key
                                         (extra-directories '())
                                         (sphere (%current-sphere)))
  (delete-file (%sphere-system-path sphere) recursive: #t)
  (make-directory (%sphere-system-path sphere))
  (copy-files '("config.scm" "sakefile.scm")
              (%sphere-system-path sphere))
  (for-each (lambda (dir)
              (make-directory (string-append (%sphere-system-path sphere) dir))
              (copy-files (fileset dir: dir recursive: #f)
                          (string-append (%sphere-system-path sphere) dir)))
            `(,(default-src-directory) ,(default-lib-directory) ,@extra-directories)))

;;! Uninstall all the files from the system installation
(##define (sake#uninstall-sphere-from-system #!optional (sphere (%current-sphere)))
  (delete-file (%sphere-system-path sphere) recursive: #t))

;;! Get the host platform
(define (sake#host-platform)
  (let ((sys (symbol->string (caddr (system-type)))))
    (cond ((and (> (string-length sys) 4)
                (string=? "linux" (substring sys 0 5)))
           'linux)
          ((and (> (string-length sys) 5)
                (string=? "darwin" (substring sys 0 6)))
           'osx)
          (else (err "sake#host-platform -> can't detect current platform")))))

;;! Parallel for-each, suitable mainly for parallel compilation, which spawns external
;; processes
(##define (sake#parallel-for-each f l #!key
                                  (max-thread-number
                                   (case (sake#host-platform)
                                     ((linux)
                                      (string->number
                                       (with-input-from-process "nproc"
                                                                read-line)))
                                     ((osx)
                                      (string->number
                                       (with-input-from-port
                                           (open-process
                                            (list path: "sysctl"
                                                  arguments:
                                                  '("-n" "hw.logicalcpu")))
                                         read-line)))
                                     (else 2))))
  (info "using " max-thread-number " compilation threads")
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
            (recur (+ n 1)
                   (cons (thread-start!
                          (make-thread
                           (lambda ()
                             (let ((current-thread-element #f))
                               (with-exception-catcher
                                (lambda (e)
                                  (thread-send main-thread
                                               (cons e current-thread-element)))
                                (lambda ()
                                  (let recur ((n 0))
                                    (mutex-lock! elements-mutex)
                                    (if (null? pending-elements)
                                        (begin (mutex-unlock! elements-mutex)
                                               'finished-thread)
                                        (let ((next (car pending-elements)))
                                          (set! current-thread-element next)
                                          (set! pending-elements (cdr pending-elements))
                                          (mutex-unlock! elements-mutex)
                                          (add-to-results! (f next))
                                          (recur (fx+ n 1)))))))))))
                         thread-pool)))
        (for-each thread-join! thread-pool)
        (let read-messages ()
          (let ((m (thread-receive 0 'finished)))
            (if (not (eq? m 'finished))
                (begin (err "Exception " (car m) " running procedure on element: " (cdr m))
                       (read-messages)))))))
    (reverse results)))
