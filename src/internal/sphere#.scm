;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Sphere (module system)

;-------------------------------------------------------------------------------
; Sphere
;-------------------------------------------------------------------------------

(define^ default-src-directory
  (make-parameter "src/"))

(define^ default-build-directory
  (make-parameter "build/"))

(define^ default-lib-directory
  (make-parameter "lib/"))

(define^ default-scm-extension
  (make-parameter ".scm"))

(define^ default-o-extension
  (make-parameter ".o1"))

(define^ default-c-extension
  (make-parameter ".o1.c"))

;;; Read config data
;;; Returns null list if no config data found
(define^ %current-config
  (let ((cached #f))
    (lambda ()
      (or cached
          (begin
            (set! cached
                  (with-exception-catcher
                   (lambda (e) (if (no-such-file-or-directory-exception? e)
                              ;; If config.scm not found try to find global %paths variable
                              (with-exception-catcher
                               (lambda (e2) (if (unbound-global-exception? e2)
                                           '()
                                           (raise e2)))
                               ;; inject global %paths variable if no config.scm found
                               (lambda () `((paths: ,@%system-paths))))
                              (raise e)))
                   (lambda () (call-with-input-file "config.scm" read-all))))
            cached)))))

(define^ (%current-sphere)
  (let ((current-sphere-info (assq sphere: (%current-config))))
    (if current-sphere-info
        (string->symbol (cadr current-sphere-info))
        #f)))

(define^ (%sphere-system-path sphere)
  (string-append "~~spheres/" (symbol->string sphere) "/"))

(define^ (%sphere-path sphere)
  (let ((paths (%paths)))
    (if sphere
        ;; First try with custom paths: in config file
        (uif (assq (string->keyword (symbol->string sphere)) paths)
             (string-append (path-strip-trailing-directory-separator (cadr ?it)) "/")
             ;; Otherwise, try system-installed spheres
             (if (file-exists? (%sphere-system-path sphere))
                 (%sphere-system-path sphere)
                 (error (string-append "Sphere not found: " (object->string sphere) " -- Please set a path in config file or install the sphere in the ~~spheres directory"))))
        #f)))

(define^ (%paths)
  (let ((paths (uif (assq paths: (%current-config))
                    (cdr ?it)
                    '())))
    (uif (%current-sphere)
         (cons
          (list (symbol->keyword ?it) (current-directory))
          paths)
         paths)))

(define^ (%dependencies)
  (uif (assq dependencies: (%current-config))
       (cdr ?it)
       '()))

;;; Used for getting a specific sphere config data
(define^ %sphere-config
  (let ((config-dict '()))
    (lambda (sphere)
      (if sphere
          (uif (assq sphere config-dict)
               (cadr ?it)
               (let ((new-pair (list
                                sphere
                                (with-exception-catcher
                                 (lambda (e) (if (no-such-file-or-directory-exception? e)
                                            (error (string-append "Sphere \"" (symbol->string sphere) "\" doesn't have a con
fig.scm file"))
                                            (raise e)))
                                 (lambda () (call-with-input-file
                                           (string-append (or (%sphere-path sphere) "")
                                                          "config.scm")
                                         read-all))))))
                 (set! config-dict
                       (cons new-pair config-dict))
                 (cadr new-pair)))
          '()))))

;;; Get dependencies of sphere's modules
(define^ (%sphere-dependencies sphere)
  (uif (assq dependencies: (%sphere-config sphere))
       (cdr ?it)
       '()))

;-------------------------------------------------------------------------------
; Module
;-------------------------------------------------------------------------------

;;; Signal error when module has a wrong format
(define^ (module-error module)
  (error "Error parsing module directive (wrong module format): " module))

;;; Module structure: (sphere: module-id [version: '(list-of-version-features)])
(define^ %module-reduced-form? symbol?)

(define^ (%module-normal-form? module)
  (and (list? module)
       (keyword? (car module))
       (not (null? (cdr module)))
       (symbol? (cadr module))))

(define^ (%module? module)
  (or (%module-reduced-form? module)
      (%module-normal-form? module)))

(define^ (%module-sphere module)
  (assure (%module? module) (module-error module))
  (if (%module-normal-form? module)
      (keyword->symbol (car module))
      (%current-sphere)))

(define^ (%module-id module)
  (assure (%module? module) (module-error module))
  (if (%module-normal-form? module)
      (cadr module)
      module))

(define^ (%module-version module)
  (assure (%module? module) (module-error module))
  (if (%module-normal-form? module)
      ;; Search for version: from the third element on
      (let ((version (memq version: (cddr module))))
        (if version
            (cadr version)
            '()))
      '()))

(define^ (%check-module-exists? module)
  ;; TODO: now only checks sphere
  (unless (file-exists? (%sphere-path (%module-sphere module)))
          (error "%check-module-exists?: sphere path can't be found"))
  #;
  (let* ((sphere-path (%sphere-path (%module-sphere module)))
         (check-)
         (check-path-1
          ;; if it's an external sphere, then search in the src directory for C files
          (string-append sphere-path
                         (default-lib-directory)
                         (%module-filename-c module)))
         (check-path-2
          ;; if it's an external sphere, then search in the src directory for object files
          (string-append (unless sphere-path "")
                         (default-lib-directory)
                         (%module-filename-o module)))
         (check-path-3
          ;; if it's a local sphere (no sphere-path), then search in the src directory
          (string-append (unless sphere-path "")
                         (default-src-directory)
                         (%module-filename-scm module))))
    (unless (or (file-exists? check-path-1)
                (file-exists? check-path-2)
                (file-exists? check-path-3))
            (error (string-append "Module cannot be found: "
                                  (%module-flat-name module)
                                  "\n-- Looked into: \n"
                                  check-path-1 "\n"
                                  check-path-2 "\n"
                                  check-path-3)))))

(define^ (%module-path module)
  (let ((sphere (%module-sphere module)))
    (if sphere
        (%sphere-path sphere)
        "")))

(define^ (%module-path-src module)
  (string-append (%module-path module) (default-src-directory)))

(define^ (%module-path-lib module)
  (string-append (%module-path module) (default-lib-directory)))

;;; Module versions identify debug, architecture or any compiled-in features
;;; Normalizes removing duplicates and sorting alphabetically
(define^ (%version->string version-symbol-list)
  (letrec ((delete-duplicates
            (lambda (l)
              (cond ((null? l)
                     '())
                    ((member (car l) (cdr l))
                     (delete-duplicates (cdr l)))
                    (else
                     (cons (car l) (delete-duplicates (cdr l)))))))
           (insertion-sort
            (letrec ((insert
                      (lambda (x lst)
                        (if (null? lst)
                            (list x)
                            (let ((y (car lst))
                                  (ys (cdr lst)))
                              (if (string<=? x y)
                                  (cons x lst)
                                  (cons y (insert x ys))))))))
              (lambda (lst)
                (if (null? lst)
                    '()
                    (insert (car lst)
                            (insertion-sort (cdr lst))))))))
    (apply string-append (map (lambda (s) (string-append s "___"))
                              (insertion-sort
                               (map symbol->string
                                    (delete-duplicates version-symbol-list)))))))

;;; Transforms / into _
(define^ (%module-flat-name module)
  (assure (%module? module) (module-error module))
  (let ((name (string-copy (symbol->string (%module-id module)))))
    (let recur ((i (-- (string-length name))))
      (if (= i 0)
          name
          (begin (when (eq? (string-ref name i) #\/)
                       (string-set! name i #\_))
                 (recur (-- i)))))))

(define^ (%module-filename-scm module)
  (assure (%module? module) (module-error module))
  (string-append (symbol->string (%module-id module))
                 (default-scm-extension)))

(define^ (%module-filename-c module #!key (version '()))
  (assure (%module? module) (module-error module))
  (string-append (if (null? version)
                     (%version->string (%module-version module))
                     (%version->string version))
                 (symbol->string (%module-sphere module))
                 "__"
                 (%module-flat-name module)
                 (default-c-extension)))

(define^ (%module-filename-o module #!key (version '()))
  (assure (%module? module) (module-error module))
  (string-append (if (null? version)
                     (%version->string (%module-version module))
                     (%version->string version))
                 (symbol->string (%module-sphere module))
                 "__"
                 (%module-flat-name module)
                 (default-o-extension)))

;;; Module dependecies
(define^ (%module-dependencies module)
  (assure (%module? module) (module-error module))
  (assq (%module-id module) (%sphere-dependencies (%module-sphere module))))

(define^ (%module-dependencies-select type)
  (lambda (module)
    (assure (%module? module) (module-error module))
    (uif (assq (%module-id module) (%sphere-dependencies (%module-sphere module)))
         (uif (assq type (cdr ?it))
              (cdr ?it)
              '())
         '())))

(define^ (%module-dependencies-to-include module)
  ((%module-dependencies-select 'include) module))

(define^ (%module-dependencies-to-load module)
  ((%module-dependencies-select 'load) module))

;-------------------------------------------------------------------------------
; Including and loading
;-------------------------------------------------------------------------------

(define *%included-modules* '((base: prelude#)))

(define-macro (%include . module.lib)
  (let* ((module (if (null? (cdr module.lib))
                     (car module.lib)
                     module.lib))
         (module-name (symbol->string (%module-id module)))
         (sphere (%module-sphere module)))
    (if sphere
        (begin
          (display (string-append "-- including: " module-name " -- (" (symbol->string sphere) ")" "\n"))
          `(include ,(string-append (%sphere-path sphere) (default-src-directory) module-name (default-scm-extension))))
        (begin
          (display (string-append "-- loading -- " module-name ")\n"))
          `(include ,(%module-filename-scm module))))))

(define *%loaded-modules* '())

(define-macro (%load #!key (verbose #t) #!rest module)
  (define (load-module module)
    (let ((sphere (%module-sphere module))
          (module-name (symbol->string (%module-id module))))
      (if sphere
          (begin (if verbose
                     (display (string-append "-- loading -- ("
                                             (symbol->string sphere)
                                             ": "
                                             module-name
                                             ")\n")))
                 (let ((file-o (string-append (%sphere-path sphere) (default-lib-directory) (%module-filename-o module)))
                       (file-scm (string-append (%sphere-path sphere) (default-src-directory) (%module-filename-scm module))))
                  (cond ((file-exists? file-o)
                         (load file-o))
                        ((file-exists? file-scm)
                         (load file-scm))
                        (else
                         (error (string-append "Module: " module-name " cannot be found in its sphere's path"))))))
          (begin (if verbose
                     (display (string-append "-- loading -- " module-name ")\n")))
                 (load (%module-filename-scm module))))))
  (define (load-deps module)
    (if (not (member module *%loaded-modules*))
        (begin (for-each load-deps (%module-dependencies-to-load module))
               (load-module module)
               (set! *%loaded-modules* (cons module *%loaded-modules*)))))
  (let ((module (if (null? (cdr module))
                    (car module)
                    module)))
    (assure (%module? module) (module-error module))
    (load-deps module)))
