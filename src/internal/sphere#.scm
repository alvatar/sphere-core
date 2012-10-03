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
  (make-parameter ".o"))

(define^ default-c-extension
  (make-parameter ".c"))

;;; Read config data
(define^ %config
  (let ((cached #f))
    (lambda ()
      (or cached
          (begin
            (set! cached
                  (with-exception-catcher
                   (lambda (e) (if (no-such-file-or-directory-exception? e)
                              ;; If config.scm not found try to find global %paths variable, otherwise signal both errors
                              (with-exception-catcher
                               (lambda (e2) (if (unbound-global-exception? e2)
                                           (error "cannot find modules: config.scm file not found and %paths variable undefined")
                                           (raise e2)))
                               ;; inject %paths variable if no config.scm found
                               (lambda () `((paths: ,@%paths))))
                              (raise e)))
                   (lambda () (call-with-input-file "config.scm" read-all))))
            cached)))))

(define^ (%current-sphere)
  (let ((current-sphere-info (assq sphere: (%config))))
    (if current-sphere-info
        (string->symbol (cadr current-sphere-info))
        (error "No sphere: tag provided in config file"))))

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
  (cons
   (list (symbol->keyword (%current-sphere)) (current-directory))
   (uif (assq paths: (%config))
        (cdr ?it)
        '())))

;-------------------------------------------------------------------------------
; Module
;-------------------------------------------------------------------------------

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
  (assure (%module? module) (error "Error parsing %include -- Wrong module format:" module))
  (if (%module-normal-form? module)
      (keyword->symbol (car module))
      (%current-sphere)))

(define^ (%module-id module)
  (assure (%module? module) (error "Error parsing %include -- Wrong module format:" module))
  (if (%module-normal-form? module)
      (cadr module)
      module))

(define^ (%module-version module)
  (assure (%module? module) (error "Error parsing %include -- Wrong module format:" module))
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

(define^ (%module-info module)
  (assure (%module? module) (error "Error parsing %include: wrong module format:" module))
  (let* ((sphere (%module-sphere module))
         (sphere-path (%sphere-path sphere))
         (module-name (%module-flat-name module)))
    ;; Check if the module exists, signal error if it doesn't
    (%check-module-exists? module)
    (values sphere
            sphere-path
            module-name)))

(define^ (%module-path module)
  (receive
   (_ path __)
   (%module-info module)
   (unless path "")))

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
  (assure (%module? module) (error "Error parsing %include: wrong module format:" module))
  (let ((name (string-copy (symbol->string (%module-id module)))))
    (let recur ((i (-- (string-length name))))
      (if (= i 0)
          name
          (begin (when (eq? (string-ref name i) #\/)
                       (string-set! name i #\_))
                 (recur (-- i)))))))

(define^ (%module-filename-scm module)
  (assure (%module? module) (error "Error parsing %include: wrong module format:" module))
  (string-append (symbol->string (%module-id module))
                 (default-scm-extension)))

(define^ (%module-filename-c module #!key (version '()))
  (assure (%module? module) (error "Error parsing %include: wrong module format:" module))
  (string-append (if (null? version)
                     (%version->string (%module-version module))
                     (%version->string version))
                 (symbol->string (%module-sphere module))
                 "__"
                 (%module-flat-name module)
                 (default-c-extension)))

(define^ (%module-filename-o module #!key (version '()))
  (assure (%module? module) (error "Error parsing %include: wrong module format:" module))
  (string-append (if (null? version)
                     (%version->string (%module-version module))
                     (%version->string version))
                 (symbol->string (%module-sphere module))
                 "__"
                 (%module-flat-name module)
                 (default-o-extension)))

;-------------------------------------------------------------------------------
; Including and loading
;-------------------------------------------------------------------------------

(define-macro (%include . module.lib)
  (receive (lib prefix module-name)
           (%module-info (if (null? (cdr module.lib))
                             (car module.lib)
                             module.lib))
           (if lib
               (begin
                 (display (string-append "-- including: " module-name " -- (" (symbol->string lib) ")" "\n"))
                 `(include ,(string-append prefix (default-src-directory) module-name (default-scm-extension))))
               (begin
                 (display (string-append "-- including: " module-name "\n"))
                 `(include ,(string-append (default-src-directory) module-name (default-scm-extension)))))))

(define-macro (%load . module.lib)
  (receive (lib lib-name prefix module-name)
           (%module-info (if (null? (cdr module.lib))
                             (car module.lib)
                             module.lib))
           (if lib
               (begin
                 (display (string-append "-- loading: " module-name " -- (" lib-name ")" "\n"))
                 `(load ,(string-append prefix (default-lib-directory) module-name)))
               (begin
                 (display (string-append "-- loading: " module-name "\n"))
                 `(load ,(string-append (default-lib-directory) module-name))))))

