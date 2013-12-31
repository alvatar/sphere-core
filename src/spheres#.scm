;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Sphere (module system)


(define expander:include #f)
(define (riaxpander:current?) #f)

(if (file-exists? "~~/spheres/core/src/riaxpander/")
    (begin
      (eval '(##begin (##include "~~/spheres/core/src/riaxpander/utils.scm")
                      (##include "~~/spheres/core/src/riaxpander/history.scm")
                      (##include "~~/spheres/core/src/riaxpander/closure.scm")
                      (##include "~~/spheres/core/src/riaxpander/denotation.scm")
                      (##include "~~/spheres/core/src/riaxpander/environment.scm")
                      (##include "~~/spheres/core/src/riaxpander/transform.scm")
                      (##include "~~/spheres/core/src/riaxpander/taxonomy.scm")
                      (##include "~~/spheres/core/src/riaxpander/classify.scm")
                      (##include "~~/spheres/core/src/riaxpander/standard.scm")
                      (##include "~~/spheres/core/src/riaxpander/synrules.scm")
                      (##include "~~/spheres/core/src/riaxpander/gambit.scm")))
      (riaxpander:install)
      (set! expander:include riaxpander:include)
      (set! riaxpander:include #t))
    (println "*** Info: Riaxpander has NOT been loaded!"))


(include "prelude.scm")


;;------------------------------------------------------------------------------

;;!! Sphere

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

(define^ config-file
  (make-parameter "config.scm"))

;;! Read current sphere config data
;; Returns null list if no config data found
(define^ %current-config
  (let ((cached #f))
    (lambda ()
      (or cached
          (begin
            (set! cached
                  (with-exception-catcher
                   (lambda (e) (if (no-such-file-or-directory-exception? e)
                              ;; If config file not found try to find global %paths variable
                              (with-exception-catcher
                               (lambda (e2) (if (unbound-global-exception? e2)
                                           '()
                                           (raise e2)))
                               ;; inject global %paths variable if no config file found
                               (lambda () `((paths: ,@%system-paths))))
                              (raise e)))
                   (lambda () (call-with-input-file (config-file) read-all))))
            cached)))))

(define^ %current-sphere
  (make-parameter
   (let ((current-sphere-info (assq 'sphere: (%current-config))))
     (if current-sphere-info
         (string->symbol (cadr current-sphere-info))
         #f))))

(define^ (%sphere-system-path sphere)
  (string-append "~~spheres/" (symbol->string sphere) "/"))

(define^ (%sphere-path sphere)
  (let ((paths (%paths)))
    (if sphere
        ;; First try with custom paths: in config file
        (let ((sphere-pair (assq (string->keyword (symbol->string sphere)) paths)))
          (if sphere-pair
              (string-append (path-strip-trailing-directory-separator (cadr sphere-pair)) "/")
              ;; Otherwise, try system-installed spheres
              (let ((system-path (%sphere-system-path sphere)))
                (and (file-exists? system-path)
                     (path-expand system-path)
                     ;; (error (string-append "Sphere not found: " (object->string sphere) " -- Please set a path in config file or install the sphere in the ~~spheres directory"))
                     ))))
        #f)))

(define^ (%sphere-exists? sphere)
  (and (%sphere-path sphere) #t))

(define^ (%paths)
  (let* ((paths-pair (assq paths: (%current-config)))
         (paths (if paths-pair (cdr paths-pair) '())))
    (if (%current-sphere)
         (cons
          (list (symbol->keyword (%current-sphere)) (current-directory))
          paths)
         paths)))

;;! Used for getting a specific sphere config data
(define^ %sphere-config
  (let ((config-dict '()))
    (lambda (sphere)
      (and
       (%sphere-exists? sphere)
       (let ((sphere-pair (assq sphere config-dict)))
         (if sphere-pair
             (cadr sphere-pair)
             (let* ((config-data (with-exception-catcher
                                  (lambda (e) (if (no-such-file-or-directory-exception? e)
                                             (error (string-append "Sphere \"" (symbol->string sphere) "\" doesn't have a con
fig.scm file"))
                                             (raise e)))
                                  (lambda () (call-with-input-file
                                            (string-append (or (%sphere-path sphere) "")
                                                           (config-file))
                                          read-all))))
                    (new-pair (list sphere config-data)))
               (if (eq? sphere (string->symbol (cadr (assq sphere: config-data))))
                   (begin (set! config-dict
                                (cons new-pair config-dict))
                          (cadr new-pair))
                   #f))))))))

;;! Get dependencies of sphere's modules
(define^ (%sphere-dependencies sphere)
  (define map*
    (lambda (f l)
      (cond ((null? l) '())
            ((not (pair? l)) (f l))
            (else (cons (map* f (car l)) (map* f (cdr l)))))))
  (and
   (%sphere-exists? sphere)
   (let ((expand-cond-features
          (lambda (deps)
            (let ((any-eq? (lambda (k l)
                             (let recur ((l l))
                               (cond ((null? l) #f)
                                     ((eq? k (car l)) #t)
                                     (else (recur (cdr l))))))))
              (let expand-cond-features ((deps deps))
                (cond ((null? deps) '())
                      ((not (pair? deps)) deps)
                      ((eq? 'cond-expand (car deps))
                       ;; cond-expand found
                       (let find-condition ((conditions (cdr deps)))
                         (cond ((null? conditions)
                                (error "cond-expand in dependencies not met"))
                               ((not (pair? (car conditions)))
                                (error "incorrect cond-expand syntax"))
                               ((and (symbol? (caar conditions))
                                     (any-eq? (caar conditions) ##cond-expand-features))
                                (cons 'cond-expanded (cdar conditions)))
                               ((and (symbol? (caar conditions))
                                     (eq? (caar conditions) 'else))
                                (cons 'cond-expanded (cdar conditions)))
                               ((and (pair? (caar conditions))
                                     (eq? (caaar conditions) 'or))
                                (error "OR clauses in dependencies not implemented yet"))
                               ((and (pair? (caar conditions))
                                     (eq? (caaar conditions) 'and))
                                (error "AND clauses in dependencies not implemented yet"))
                               ((and (pair? (caar conditions))
                                     (eq? (caaar conditions) 'not))
                                (error "NOT clauses in dependencies not implemented yet"))
                               ((and (pair? (caar conditions)))
                                (error "incorrect cond-expand syntax: must use OR, AND, NOT lists or single symbols"))
                               (else
                                (find-condition (cdr conditions))))))
                      (else (let ((head (expand-cond-features (car deps)))
                                  (tail (expand-cond-features (cdr deps))))
                              ;; Find a cleaner algorithm!
                              ;; What does now is tag expanded expressions for appending them instead of cons'ing
                              (if (and (pair? head) (eq? (car head) 'cond-expanded))
                                  (append (cdr head) tail)
                                  (cons head tail)))))))))
         (expand-wildcards
          (lambda (deps)
            (map* (lambda (e)
                    (if (eq? e '=)
                        (string->keyword (symbol->string sphere))
                        e))
                  deps)))
         (normalize-modules
          (lambda (deps)
            (map (lambda (e)
                   (let ((module (car e))
                         (rest (cdr e)))
                     (cons
                      (cond
                       ((%module-reduced-form? module) (%module-normalize module override-sphere: sphere))
                       ((eq? '= (car module)) (cons (symbol->keyword sphere) (cdr module)))
                       (else module))
                      rest)))
                 deps))))
     (let ((deps-pair (assq dependencies: (%sphere-config sphere))))
       (if deps-pair
           (normalize-modules
            (expand-wildcards
             (expand-cond-features
              (cdr deps-pair))))
           '())))))


;;------------------------------------------------------------------------------

;;!! Module

(define^ (%make-module #!key (sphere #f) id (version '()))
  (if sphere
      (append (list (->keyword sphere)
                    (->symbol id)
                    version: version))
      (->symbol id)))

;;! Module structure: (sphere: module-id [version: '(list-of-version-features)])
(define^ %module-reduced-form? symbol?)

(define^ (%module-normal-form? module)
  (and (list? module)
       (or (keyword? (car module))
           (and (symbol? (car module)) ; In case we don't have keywords (syntax-case)
                (equal? #\: (let ((str (symbol->string (car module))))
                              (string-ref str (- (string-length str) 1)))))
           (eq? '= (car module))) ; Wildcard = represents the "this" sphere
       (not (null? (cdr module)))
       (symbol? (cadr module))
       (or (null? (cddr module))
           (and (eq? version: (caddr module))
                (list? (cadddr module))))))

(define^ (%module-normalize module
                            #!key
                            (override-sphere #f)
                            (override-version #f))
  (%make-module sphere: (if override-sphere override-sphere (%module-sphere module))
                id: (%module-id module)
                version: (if override-version override-version (%module-version module))))

;;! Module predicate
(define^ (%module? module)
  (or (%module-reduced-form? module)
      (%module-normal-form? module)))

;;! Module equality
(define^ (%module=? m1 m2)
  (equal? (%module-normalize m1)
          (%module-normalize m2)))

;;! Throw a module format error
(define^ (%module-error module)
  (error "Error parsing module directive (wrong module format): " module))

;;! Check if module exists
(define^ (%check-module module)
  (or (%module? module)
      (error "Ill-defined module: " module))
  (let ((sphere (%module-sphere module)))
    (or (%sphere-exists? sphere)
        (error "Sphere doesn't exist: " (symbol->string sphere))))
  (or (string-append (%module-path-src module) (%module-filename-scm module))
      (string-append (%module-path-lib module) (%module-filename-o module))
      (error "Module doesn't exist: " (object->string module))))

(define^ (%module-sphere module)
  (or (%module? module) (%module-error module))
  (if (%module-normal-form? module)
      (let ((first (car module)))
        (if (keyword? first) (keyword->symbol first) first))
      (%current-sphere)))

(define^ (%module-id module)
  (or (%module? module) (%module-error module))
  (if (%module-normal-form? module)
      (cadr module)
      module))

(define^ (%module-version module)
  (or (%module? module) (%module-error module))
  (if (%module-normal-form? module)
      ;; Search for version: from the third element on
      (let ((version (memq 'version: (cddr module))))
        (if version
            (cadr version)
            '()))
      '()))

(define^ (%module-path module)
  (let ((sphere (%module-sphere module)))
    (if sphere
        (%sphere-path sphere)
        "")))

(define^ (%module-path-src module)
  (string-append (%module-path module) (default-src-directory)))

(define^ (%module-path-lib module)
  (string-append (%module-path module) (default-lib-directory)))

(define^ (%module-namespace module)
  (string-append
   (symbol->string (%module-sphere module))
   ":"
   (symbol->string (%module-id module))
   "#"))

;;! Module versions identify debug, architecture or any compiled-in features
;; Normalizes removing duplicates and sorting alphabetically
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

;;! Transforms / into _
(define^ (%module-flat-name module)
  (or (%module? module) (%module-error module))
  (let ((name (string-copy (symbol->string (%module-id module)))))
    (let recur ((i (- (string-length name) 1)))
      (if (= i 0)
          name
          (begin (and (eq? (string-ref name i) #\/)
                      (string-set! name i #\_))
                 (recur (- i 1)))))))

(define^ (%module-filename-scm module)
  (or (%module? module) (%module-error module))
  (string-append (symbol->string (%module-id module))
                 (default-scm-extension)))

(define^ (%module-filename-c module #!key (version '()))
  (or (%module? module) (%module-error module))
  (string-append (if (null? version)
                     (%version->string (%module-version module))
                     (%version->string version))
                 (symbol->string (%module-sphere module))
                 "__"
                 (%module-flat-name module)
                 (default-c-extension)))

(define^ (%module-filename-o module #!key (version '()))
  (or (%module? module) (%module-error module))
  (string-append (if (null? version)
                     (%version->string (%module-version module))
                     (%version->string version))
                 (symbol->string (%module-sphere module))
                 "__"
                 (%module-flat-name module)
                 (default-o-extension)))

;;! Module dependecies, as directly read from the %config
(define^ (%module-dependencies module)
  (%check-module module)
  (assq (%module-id module) (%sphere-dependencies (%module-sphere module))))

;;! Builds a procedure to get a list of dependencies of one type
;; It works by first trying the versioned module, if not found it falls back to the unversioned module
;; If the root module doesn't have a dependencies for the exact version, the unversioned dependencies will
;; be used instead. In this case, the same version requested for the root module will be tried for every
;; dependency first before falling back.
;; Resulting dependency lists are normalized modules. However, if the dependency lists contains stuff
;; other than modules, they are not normalized (obviously).
(define^ (%module-shallow-dependencies-select type)
  (letrec ((find-dependencies-list
            (lambda (module sphere sphere-deps omit-version?)
              (cond ((null? sphere-deps) #f)
                    ;; Check if module is from this sphere
                    ((not (eq? (%module-sphere (caar sphere-deps)) sphere))
                     (error (string-append
                             "Dependency lists can't be done for non-local modules -> change "
                             (config-file)
                             " (tip: you can use = to identify local spheres)")))
                    ;; Try versioned
                    ((equal? (%module-normalize module)
                             (%module-normalize (caar sphere-deps)
                                                override-sphere: sphere))
                     (car sphere-deps))
                    ;; Try unversioned
                    ((and omit-version?
                          (equal? (cadr (%module-normalize module))
                                  (cadr (%module-normalize (caar sphere-deps)
                                                           override-sphere: sphere))))
                     ;; (display (string-append "*** INFO -- Propagating "
                     ;;                         (object->string (%module-version module))
                     ;;                         " version to "
                     ;;                         (object->string module)
                     ;;                         " dependencies:\n"))
                     (car sphere-deps))
                    (else (find-dependencies-list module sphere (cdr sphere-deps) omit-version?))))))
    (lambda (module)
      (%check-module module)
      (let ((module-sphere (%module-sphere module))
            (get-dependency-list (lambda (l) (let ((type-pair (assq type (cdr l))))
                                          (if type-pair (cdr type-pair) '())))))
        ;; Try first with versioned module
        (let ((versioned (find-dependencies-list module
                                                 module-sphere
                                                 (%sphere-dependencies module-sphere)
                                                 #f)))
          (if versioned
              (map (lambda (m) (if (%module? m)
                              (%module-normalize m)
                              m))
                   (get-dependency-list versioned))
              ;; ...otherwise try to find unversioned module in dependency list
              (let ((unversioned (find-dependencies-list module
                                                         module-sphere
                                                         (%sphere-dependencies module-sphere)
                                                         #t)))
                (if unversioned
                    ;; ...but we will make them versioned, propagating the version
                    (map (lambda (m) (if (%module? m)
                                    (%module-normalize m override-version: (%module-version module))
                                    m))
                         (get-dependency-list unversioned))
                    '()))))))))

(define^ (%module-shallow-dependencies-to-prelude module)
  ((%module-shallow-dependencies-select 'prelude) module))

(define^ (%module-shallow-dependencies-to-include module)
  ((%module-shallow-dependencies-select 'include) module))

(define^ (%module-shallow-dependencies-to-load module)
  ((%module-shallow-dependencies-select 'load) module))

(define^ (%module-shallow-dependencies-cc-options module)
  ((%module-shallow-dependencies-select 'cc-options) module))

(define^ (%module-shallow-dependencies-ld-options module)
  ((%module-shallow-dependencies-select 'ld-options) module))

;;! Gets the full tree of dependencies, building a list in the right order
;; .parameter symbol-to-follow They symbol that will look for in the dependencies,
;; following its subdependencies recursively
;; .parameter symbol-to-return The symbol that the function will record, returning
;; it after the whole dependency tree has been traversed
;; .parameter append The procedure used to append the returned results
(define^ (%module-deep-dependencies-select type-to-follow type-to-return)
  (lambda (module)
    (let ((deps '()))
      (let recur ((module module))
        (for-each recur ((%module-shallow-dependencies-select type-to-follow) module))
        (or (assq (%module-normalize module) deps)
            (set! deps (cons (list
                              (%module-normalize module)
                              ((%module-shallow-dependencies-select type-to-return) module))
                             deps))))
      (apply append (map cadr (reverse deps))))))

;;! Gets a list with all the dependencies to load in the right order
(define^ (%module-deep-dependencies-to-load module)
  ((%module-deep-dependencies-select 'load 'load) module))

;;! Gets a list with all the dependencies to include in the right order
(define^ (%module-deep-dependencies-to-include module)
  ((%module-deep-dependencies-select 'include 'include) module))

;;! Convert cc-options dependencies into a string
(define^ (%process-cc-options option-items)
  (define (--cflag? o) (and (pair? o)
                            (not (null? (cdr o)))
                            (eq? (car o) 'pkg-config--cflags)))
  ;; TODO: What works for Windows?
  (let* ((free-strings '())
         (pkg-config--cflags-list
          (let recur ((result option-items))
            (cond ((null? result) '())
                  ((string? (car result))
                   (or (member (car result) free-strings)
                       (set! free-strings (cons (car result) free-strings)))
                   (recur (cdr result)))
                  ((--cflag? (car result))
                   (cons (cadar result) (recur (cdr result))))
                  (else (recur (cdr result)))))))
    ;; Append free strings at the beginning of the ld-options
    (string-append (apply string-append
                          (reverse (cons " " free-strings)))
                   (if (null? pkg-config--cflags-list)
                       ""
                       (let ((p (open-process
                                 (list path: "pkg-config"
                                       arguments: (cons "--cflags" pkg-config--cflags-list)))))
                         (if (zero? (process-status p))
                             (read-line p)
                             (error "Error running pkg-config: any C library in the dependency list might not be installed, pkg-config might not be installed, or config.scm file might be misconfigured.")))))))

;;! Get a string of cc-options from the full deep of dependencies
(define^ (%module-deep-dependencies-cc-options module)
  ((%module-deep-dependencies-select 'load 'cc-options) module))

;;! Convert ld-options dependencies into a string
(define^ (%process-ld-options option-items)
  (define (--libs? o) (and (pair? o)
                           (not (null? (cdr o)))
                           (eq? (car o) 'pkg-config--libs)))
  ;; TODO: What works for Windows?
  (let* ((free-strings '())
         (pkg-config--libs-list
          (let recur ((result option-items))
            (cond ((null? result) '())
                  ((string? (car result))
                   (or (member (car result) free-strings)
                       (set! free-strings (cons (car result) free-strings)))
                   (recur (cdr result)))
                  ((--libs? (car result))
                   (cons (cadar result) (recur (cdr result))))
                  (else (recur (cdr result)))))))
    ;; Append free strings at the beginning of the ld-options
    (string-append (apply string-append
                          (reverse (cons " " free-strings)))
                   (if (null? pkg-config--libs-list)
                       ""
                       (let ((p (open-process
                                 (list path: "pkg-config"
                                       arguments: (cons "--libs" pkg-config--libs-list)))))
                         (if (zero? (process-status p))
                             (read-line p)
                             (error "Error running pkg-config: any C library in the dependency list might not be installed, pkg-config might not be installed, or config.scm file might be misconfigured.")))))))

;;! Get a string of ld-options from the full deep of dependencies
(define^ (%module-deep-dependencies-ld-options module)
  ((%module-deep-dependencies-select 'load 'ld-options) module))


;;------------------------------------------------------------------------------

;;!! Utils

;;! Builds a new list of modules merging two lists
;; Not optimized
(define^ (%merge-module-lists dep1 dep2)
  (letrec ((delete-duplicates
            (lambda (l) (cond ((null? l) '())
                         ((member (car l) (cdr l)) (delete-duplicates (cdr l)))
                         (else (cons (car l) (delete-duplicates (cdr l))))))))
    ;; We work on reversed list to keep the first occurence
    (reverse (delete-duplicates (reverse (append dep1 dep2))))))

;;! Select modules from a list belonging to a sphere
(define^ (%select-modules modules spheres)
  (let* ((select (if (pair? spheres) spheres (list spheres)))
         (any-eq? (lambda (k l)
                    (let recur ((l l))
                      (cond ((null? l) #f)
                            ((eq? k (car l)) #t)
                            (else (recur (cdr l))))))))
    (let recur ((output modules))
      (cond ((null? output) '())
            ((any-eq? (%module-sphere (car output)) select)
             (cons (car output) (recur (cdr output))))
            (else (recur (cdr output)))))))


;;------------------------------------------------------------------------------

;;!! Including and loading

;;! Is there a header for this module? If so, return the header module
(define^ (%module-header module)
  (let ((header-module (%make-module
                        sphere: (%module-sphere module)
                        id: (string->symbol (string-append (symbol->string (%module-id module)) "#"))
                        version: (%module-version module))))
    (and (file-exists?
          (string-append (%module-path-src header-module)
                         (%module-filename-scm header-module)))
         header-module)))

;;! Is there a macros module for this module? If so, return the macros module
(define^ (%module-macros module)
  (let ((macros-module (%make-module
                        sphere: (%module-sphere module)
                        id: (string->symbol (string-append (symbol->string (%module-id module)) "-macros"))
                        version: (%module-version module))))
    (and (file-exists?
          (string-append (%module-path-src macros-module)
                         (%module-filename-scm macros-module)))
         macros-module)))

;;! Include module and dependencies
(define ##include-module-and-dependencies #f)

;;! Load module and dependencies
(define ##load-module-and-dependencies #f)

(let* ((*loaded-modules* '())
       (*included-modules* '())
       (include-single-module
        (lambda (module options)
          (let* ((verbose (and (memq 'verbose options) #t))
                 (sphere (%module-sphere module))
                 (module-name (symbol->string (%module-id module))))
            (if sphere
                (let ((include-file (string-append (%module-path-src module)
                                                   (%module-filename-scm module))))                  
                  (if (not (member (%module-normalize module override-version: '()) *included-modules*))
                      (begin
                        (if verbose
                            (display (string-append "-- source included -- " (object->string module) "\n")))
                        (set! *included-modules* (cons (%module-normalize module override-version: '()) *included-modules*))
                        (expander:include include-file))))
                (begin
                  (if (not (member (%module-normalize module override-version: '()) *included-modules*))
                      (begin
                        (if verbose
                            (display (string-append "-- source included -- " (object->string module) "\n")))
                        (set! *included-modules* (cons (%module-normalize module override-version: '()) *included-modules*))
                        (expander:include (%module-filename-scm module))))))))))
  (set!
   ##include-module-and-dependencies
   (lambda (root-module options)
     (let ((force-include (and (memq 'force options) #f)))
       (let recur ((module root-module))
         (if (or force-include
                 (not (member (%module-normalize module override-version: '()) *included-modules*)))
             (begin (for-each recur (%module-shallow-dependencies-to-include module))
                    (include-single-module module '(verbose #t))))))))
  (set!
   ##load-module-and-dependencies
   (let ((load-single-module
          (lambda (module options)
            (%check-module module)
            (let ((verbose (and (memq 'verbose options) #t))
                  (includes (and (memq 'includes options) #t))
                  (sphere (%module-sphere module)))
              (let ((header-module (%module-header module))
                    (macros-module (%module-macros module)))
                (if header-module
                    (eval `(expander:include ,(string-append (%module-path-src header-module)
                                                                 (%module-filename-scm header-module)))))
                (if includes
                    (for-each (lambda (m) (include-single-module m '(verbose)))
                              (%module-shallow-dependencies-to-include module)))
                (if macros-module
                    (##include-module-and-dependencies macros-module '()))
                ;; (pp (string-append (%sphere-path sphere) (default-lib-directory) (%module-filename-o module)))
                (if sphere
                    (let ((file-o (string-append (%sphere-path sphere) (default-lib-directory) (%module-filename-o module)))
                          (file-scm (string-append (%sphere-path sphere) (default-src-directory) (%module-filename-scm module))))
                      (cond ((file-exists? file-o)
                             (if verbose
                                 (display (string-append "-- object loaded -- " (object->string module) "\n")))
                             (load file-o)
                                        ;(pp file-o)
                             file-o)
                            ((file-exists? file-scm)
                             (expander:include file-scm)
                             (if verbose
                                 (display (string-append "-- source loaded -- " (object->string module) "\n")))
                             file-scm)
                            (else
                             (error (string-append "Module: "
                                                   (object->string module)
                                                   " cannot be found in current sphere's path"))))
                      (set! *loaded-modules* (cons (%module-normalize module override-version: '()) *loaded-modules*)))
                    (begin (if verbose
                               (display (string-append "-- object loaded -- " (object->string module) "\n")))
                           (load (%module-filename-scm module)))))))))
     (lambda (root-module options)
       ;; Get options, as #t or #f
       (let ((omit-root (and (memq 'omit-root options) #t)))
         (let recur ((module root-module))
           (if (not (member (%module-normalize module override-version: '()) *loaded-modules*))
               (begin (for-each recur (%module-shallow-dependencies-to-load module))
                      (or (and omit-root (equal? root-module module))
                          (load-single-module module options))))))))))

;;! import-include macro
(##define-macro (##import-include . module)
  (cond
   ((string? (car module))
    ;; If filename given, just include it (doesn't register as loaded module)
    (eval `(expander:include ,(car module))))
   ;; It comes quoted (it's a monster)
   ((and (pair? (car module))
         (eq? 'quote (caar module)))
    (let ((module (cadar module)))
      (%check-module module)
      `(##include-module-and-dependencies ',module '(verbose))))
   ;; oterwise act normally
   (else
    (let ((module (if (null? (cdr module))
                      (car module)
                      ;; If it defines the sphere, process the sphere name to make it a keyword
                      (cons (let ((first (car module)))
                              (if (keyword? first)
                                  first
                                  (let ((str (apply string
                                                    (string->list
                                                     (symbol->string first)))))
                                    (string-shrink! str (- (string-length str) 1))
                                    (string->keyword str))))
                            (cdr module)))))
      (%check-module module)
      `(##include-module-and-dependencies ',module '(verbose))))))

;;; Load only module dependencies, do not load the module
;; (##define-macro (##load-module-dependencies . module)
;;   (let ((module (if (null? (cdr module))
;;                     (car module)
;;                     module)))
;;     (%check-module module)
;;     (##load-module-and-dependencies module '(omit-root verbose includes))))

;;! import macro, loads dependencies
(##define-macro (##import . module)
  (let* ((module (if (null? (cdr module))
                     (car module)
                     ;; If it defines the sphere, process the sphere name to make it a keyword
                     (cons (let ((first (car module)))
                             (if (keyword? first)
                                 first
                                 (let* ((str (symbol->string first))
                                        (str-len (string-length str)))
                                   (if (not (char=? (string-ref str (- str-len 1)) #\:))
                                       (error "Sphere names should end with ':'"))
                                   (string-shrink! str (- str-len 1))                                   
                                   (string->keyword str))))
                           (cdr module)))))
    (%check-module module)
    `(##load-module-and-dependencies ',module '(verbose includes))))
