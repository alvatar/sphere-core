;;!!! Spheres configuration and installation prelude
;; .author Álvaro Castro-Castilla, Copyright (c) 2012-2014 All rights reserved.


(define expander:include #f)
(define current-macro-expander #f)
(define ##spheres-environment? #t)


;; (let ((riaxpander-o "~~spheres/riaxpander.o1")
;;       (riaxpander-scm "src/riaxpander/riaxpander.scm"))
;;   (if (file-exists? riaxpander-o)
;;       (load riaxpander-o)
;;       (if (file-exists? riaxpander-scm)
;;           (begin
;;             (println "*** INFO -- Riaxpander is being included instead of loaded. If you are bootstrapping Spheres, this is ok.")
;;             (eval `(##include ,riaxpander-scm)))
;;           (error "Cannot find Macro Expander. Is Sphere Core properly installed?"))))
;; (riaxpander#riaxpander:install!)
;; (set! current-macro-expander 'alexpander)
;; (set! expander:include riaxpander#riaxpander:include)

(let ((syntax-case-o "~~spheres/syntax-case.o1")
      (syntax-case-scm "~~spheres/syntax-case.scm"))
  (cond
   ((file-exists? syntax-case-o)
    (println "*** INFO -- loading syntax expander")
    (load syntax-case-o)
    (set! current-macro-expander 'syntax-case))
   ((file-exists? syntax-case-scm)
    (println "*** INFO -- loading syntax expander from source")
    (parameterize ((current-directory (getenv "scsc_home" "~~spheres/"))
                   (current-readtable (readtable-sharing-allowed?-set (current-readtable) 'serialize)))
                  (load syntax-case-scm))
    (set! current-macro-expander 'syntax-case))
   ((file-exists? "src/scsc/scsc.scm")
    (println "*** INFO -- Bootstrapping: syntax expander omitted"))
   (else
    (error "Cannot find macro expander. Is Sphere Core properly installed?"))))

(if current-macro-expander
    (set! expander:include
          (lambda (file)
            (for-each eval (with-input-from-file file read-all)))))

;;------------------------------------------------------------------------------
;;!! Macro utils

;;!! Define functions for usage in low-level macros (first method)
;; (define^ (f ... ) ... )

(##define-macro (eval-in-macro-environment . exprs)
  (if (pair? exprs)
      (eval (if (null? (cdr exprs)) (car exprs) (cons 'begin exprs))
            (interaction-environment))
      #f))
(##define-macro (eval-in-macro-environment-no-result . exprs)
  `(eval-in-macro-environment
    ,@exprs
    '(begin)))

(##define-macro (define^ . args)
  (let ((pattern (car args))
        (body (cdr args)))
    `(eval-in-macro-environment-no-result
      (##define ,pattern ,@body))))


;;! symbol->keyword
(define^ (symbol->keyword s)
  (string->keyword (symbol->string s)))

;;! keyword->symbol
(define^ (keyword->symbol k)
  (string->symbol (keyword->string k)))

;;! Anything to symbol
(define^ (->symbol o)
  (string->symbol (object->string o)))

;;! Anything to keyword
(define^ (->keyword o)
  (string->keyword (object->string o)))

;;!! Define functions for usage in low-level macros (second method)
;; Insert your defines inside the following macros:
;; (at-expand-time-and-runtime
;;   (define ... )
;;   ... )
;; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2009-August/003781.html

;;! Define for both expand time and runtime
(##define-macro (at-expand-time-and-runtime . exprs)
  (let ((l `(begin ,@exprs)))
    (eval l)
    l))

;;! Define for expand time
(##define-macro (at-expand-time . expr)
  (eval (cons 'begin expr)))


;;------------------------------------------------------------------------------
;;!! Spheres


;;!! Default paths and extensions.
;; They should be used for refencing Spheres files in a general way, but not within
;; Sake tasks. In those situations (current-*-directory) versions should be used instead

(define^ default-source-directory
  (make-parameter "src/"))
(define^ default-build-directory
  (make-parameter "build/"))
(define^ default-lib-directory
  (make-parameter "lib/"))
(define^ default-bin-directory
  (make-parameter "bin/"))
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
  (define (flatten-tag tag lst)
    (let recur ((lst (if (and (not (null? lst)) (eq? (car lst) tag))
                         (cdr lst)
                         lst)))
      (cond
       ((null? lst)
        '())
       ((and (pair? (car lst)) (eq? tag (caar lst)))
        (append (recur (cdar lst)) (recur (cdr lst))))
       ((pair? (car lst))
        (cons (recur (car lst)) (recur (cdr lst))))
       (else
        (cons (car lst) (recur (cdr lst)))))))
  (and
   (%sphere-exists? sphere)
   (let ((expand-cond-features
          (lambda (form)
            (define expand-clauses
              (lambda clauses
                (define (feature-present? id)
                  (memq id (##cond-expand-features)))
                (define (eval-feature-req? feature-req)
                  (define (eval-and-clause? req-list)
                    (or (null? req-list)
                        (and (eval-feature-req? (car req-list))
                             (eval-and-clause? (cdr req-list)))))
                  (define (eval-or-clause? req-list)
                    (and (not (null? req-list))
                         (or (eval-feature-req? (car req-list))
                             (eval-or-clause? (cdr req-list)))))
                  (define (eval-not-clause? req)
                    (not (eval-feature-req? req)))
                  (cond
                   ((not (pair? feature-req))
                    (feature-present? feature-req))
                   ((eq? 'and (car feature-req))
                    (eval-and-clause? (cdr feature-req)))
                   ((eq? 'or (car feature-req))
                    (eval-or-clause? (cdr feature-req)))
                   ((eq? 'not (car feature-req))
                    (apply eval-not-clause? (cdr feature-req)))
                   (else (error "Invalid <feature requirement>"))))
                (define (do-cond-expand clauses)
                  (cond
                   ((null? clauses)  (error "Unfulfilled cond-expand"))
                   ((not (pair? (car clauses)))
                    (error "Invalid <cond-expand clause>"))
                   ((eq? 'else (caar clauses))
                    (or (null? (cdr clauses))
                        (error "else clause is not the final one"))
                    (cons '##begin (cdar clauses)))
                   ((eval-feature-req? (caar clauses))
                    (cons '##begin (cdar clauses)))
                   (else (do-cond-expand (cdr clauses)))))
                (do-cond-expand clauses)))
            (let recur ((form form))
              (cond ((null? form) '())
                    ((and (pair? form) (eq? 'cond-expand (car form)))
                     (apply expand-clauses (cdr form)))
                    ((not (pair? form)) form)
                    (else (cons (recur (car form)) (recur (cdr form))))))))
         (expand-wildcards
          (lambda (deps)
            (define map*
              (lambda (f l)
                (cond ((null? l) '())
                      ((not (pair? l)) (f l))
                      (else (cons (map* f (car l)) (map* f (cdr l)))))))

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
             (flatten-tag '##begin
                          (expand-cond-features (cdr deps-pair)))))
           '())))))

;;------------------------------------------------------------------------------
;;!! Module (unchecked functions)

;;! Throw a module format error
(define^ (%module-error module)
  (error "Error parsing module directive (wrong module format): " module))

;;! Build a module
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

;;! Normalize module to the normal form, allowing comparison of modules with different formats
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

;;! Check if module exists
(define^ (%check-module module caller)
  (or (%module? module)
      (error (string-append "Ill-defined module: " (object->string module)
                            " -> detected in " (object->string caller))))
  (let ((sphere (%module-sphere module)))
    (or (%sphere-exists? sphere)
        (error (string-append "Sphere '" (symbol->string sphere) "' not found in the system."))))
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


;;! Module equality
(define^ (%module=? m1 m2)
  (equal? (%module-normalize m1)
          (%module-normalize m2)))

;;! Module equality (without version
(define^ (%module~=? m1 m2)
  (and (equal? (%module-sphere m1)
               (%module-sphere m2))
       (equal? (%module-id m1)
               (%module-id m2))))

;;! Path of the module's sphere
(define^ (%module-path module)
  (or (%module? module) (%module-error module))
  (let ((sphere (%module-sphere module)))
    (if sphere
        (%sphere-path sphere)
        "")))

(define^ (%module-path-src module)
  (or (%module? module) (%module-error module))
  (string-append (%module-path module) (default-source-directory)))

(define^ (%module-path-lib module)
  (or (%module? module) (%module-error module))
  (string-append (%module-path module) (default-lib-directory)))

(define^ (%module-namespace module)
  (or (%module? module) (%module-error module))
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
  (let ((name (string-copy (symbol->string (%module-id module)))))
    (let recur ((i (- (string-length name) 1)))
      (if (= i 0)
          name
          (begin (and (eq? (string-ref name i) #\/)
                      (string-set! name i #\_))
                 (recur (- i 1)))))))

(define^ (%module-filename-scm module)
  (string-append (symbol->string (%module-id module))
                 (default-scm-extension)))

(define^ (%module-filename-c module #!key (version '()))
  (string-append (if (null? version)
                     (%version->string (%module-version module))
                     (%version->string version))
                 (symbol->string (%module-sphere module))
                 "__"
                 (%module-flat-name module)
                 (default-c-extension)))

(define^ (%module-filename-o module #!key (version '()))
  (string-append (if (null? version)
                     (%version->string (%module-version module))
                     (%version->string version))
                 (symbol->string (%module-sphere module))
                 "__"
                 (%module-flat-name module)
                 (default-o-extension)))


;;------------------------------------------------------------------------------
;;!! Module (checked functions)


;;! Is there a header for this module? If so, return the header module
(define^ (%module-header module)
  (%check-module module '%module-header)
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
  (%check-module module '%module-macros)
  (let ((macros-module (%make-module
                        sphere: (%module-sphere module)
                        id: (string->symbol (string-append (symbol->string (%module-id module)) "-macros"))
                        version: (%module-version module))))
    (and (file-exists?
          (string-append (%module-path-src macros-module)
                         (%module-filename-scm macros-module)))
         macros-module)))

;;! Module dependecies, as directly read from the %config
(define^ (%module-dependencies module)
  (%check-module module '%module-dependencies)
  (assq (%module-id module) (%sphere-dependencies (%module-sphere module))))

;;! Builds a procedure to get a list of dependencies of one type
;; It works by first trying the versioned module, if not found it falls back to the unversioned module
;; If the root module doesn't have a dependencies for the exact version, the unversioned dependencies will
;; be used instead. In this case, the same version requested for the root module will be tried for every
;; dependency first before falling back.
;; Resulting dependency lists are normalized modules. However, if the dependency lists contains stuff
;; other than modules, they are not normalized (obviously).
(define^ (%module-shallow-dependencies-select type find-with-postfix)
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
      (%check-module module %module-shallow-dependencies-select)
      (let ((module-sphere (%module-sphere module))
            (get-dependency-list (lambda (l) (let ((type-pair (assq type (cdr l))))
                                          (if type-pair (cdr type-pair) '()))))
            (add-found-with-postfix
             (lambda (dependency-list)
               (if find-with-postfix
                   (let ((search-module (%make-module
                                         sphere: (%module-sphere module)
                                         id: (string->symbol (string-append
                                                              (symbol->string (%module-id module))
                                                              find-with-postfix))
                                         version: (%module-version module))))
                     (if (file-exists?
                          (string-append (%module-path-src search-module)
                                         (%module-filename-scm search-module)))
                         ;; Add if file exists and another version is not already in the list
                         (let recur ((lis dependency-list))
                           (cond ((null? lis) (cons search-module dependency-list))
                                 ((%module~=? module search-module) dependency-list)
                                 (else (recur (cdr lis)))))
                         dependency-list))
                   dependency-list))))
        ;; Try first with versioned module
        (let ((versioned (find-dependencies-list module
                                                 module-sphere
                                                 (%sphere-dependencies module-sphere)
                                                 #f)))
          (if versioned
              (map (lambda (m) (if (%module? m)
                              (%module-normalize m)
                              m))
                   (add-found-with-postfix
                    (get-dependency-list versioned)))
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
                         (add-found-with-postfix
                          (get-dependency-list unversioned)))
                    (add-found-with-postfix '())))))))))

(define^ (%module-shallow-dependencies-to-prelude module)
  (%check-module module '%module-shallow-dependencies-to-prelude)
  ((%module-shallow-dependencies-select 'prelude #f) module))

(define^ (%module-shallow-dependencies-to-include module)
  (%check-module module '%module-shallow-dependencies-to-include)
  ((%module-shallow-dependencies-select 'include "-macros") module))

(define^ (%module-shallow-dependencies-to-load module)
  (%check-module module '%module-shallow-dependencies-to-load)
  ((%module-shallow-dependencies-select 'load #f) module))

(define^ (%module-shallow-dependencies-cc-options module)
  (%check-module module '%module-shallow-dependencies-cc-options)
  ((%module-shallow-dependencies-select 'cc-options #f) module))

(define^ (%module-shallow-dependencies-ld-options module)
  (%check-module module '%module-shallow-dependencies-ld-options)
  ((%module-shallow-dependencies-select 'ld-options #f) module))

(define^ (%module-shallow-dependencies-env-options module)
  (%check-module module '%module-shallow-dependencies-env-options)
  ((%module-shallow-dependencies-select 'env-options #f) module))

;;! Gets the full tree of dependencies, building a list in the right order.
;; .parameter symbol-to-follow They symbol that will look for in the dependencies,
;; following its subdependencies recursively
;; .parameter symbol-to-return The symbol that the function will record, returning
;; it after the whole dependency tree has been traversed
;; .parameter append The procedure used to append the returned results
(define^ (%module-deep-dependencies-select type-to-follow type-to-return find-with-postfix)
  (lambda (module)
    (let ((root-module (%module-normalize module))
          (visited-deps '())
          (built-deps '()))
      (let recur ((module root-module))
        (for-each recur ((%module-shallow-dependencies-select type-to-follow #f) module))
        (if (eq? type-to-follow type-to-return)
            (let ((normalized-module (%module-normalize module)))
              (or (member normalized-module built-deps)
                  (equal? normalized-module root-module)
                  (set! built-deps (append built-deps (list normalized-module)))))
            (or (member (%module-normalize module) visited-deps)
                (begin
                  (for-each
                   (lambda (d) (if (%module? d)
                              (let ((normalized-module (%module-normalize d)))
                                (or (member normalized-module built-deps)
                                    (set! built-deps (append built-deps (list normalized-module)))))
                              (or (member d built-deps)
                                  (set! built-deps (append built-deps (list d))))))
                   ((%module-shallow-dependencies-select type-to-return find-with-postfix) module))
                  (set! visited-deps (cons (%module-normalize module) visited-deps))))))
      built-deps)))

;;! Gets a list with all the dependencies to load in the right order
(define^ (%module-deep-dependencies-to-load module)
  (%check-module module '%module-deep-dependencies-to-load)
  ((%module-deep-dependencies-select 'load 'load #f) module))

;;! Gets a list with all the dependencies to include in the right order
(define^ (%module-deep-dependencies-to-include module)
  (%check-module module '%module-deep-dependencies-to-include)
  ((%module-deep-dependencies-select 'include 'include "-macros") module))

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
  (%check-module module '%module-deep-dependencies-cc-options)
  ((%module-deep-dependencies-select 'load 'cc-options #f) module))

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
  (%check-module module '%module-deep-dependencies-ld-options)
  ((%module-deep-dependencies-select 'load 'ld-options #f) module))


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
                        (expander:include (%module-filename-scm module)))))))))
       (load-single-module
        (lambda (module options)
          (%check-module module '##load-module-and-dependencies#load-single-module)
          (let ((verbose (and (memq 'verbose options) #t))
                (includes (and (memq 'includes options) #t))
                (sphere (%module-sphere module)))
            (let ((header-module (%module-header module)))
              (if header-module
                  (expander:include (string-append (%module-path-src header-module)
                                                   (%module-filename-scm header-module))))
              (if includes
                  (for-each (lambda (m) (include-single-module m '(verbose)))
                            (%module-shallow-dependencies-to-include module)))
              (if sphere
                  (let ((file-o (string-append (%sphere-path sphere) (default-lib-directory) (%module-filename-o module)))
                        (file-scm (string-append (%sphere-path sphere) (default-source-directory) (%module-filename-scm module))))
                    (cond ((file-exists? file-o)
                           (if verbose
                               (display (string-append "-- object loaded -- " (object->string module) "\n")))
                           (load file-o)
                                        ;(pp file-o)
                           file-o)
                          ((file-exists? file-scm)
                           (load file-scm)
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
   (lambda (root-module options)
     ;; Get options, as #t or #f
     (let ((omit-root (and (memq 'omit-root options) #t)))
       (let recur ((module root-module))
         (if (not (member (%module-normalize module override-version: '()) *loaded-modules*))
             (begin (for-each recur (%module-shallow-dependencies-to-load module))
                    (or (and omit-root (equal? root-module module))
                        (load-single-module module options)))))))))

;;! import-include macro
(define-macro (##spheres-include . module)
  (cond
   ((string? (car module))
    ;; If filename given, just include it (doesn't register as loaded module)
    (expander:include (car module)))
   ;; It comes quoted (it's a monster)
   ((and (pair? (car module))
         (eq? 'quote (caar module)))
    (let ((module (cadar module)))
      (%check-module module '##spheres-include)
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
      (%check-module module '##spheres-include)
      `(##include-module-and-dependencies ',module '(verbose))))))

;;; Load only module dependencies, do not load the module
;; (##define-macro (##load-module-dependencies . module)
;;   (let ((module (if (null? (cdr module))
;;                     (car module)
;;                     module)))
;;     (%check-module module)
;;     (##load-module-and-dependencies module '(omit-root verbose includes))))

;;! import macro, loads dependencies
(define-macro (##spheres-load . module)
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
    (%check-module module '##spheres-load)
    `(##load-module-and-dependencies ',module '(verbose includes))))
