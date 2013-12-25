;;;; Sourcification and Desourcification

(define riaxpander:source-mapping (make-parameter #f))

(define (riaxpander:desourcify form)
  (cond ((riaxpander:source-mapping)
         => (lambda (mapping)
              (let recur ((form form))
                (cond ((##source? form)
                       (let ((code (##source-code form)))
                         (cond ((pair? code)
                                (table-set! mapping code (##source-locat form))
                                (cons (recur (car code))
                                      (recur (cdr code))))
                               ((symbol? code)
                                code)   ;++ fix
                               (else code))))
                      ((pair? form)
                       (cons (recur (car form))
                             (recur (cdr form))))
                      (else form)))))
        (else (##desourcify form))))

;; (define (riaxpander:resourcify form history)
;;   (cond ((not (pair? form)) form)
;;         ((riaxpander:source-mapping)
;;          => (lambda (mapping)
;;               (if history
;;                   (cond ((table-ref mapping
;;                                     (reduction/form
;;                                      (history/original-reduction history)))
;;                          => (lambda (location)
;;                               (##make-source form location)))
;;                         (else form))
;;                   form)))
;;         (else form)))

(define (riaxpander:expand form environment)
  ((lambda (results)
     ((if (compiling-expander)
          ##sourcify-deep
          ##sourcify)
      (let ((r (if (and (pair? results)
                        (null? (cdr results))
                        (not (compiling-expander)))
                   (car results)
                   `(##begin ,@results))))
        r)
      (##make-source #f #f)))
   (parameterize ((current-location-uid 0))
                 (map (lambda (item)
                        (cond ((binding? item) (gambit/compile-binding item))
                              ((declaration? item) (gambit/compile-declaration item))
                              ((expression? item) (gambit/compile-expression item))
                              (else (error "Invalid top-level item:" item))))
                      (let ((forms (list (##desourcify form)))) 
                        (scan-top-level identity-selector
                                        forms
                                        environment
                                        (make-top-level-history forms environment)))))))

(define compiling-expander (make-parameter #f))

(define (riaxpander:expand-toplevel form)
  (set! new-syntax-definitions '())
  (let ((form* (riaxpander:expand form
                                  (or riaxpander:top-level-environment
                                      (make-gambit-environment)))))
    (cond
     ((and (pair? new-syntax-definitions)
           (compiling-expander))
      ;; include code to install the compiled macros at load time
      (let ((macro*+form* (##sourcify-deep
                `(##begin ,@(filter-map expand-toplevel-syntax-definition
                                        new-syntax-definitions)
                          ,(##desourcify form*)) (##make-source #f #f))))
        macro*+form*))
     (else
      form*))))

(define (riaxpander:include file)
  (for-each eval (with-input-from-file
                     file
                   (lambda () (read-all)))))

(define (expand-toplevel-syntax-definition name)
  (let ((transformer (syntactic-lookup riaxpander:top-level-environment name)))
    (if (transformer? transformer)
        (let ((source (transformer/source transformer)))
          (if source
              (let ((form (strip-syntactic-closures source riaxpander:top-level-environment)))
                (cond
                 ((pair? form)
                  (case (car form)
                    ((rsc-macro-transformer)
                     (expand-rsc-macro-transformer name form))
                    ((sc-macro-transformer)
                     (expand-sc-macro-transformer name form))
                    ((er-macro-transformer)
                     (expand-er-macro-transformer name form))
                    (else
                     (warning "non-standard syntax transformer" name form)
                     `(syntactic-bind! riaxpander:top-level-environment ',name ,form))))
                 (else
                  (warning "non-standard syntax transformer" name form)
                  `(syntactic-bind! riaxpander:top-level-environment ',name ,form)))))))))

(define (expand-rsc-macro-transformer name form)
  (let ((expr (gensym 'expr))
        (use-env (gensym 'use-env))
        (close-env (gensym 'close-env)))
    `(syntactic-bind!
      riaxpander:top-level-environment
      ',name
      (make-transformer riaxpander:top-level-environment
                        ,(and (pair? (cddr form)) (list 'quote (caddr form)))
                        (lambda (,expr ,use-env ,close-env)
                          (,(cadr form) ,expr ,close-env))
                        #f))))

(define (expand-sc-macro-transformer name form)
  (let ((expr (gensym 'expr))
        (use-env (gensym 'use-env))
        (close-env (gensym 'close-env)))
    `(syntactic-bind!
      riaxpander:top-level-environment
      ',name
      (make-transformer riaxpander:top-level-environment
                        ,(and (pair? (cddr form)) (list 'quote (caddr form)))
                        (lambda (,expr ,use-env ,close-env)
                          (close-syntax (,(cadr form) ,expr ,use-env)
                                        ,close-env))
                        #f))))

(define (expand-er-macro-transformer name form)
  (let ((expr (gensym 'expr))
        (use-env (gensym 'use-env))
        (close-env (gensym 'close-env)))
    `(syntactic-bind!
      riaxpander:top-level-environment
      ',name
      (make-transformer riaxpander:top-level-environment
                        ,(and (pair? (cddr form)) (list 'quote (caddr form)))
                        (lambda (,expr ,use-env ,close-env)
                          (,(cadr form)
                           ,expr
                           (make-alias-generator ,close-env)
                           (make-name-comparator ,use-env)))
                        #f))))

;; XXXX use qualified names - since we currently have no modules and
;; all top-level macros just reference the same top-level namespace,
;; this isn't essential yet
(define (strip-syntactic-closures expr env)
  (let strip ((x expr))
    (cond
     ((pair? x)
      ;; XXXX hygienically strip syntax-quote
      (if (or (eq? 'syntax-quote (car x))
              (and (syntactic-closure? (car x))
                   (eq? 'syntax-quote (syntactic-closure/form (car x)))))
          (list 'quote (strip (cadr x)))
          (cons (strip (car x)) (strip (cdr x)))))
     ((syntactic-closure? x)
      (strip (syntactic-closure/form x)))
     (else
      x))))

(define (riaxpander:c-expand-toplevel form)
  (parameterize ((compiling-expander #t))
                (riaxpander:expand-toplevel form)))

;; (trace riaxpander:c-expand-toplevel)
;; (trace riaxpander:expand-toplevel)


(define riaxpander:top-level-environment #f)
(define new-syntax-definitions '())

;;; This must be defined in case the compiler isn't loaded, so that
;;; the following assignment is not an error.

(define c#expand-source (lambda (src) src))

(define (riaxpander:install)
  (set! riaxpander:top-level-environment (make-gambit-environment))
  (set! c#expand-source riaxpander:c-expand-toplevel)
  (set! ##expand-source riaxpander:expand-toplevel))

(define (gambit/meta-evaluate expression environment)
  (eval expression (interaction-environment)))

(define (gambit/compile-reference variable reference environment)
  (if (not variable)
      (name->symbol reference)
      (let ((name (variable/name variable))
            (location (variable/location variable)))
        (cond ((number? location)
               (string->symbol
                (string-append (number->string location)
                               "#"
                               (symbol->string (name->symbol name)))))
              ((name? location)
               ;** Note that this strips the information necessary to
               ;** resolve hygienic module references later.
               (name->symbol location))
              (else
               (error "Variable has bogus location:"
                      variable reference environment))))))

(define current-location-uid (make-parameter 0))

(define (gambit/allocate-location environment name)
  (if (not (syntactic-environment/parent environment))
      name
      (let ((uid (current-location-uid)))
        (current-location-uid (+ uid 1))
        uid)))

;;;; S-Expression Syntactic Environment

(define (make-gambit-environment)
  (let ((environment
         (make-syntactic-environment gambit/syntactic-operations
                                     gambit/syntactic-parameters
                                     #f
                                     '())))
    (apply-macrology (gambit-macrology) environment)
    environment))

(define (gambit-macrology)
  (compose-macrologies
   (macrology/standard-assignment)
   (macrology/standard-conditional gambit/compile-conditional)
   (macrology/standard-definition)
   (macrology/standard-derived-syntax)
   (macrology/standard-keyword-definition)
   (macrology/standard-lambda gambit/compile-lambda gambit/map-lambda-bvl)
   (macrology/standard-quotation gambit/compile-quotation)
   (macrology/standard-sequence)
   (macrology/standard-syntactic-binding)
   (macrology/syntax-rules)
   (gambit-extensions-macrology)))

(define gambit/syntactic-operations
  (let ()
    (define (global-bindings environment)
      (syntactic-environment/data environment))
    (define (set-global-bindings! environment bindings)
      (set-syntactic-environment/data! environment bindings))
    (make-syntactic-operations
     (lambda (environment name)              ;lookup
       (cond ((assq name (global-bindings environment))
              => cdr)
             ((syntactic-closure? name)
              (syntactic-lookup (syntactic-closure/environment name)
                                (syntactic-closure/form name)))
             (else #f)))
     (lambda (environment name denotation)   ;bind!
       (set! new-syntax-definitions
             (cons name new-syntax-definitions))

       (set-global-bindings! environment
                             (cons (cons name denotation)
                                   (global-bindings environment))))
     (lambda (environment)                   ;seal!
       environment                      ;ignore
       (if #f #f))
     (lambda (environment name)              ;alias
       environment                      ;ignore
       name)
     (lambda (environment)                   ;disclose
       environment                      ;ignore
       '(sexp))
     (lambda (environment procedure)         ;for-each-binding
       (for-each (lambda (binding)
                   (procedure (car binding) (cdr binding)))
                 (global-bindings environment))))))

(define gambit/syntactic-parameters
  (lambda (key)
    (cond ((eq? key variable-classifier) gambit/classify-variable)
          ((eq? key free-variable-classifier) gambit/classify-free-variable)
          ((eq? key datum-classifier) gambit/classify-datum)
          ((eq? key self-evaluating?) gambit/self-evaluating?)
          ((eq? key combination-classifier) gambit/classify-combination)
          ((eq? key location-allocator) gambit/allocate-location)
          ((eq? key meta-evaluator) gambit/meta-evaluate)
          (else #f))))

;;;; Gambit Specifics

(define-record-type <declaration>
    (make-declaration forms environment history)
    declaration?
  (forms declaration/forms)
  (environment declaration/environment)
  (history declaration/history))

(define (make-declaration-definition selector forms environment history)
  selector                              ;ignore
  (make-definition
   (lambda (definition-environment)
     definition-environment             ;ignore
     (list (make-declaration forms environment history)))))

(define (gambit/compile-declaration declaration)
  ;++ Can any Gambit declarations affect local variables?  If so, this
  ;++ needs to go through the list of forms to rename any references to
  ;++ them.  (This is why we include the environment.)
  `(declare ,@(declaration/forms declaration)))

(define (macrology/gambit-sequence)
  (make-extended-classifier-macrology
   (lambda (define-classifier)
     (define-classifier '(##begin * form)
       (lambda (form environment history)
         (classify-sequence cdr-selector (cdr form) environment history))))))

(define (macrology/gambit-define)
  (make-extended-classifier-macrology
   (lambda (define-classifier)
     (define-classifier '(declare + (name * datum))
       (lambda (form environment history)
         (values (make-declaration-definition cdr-selector
                                              (cdr form)
                                              environment
                                              history)
                 history))))))

(define (macrology/gambit-cond-expand)
  (make-extended-classifier-macrology
   (lambda (define-classifier)
     (define-classifier  '(cond-expand + (expression + expression))
       (lambda (form environment history)
         (let ((clauses (cdr form))
               (clauses-selector cdr-selector))
           (define (err x) 
             (syntax-error "syntax error in COND-EXPAND form"
                           x (cons 'cond-expand clauses)))
           (define (test feature-requirement)
             (cond ((symbol? feature-requirement)
                    (not (not (memq feature-requirement ##cond-expand-features))))
                   ((not (pair? feature-requirement))
                    (err feature-requirement))
                   (else
                    (let ((head (car feature-requirement))
                          (rest (cdr feature-requirement)))
                      (case head
                        ((and) (or (eq? rest '())
                                   (if (pair? rest)
                                       (and (test (car rest))
                                            (test `(and ,@(cdr rest))))
                                       (err feature-requirement))))
                        ((or) (and (not (eq? rest '()))
                                   (if (pair? rest)
                                       (or (test (car rest))
                                           (test `(or ,@(cdr rest))))
                                       (err feature-requirement))))
                        ((not) (not (test (cadr feature-requirement))))
                        (else (err feature-requirement)))))))
           (let expand ((clauses clauses)
                        (clauses-selector clauses-selector))
             (cond ((eq? clauses '())
                    (apply syntax-error `("no matching clause in `cond-expand' form"
                                          ,(map (lambda (x) (car x)) clauses))))
                   ((not (pair? clauses)) (err clauses))
                   (else
                    (let ((clause (car clauses))
                          (clause-selector (select-car clauses-selector))
                          (rclauses (cdr clauses))
                          (rclauses-selector (select-cdr clauses-selector)))
                      (if (not (pair? clause))
                          (err clause)
                          (let ((feature-requirement (car clause))
                                (expressions (cdr clause))
                                (expressions-selector (select-cdr clause-selector)))
                            (cond ((eq? feature-requirement 'else)
                                   (if (eq? expressions '())
                                       (err "invalid ELSE <expression>")
                                       (classify-sequence expressions-selector expressions environment history)))
                                  ((test feature-requirement)
                                   (classify-sequence expressions-selector expressions environment history))
                                  (else (expand rclauses rclauses-selector)))))))))))))))

(define (gambit-extensions-macrology)
  (compose-macrologies
   (macrology/non-standard-macro-transformers)
   (macrology/syntax-quote 'syntax-quote gambit/compile-quotation)
   (macrology/gambit-sequence)
   (macrology/gambit-cond-expand)
   (macrology/gambit-define)))

;;;;; S-Expression Syntactic Parameters

(define (gambit/classify-datum datum environment history)
  (if (gambit/self-evaluating? datum)
      (values (make-expression (lambda () datum)) history)
      (classify-error "Inevaluable datum:" history datum)))

(define (gambit/self-evaluating? datum)
  (or (boolean? datum)
      (char? datum)
      (number? datum)
      (string? datum)
      (keyword? datum)
      (memq datum
            '(
              #!eof         ;ARGH!  What *stupid* lossage.
              #!key
              #!rest
              #!optional
              #!unbound
              #!unbound2
              #!void
              ))))

(define (gambit/classify-variable variable reference environment history)
  (values
   (make-location
    (lambda () (gambit/compile-reference variable reference environment))
    (lambda (expression assignment-history)
      assignment-history                ;ignore
      `(set! ,(gambit/compile-reference variable reference environment)
             ,(gambit/compile-expression expression))))
   history))

(define (gambit/classify-free-variable reference environment history)
  (gambit/classify-variable #f reference environment history))

(define (gambit/classify-combination operator operator-history
                                     selector forms environment history)
  (cond ((not (expression? operator))
         (classify-error "Non-expression in operator position of combination:"
                         operator-history
                         operator))
        ((not (list? forms))
         (classify-error "Invalid operands in combination -- improper list:"
                         history
                         forms))
        (else
         (values
          (make-expression
           (lambda ()
             (gambit/compile-combination
              operator
              (classify-subexpressions selector forms environment history)
              history)))
          history))))

;;;;; S-Expression Compilers

(define (gambit/compile-quotation datum history)
  history                               ;ignore
  (if (gambit/self-evaluating? datum)
      datum
      `',datum))

(define (gambit/compile-conditional condition consequent alternative history)
  history                               ;ignore
  `(if ,(gambit/compile-expression condition)
       ,(gambit/compile-expression consequent)
       ,@(if alternative
             `(,(gambit/compile-expression alternative))
             '())))

(define (gambit/compile-lambda bvl body environment history)
  history                               ;ignore
  `(lambda ,(gambit/%map-lambda-bvl bvl
              (lambda (variable)
                (gambit/compile-reference variable #f environment)))
     ,@(gambit/compile-lambda-body body)))

(define (gambit/compile-lambda-body body)
  (receive (definitions expressions) (classify/sequence scan-r6rs-body body)
    `(,@(map (lambda (item)
               (cond ((binding? item) (gambit/compile-binding item))
                     ((declaration? item) (gambit/compile-declaration item))
                     (else (error "Invalid item in body:" item))))
             definitions)
      ,@(map gambit/compile-expression expressions))))

;++ Handle extended BVLs.

(define (gambit/map-lambda-bvl bvl history procedure)
  (if (not (gambit/valid-bvl? bvl))
      (syntax-error "Invalid lambda BVL:" history bvl)
      (gambit/%map-lambda-bvl bvl procedure)))

(define (gambit/valid-bvl? bvl)
  (let loop ((bvl bvl) (seen '()))
    (if (pair? bvl)
        (and (name? (car bvl))
             (not (memq (car bvl) seen))
             (loop (cdr bvl) (cons (car bvl) seen)))
        (or (null? bvl)
            (and (name? bvl)
                 (not (memq bvl seen)))))))

(define (gambit/%map-lambda-bvl bvl procedure)
  (let recur ((bvl bvl))
    (cond ((pair? bvl)
           (cons (procedure (car bvl))
                 (recur (cdr bvl))))
          ((null? bvl)
           '())
          (else
           (procedure bvl)))))

;;;;; S-Expression Compilation Utilities

(define (gambit/compile-expression expression)
  (cond ((location? expression)
         ((location/expression-compiler expression)))
        ((sequence? expression)
         (gambit/compile-sequence
          (classify/sequence scan-expressions expression)
          (sequence/history expression)))
        (else
         ((expression/compiler expression)))))

(define (gambit/compile-expressions expressions)
  (map gambit/compile-expression expressions))

(define (gambit/compile-combination operator operands history)
  history                               ;ignore
  `(,(gambit/compile-expression operator)
    ,@(gambit/compile-expressions operands)))

(define (gambit/compile-binding binding)
  `(define ,(gambit/compile-reference (binding/variable binding)
                                      #f
                                      (binding/environment binding))
     ,(receive (expression history) ((binding/classifier binding))
        history                         ;ignore
        (gambit/compile-expression expression))))

(define (gambit/compile-sequence expressions history)
  history                               ;ignore
  `(##begin ,@(map gambit/compile-expression expressions)))

;; (define-syntax define-macro
;;   (syntax-rules ()
;;     ((_ (id . llist) . body) 
;;      (define-syntax id
;;        (rsc-macro-transformer 
;;  (lambda (exp env)
;;    (apply (lambda llist . body) (cdr exp))))))
;;     ((_ id expander) 
;;      (define-syntax id
;;        (rsc-macro-transformer 
;;  (lambda (exp env)
;;    (apply expander (cdr exp))))))))

;; (define-syntax include
;;   (rsc-macro-transformer
;;    (lambda (exp env)
;;      (syntax-check '(keyword expression) exp)
;;      (let ((filename (cadr exp)))
;;        (let ((path (##sys#resolve-include-filename filename #t)))
;;   (if (load-verbose) (print "; including " path " ..."))
;;   `(,(make-syntactic-closure env '() 'begin)
;;     ,@(with-input-from-file path
;;         (lambda ()
;;     (do ((x (read) (read))
;;          (xs '() (cons x xs)))
;;         ((eof-object? x) 
;;          (reverse xs)))))))))))
