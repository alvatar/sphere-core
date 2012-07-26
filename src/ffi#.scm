;;;=============================================================================
;;; Copyright (c) 2012 by Álvaro Castro Castilla / Estevo Castro
;;; All Rights Reserved. See LICENSE
;;;=============================================================================

(##namespace ("ffi#"
              sizeof-unsigned-char
              void*-offset
              ->void*
              void*->unsigned-char*
              calloc
              malloc
              realloc
              free
              make-unsigned-int*
              unsigned-int*-ref
              unsigned-int*-set!
              vector->unsigned-int* vec
              ))

;;; C constants generation macro

;; Creating the bindings in a simple C function makes for more compact
;; binaries, as per Marc Feeley's advice.
;;
;; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2012-February/005688.html
;; (Via 'Alvaro Castro-Castilla).
(define-macro
  (c-constants . names)
  (define (interval lo hi)
    (if (< lo hi) (cons lo (interval (+ lo 1) hi)) '()))
  (let ((nb-names (length names))
        (wrapper (gensym)))
    `(begin
       (define ,wrapper
         (c-lambda (int)
                   int
                   ,(string-append
                      "static int _tmp_[] = {\n"
                      (apply string-append
                             (map (lambda (i name)
                                    (let ((name-str (symbol->string name)))
                                      (string-append
                                        (if (> i 0) "," "")
                                        name-str)))
                                  (interval 0 nb-names)
                                  names))
                      "};\n"
                      "___result = _tmp_[___arg1];\n")))
       ,@(map (lambda (i name)
                `(define ,name (,wrapper ,i)))
              (interval 0 nb-names)
              names))))

;;; Struct and union generation macro
;;; (code by Estevo Castro)

; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2009-August/003781.html
(define-macro (at-expand-time-and-runtime . exprs)
  (let ((l `(begin ,@exprs)))
    (eval l)
    l))

(define-macro (at-expand-time . expr)
  (eval (cons 'begin expr)))

(c-declare #<<c-declare-end

___SCMOBJ ffimacro__leave_alone(void *p);
___SCMOBJ ffimacro__free_foreign(void *p);

c-declare-end
)

(at-expand-time
  (define (to-string x)
    (cond ((string? x) x)
          ((symbol? x) (symbol->string x))
          (else (error "Unsupported type: " x))))
  (define (mixed-append . args) (apply string-append (map to-string args)))
  (define (symbol-append . args)
    (string->symbol (apply mixed-append args)))
  (define managed-prefix "managed-")
  (define unmanaged-prefix "unmanaged-")
  (define (c-native struct-or-union type . fields)
    (let*
      ((scheme-type (if (pair? type) (car type) type))
       (pointer-type (symbol-append scheme-type "*"))
       (c-type (if (pair? type) (cadr type) type))
       (c-type-name (symbol->string c-type))
       (attr-worker
         (lambda (fn)
           (lambda (field)
             (let* ((attr (car field))
                    (scheme-attr-name (symbol->string (if (pair? attr)
                                                        (car attr)
                                                        attr)))
                    (c-attr-name (symbol->string (if (pair? attr)
                                                   (cadr attr)
                                                   attr)))
                    (attr-type (cadr field))
                    (scheme-attr-type (if (pair? attr-type)
                                        (car attr-type)
                                        attr-type))
                    (c-attr-type (if (pair? attr-type)
                                   (cadr attr-type)
                                   attr-type))
                    (access-type (if (null? (cddr field))
                                   'scalar
                                   (caddr field)))
                    (voidstar (eq? access-type 'voidstar))
                    (pointer (eq? access-type 'pointer)))
               (fn scheme-attr-name
                   c-attr-name
                   scheme-attr-type
                   c-attr-type
                   voidstar
                   pointer)))))
       (accessor
         (attr-worker
           (lambda (scheme-attr-name c-attr-name scheme-attr-type c-attr-type
                                     voidstar pointer)
             (let ((_voidstar (if (or voidstar pointer) "_voidstar" ""))
                   (amperstand (if voidstar "&" ""))
                   (scheme-attr-type (if voidstar
                                       (symbol-append unmanaged-prefix
                                                      scheme-attr-type)
                                       scheme-attr-type)))
               `(define (,(symbol-append scheme-type
                                         "-"
                                         scheme-attr-name)
                          parent)
                  (let ((ret
                          ((c-lambda
                             (,scheme-type) ,scheme-attr-type
                             ,(string-append
                                "___result" _voidstar
                                ; XXX: correctly cast to type, should help with enums in C++.
                                ;" = (" (symbol->string c-attr-type) ")"
                                " = "
                                amperstand "(((" c-type-name
                                "*)___arg1_voidstar)->"
                                c-attr-name ");"))
                           parent)))
                    ,@(if voidstar
                        '((ffi#link parent ret))
                        '())
                    ret))))))
       (mutator
         (attr-worker
           (lambda (scheme-attr-name c-attr-name scheme-attr-type c-attr-type
                                     voidstar pointer)
             (let ((_voidstar (if (or voidstar pointer) "_voidstar" ""))
                   (cast
                     (cond
                       (voidstar
                         (mixed-append "(" c-attr-type "*)"))
                       (pointer
                         (mixed-append "(" c-attr-type ")"))
                       ; XXX: cast primitive types too, should help with enums in C++
                       (else "")))
                   (dereference (if voidstar "*" "")))
               `(define ,(symbol-append
                           scheme-type "-" scheme-attr-name "-set!")
                  (c-lambda
                    (,scheme-type ,scheme-attr-type) void
                    ,(string-append
                       "(*(" c-type-name "*)___arg1_voidstar)." c-attr-name
                       " = " dereference cast "___arg2" _voidstar ";"))))))))
      (append
        `(begin
           (c-define-type ,scheme-type (,struct-or-union ,c-type-name ,c-type))
           ; Unmanaged version of structure.
           (c-define-type ,(symbol-append unmanaged-prefix scheme-type)
                          (,struct-or-union ,c-type-name ,c-type "ffimacro__leave_alone"))
           (c-define-type
             ,pointer-type
             (pointer ,scheme-type ,pointer-type))
           (c-define-type
             ,(symbol-append managed-prefix pointer-type)
             (pointer ,scheme-type ,pointer-type "ffimacro__free_foreign"))
           (define ,(symbol-append "make-" scheme-type)
             ; Constructor.
             (c-lambda
               () ,scheme-type
               ,(string-append "___result_voidstar = malloc(sizeof(" c-type-name "));")))
           (define (,(symbol-append scheme-type "?") x)
             ; Type predicate.
             (and (foreign? x) (memq (quote ,c-type) (foreign-tags x)) #t))
           (define (,(symbol-append scheme-type "-pointer?") x)
             ; Pointer type predicate.
             (and (foreign? x)
                  (memq (quote ,pointer-type)
                        (foreign-tags x))
                  #t))
           (define (,(symbol-append scheme-type "-pointer") x)
             ; Take pointer.
             (let ((ret
                     ((c-lambda
                        (,scheme-type) ,pointer-type
                        "___result_voidstar = ___arg1_voidstar;")
                      x)))
               (ffi#link x ret)
               ret))
           (define (,(symbol-append "pointer->" scheme-type) x)
             ; Pointer dereference
             (let ((ret
                     ((c-lambda
                        (,pointer-type) ,(symbol-append unmanaged-prefix scheme-type)
                        "___result_voidstar = ___arg1_voidstar;")
                      x)))
               (ffi#link x ret)
               ret))
           (define ,(symbol-append "make-" scheme-type "-array")
             (c-lambda
               (int) ,(symbol-append managed-prefix pointer-type)
               ,(string-append
                  "___result_voidstar = malloc(___arg1 * sizeof(" c-type-name "));")))
           (define (,(symbol-append scheme-type "-pointer-offset") p i)
             (let ((ret
                     ((c-lambda
                       (,pointer-type int) ,pointer-type
                       ,(string-append "___result_voidstar = (" c-type-name "*)___arg1_voidstar + ___arg2;"))
                      p i)))
               (ffi#link p ret)
               ret)))
        (map accessor fields)
        (map mutator fields)))))

(define-macro
  (c-struct type . fields)
  (apply c-native 'struct type fields))

(define-macro
  (c-union type . fields)
  (apply c-native 'union type fields))

;;; Common types

(c-define-type size-t unsigned-int)
(c-define-type unsigned-int* (pointer unsigned-int))

;;; Build a size-of value equivalent to the C operator
;;; c-build-sizeof float -> sizeof-float

(define-macro c-build-sizeof
  (lambda (type)
    (let ((type-str (symbol->string type)))
      `(define ,(string->symbol (string-append "sizeof-" type-str))
         ((c-lambda () unsigned-int
                    ,(string-append "___result = sizeof(" type-str ");")))))))

;;; Automatic memory freeing macro

(define-macro (with-alloc ?b ?e . ?rest)
  `(let ((,?b ,?e))
     (let ((ret (begin ,@?rest)))
       (free ,(car expr))
       ret)))
