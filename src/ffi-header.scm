;;; Copyright (c) 2013 by Álvaro Castro Castilla. All Rights Reserved.
;;; Copyright (c) 2012 by Álvaro Castro Castilla / Estevo Castro. All Rights Reserved.
;;; Foreign Function Interface functionality

(define^ (make-symbol #!rest elems)
  (string->symbol (apply string-append
                         (map (lambda (e)
                                (cond ((symbol? e) (symbol->string e))
                                      ((string? e) e)
                                      (else (error "make-symbol: unsupported type"))))
                              elems))))

(define^ (->string o)
  (cond ((string? o) o)
        ((symbol? o) (symbol->string o))
        ((keyword? o) (keyword->string o))
        (else (error (string-append "->string: unrecognized type " o)))))

(define^ (generic-string-append #!rest ol)
  (apply string-append (map ->string ol)))

(define^ (symbol-append #!rest ol)
  (string->symbol (apply generic-string-append ol)))

(define^ (build-defines #!rest define-blocks)
  (cons 'begin
        (let recur ((ds define-blocks))
          (cond ((null? ds) '())
                ((null? (car ds)) (recur (cdr ds)))
                (else (cons (car ds)
                            (recur (cdr ds))))))))

;-------------------------------------------------------------------------------
; Types
;-------------------------------------------------------------------------------

(c-define-type void* (pointer void))
(c-define-type bool* (pointer bool))
(c-define-type short* (pointer short))
(c-define-type unsigned-short* (pointer unsigned-short))
(c-define-type int* (pointer int))
(c-define-type unsigned-int* (pointer unsigned-int))
(c-define-type long* (pointer long))
(c-define-type unsigned-long* (pointer unsigned-long))
(c-define-type long-long* (pointer long-long))
(c-define-type unsigned-long-long* (pointer unsigned-long-long))
(c-define-type float* (pointer float))
(c-define-type double* (pointer double))
(c-define-type char* (pointer char))
(c-define-type char** (pointer char*))
(c-define-type unsigned-char* (pointer unsigned-char))
(c-define-type unsigned-char** (pointer unsigned-char*))
(c-define-type int8* (pointer int8))
(c-define-type unsigned-int8* (pointer unsigned-int8))
(c-define-type int16* (pointer int16))
(c-define-type unsigned-int16* (pointer unsigned-int16))
(c-define-type int32* (pointer int32))
(c-define-type unsigned-int32* (pointer unsigned-int32))
(c-define-type int64* (pointer int64))
(c-define-type unsigned-int64* (pointer unsigned-int64))
(c-define-type size-t unsigned-long-long)

;-------------------------------------------------------------------------------
; FFI generation
;-------------------------------------------------------------------------------

;;! C constants generation macro
;; Creating the bindings in a simple C function makes for more compact
;; binaries, as per Marc Feeley's advice.
;;
;; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2012-February/005688.html
(##define-macro
  (c-constants . names)
  (let ((nb-names (length names))
        (wrapper (gensym)))
    (letrec ((interval (lambda (lo hi)
                         (if (< lo hi) (cons lo (interval (+ lo 1) hi)) '()))))
      `(begin
         (##define ,wrapper
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
                  `(##define ,name (,wrapper ,i)))
                (interval 0 nb-names)
                names)))))

;;! Struct and union generation macro
;; (code by Estevo Castro)
(define^ (c-native struct-or-union type fields)
  (letrec ((to-string (lambda (x)
                        (cond ((string? x) x)
                              ((symbol? x) (symbol->string x))
                              (else (error "Unsupported type: " x)))))
           (mixed-append (lambda args
                           (apply string-append (map to-string args))))
           (symbol-append (lambda args
                            (string->symbol (apply mixed-append args)))))
    (let*
        ((managed-prefix "managed-")
         (unmanaged-prefix "unmanaged-")
         (scheme-type (if (pair? type) (car type) type))
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
                   (ampersand (if voidstar "&" ""))
                   (scheme-attr-type (if voidstar
                                         (symbol-append unmanaged-prefix
                                                        scheme-attr-type)
                                         scheme-attr-type)))
               `(##define (,(symbol-append scheme-type
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
                             ampersand "(((" c-type-name
                             "*)___arg1_voidstar)->"
                             c-attr-name ");"))
                          parent)))
                    ,@(if voidstar
                          '((ffi:link parent ret))
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
               `(##define ,(symbol-append
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
          (##define ,(symbol-append "make-" scheme-type)
                                        ; Constructor.
            (c-lambda
             () ,scheme-type
             ,(string-append "___result_voidstar = malloc(sizeof(" c-type-name "));")))
          (##define (,(symbol-append scheme-type "?") x)
                                        ; Type predicate.
            (and (foreign? x) (memq (quote ,c-type) (foreign-tags x)) #t))
          (##define (,(symbol-append scheme-type "-pointer?") x)
                                        ; Pointer type predicate.
            (and (foreign? x)
                 (memq (quote ,pointer-type)
                       (foreign-tags x))
                 #t))
          (##define (,(symbol-append scheme-type "-pointer") x)
                                        ; Take pointer.
            (let ((ret
                   ((c-lambda
                     (,scheme-type) ,pointer-type
                     "___result_voidstar = ___arg1_voidstar;")
                    x)))
              (ffi:link x ret)
              ret))
          (##define (,(symbol-append "pointer->" scheme-type) x)
                                        ; Pointer dereference
            (let ((ret
                   ((c-lambda
                     (,pointer-type) ,(symbol-append unmanaged-prefix scheme-type)
                     "___result_voidstar = ___arg1_voidstar;")
                    x)))
              (ffi:link x ret)
              ret))
          (##define ,(symbol-append "make-" scheme-type "-array")
            (c-lambda
             (int) ,(symbol-append managed-prefix pointer-type)
             ,(string-append
               "___result_voidstar = malloc(___arg1 * sizeof(" c-type-name "));")))
          (##define (,(symbol-append scheme-type "-pointer-offset") p i)
            (let ((ret
                   ((c-lambda
                     (,pointer-type int) ,pointer-type
                     ,(string-append "___result_voidstar = (" c-type-name "*)___arg1_voidstar + ___arg2;"))
                    p i)))
              (ffi:link p ret)
              ret)))
       (map accessor fields)
       (map mutator fields)))))

(##define-macro
  (c-struct . type.fields)
  (c-native 'struct (car type.fields) (cdr type.fields)))

(##define-macro
  (c-union . type.fields)
  (c-native 'union (car type.fields) (cdr type.fields)))

;;! Build a size-of value equivalent to the C operator
;; c-build-sizeof float -> sizeof-float

(##define-macro (build-c-sizeof scheme-type #!key (c-type (symbol->string scheme-type)))
  `(define ,(symbol-append scheme-type '-size)
     ((c-lambda () size-t
                ,(string-append "___result = sizeof(" c-type ");")))))

;;! Build FFI procedures for C type arrays
;; (build-c-array-ffi float f32) ->
;; alloc-float*
;; float*-ref
;; float*-set!
;; *->float*
;; f32vector->float*
(##define-macro (build-c-array-ffi scheme-type #!key (c-type scheme-type) scheme-vector)
  (build-defines
   `(define ,(symbol-append 'alloc- scheme-type '*)
      (c-lambda (size-t) (pointer ,scheme-type)
                ,(generic-string-append "___result_voidstar = malloc(___arg1*sizeof(" c-type "));")))
   `(define ,(symbol-append scheme-type '*-ref)
      (c-lambda ((pointer ,scheme-type) size-t) ,scheme-type
                ,(generic-string-append "___result = ___arg1[___arg2];")))
   `(define ,(symbol-append scheme-type '*-set!)
      (c-lambda ((pointer ,scheme-type) size-t ,scheme-type) void
                ,(generic-string-append "___arg1[___arg2] = ___arg3;")))
   `(define ,(symbol-append '*-> scheme-type)
      (c-lambda ((pointer ,scheme-type #f)) ,scheme-type
                ,(generic-string-append "___result = *___arg1;")))
   (if scheme-vector
       `(define (,(symbol-append scheme-vector 'vector-> scheme-type '*) vec)
         (let* ((length (,(symbol-append scheme-vector 'vector-length) vec))
                (buf (,(symbol-append 'alloc- scheme-type '*) length)))
           (let loop ((i 0))
             (if (fx< i length)
                 (begin
                   (,(symbol-append scheme-type '*-set!) buf i (,(symbol-append scheme-vector 'vector-ref) vec i))
                   (loop (fx+ i 1)))
                 buf))))
       '())))

;;; Automatic memory freeing macro

;; (##define-macro (with-alloc ?b ?e . ?rest)
;;   `(let ((,?b ,?e))
;;      (let ((ret (begin ,@?rest)))
;;        (free ,(car expr))
;;        ret)))


 
  
(c-declare #<<c-declare-end

#ifndef FFIMACRO
#define FFIMACRO

#include <malloc.h>

___SCMOBJ ffimacro__leave_alone(void *p)
{
  return ___FIX(___NO_ERR);
}

___SCMOBJ ffimacro__free_foreign(void *p)
{
  if (p)
    free(p);
  return ___FIX(___NO_ERR);
}

#endif

c-declare-end
)

;; (c-declare #<<c-declare-end
;;            ___SCMOBJ ffimacro__leave_alone(void *p)  ;
;;            ___SCMOBJ ffimacro__free_foreign(void *p) ;
;; c-declare-end
;; )
