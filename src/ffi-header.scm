;;; Copyright (c) 2013 by Álvaro Castro Castilla. All Rights Reserved.
;;; Copyright (c) 2012 by Álvaro Castro Castilla / Estevo Castro. All Rights Reserved.
;;; Foreign Function Interface functionality

(define^ (->string o)
  (cond ((string? o) o)
        ((symbol? o) (symbol->string o))
        ((keyword? o) (keyword->string o))
        (else (error (string-append "->string: unrecognized type -- " (object->string o))))))

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
;; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2012-February/005688.html
(##define-macro (build-c-constants . names)
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
(define^ (c-native struct-or-union
                   scheme-type
                   fields
                   #!key
                   (c-type (symbol->string scheme-type)))
  (let* ((scheme-pointer-type `(pointer ,scheme-type ,(symbol-append scheme-type '*)))
         (attribute-builder
          (lambda (fn)
            (lambda (field)
              (let ((process-arguments
                     (lambda (field)
                       (list (car field)    ; scheme-attribute
                             (cadr field)   ; scheme-attribute-type
                             (caddr field)  ; c-attribute
                             (cadddr field) ; c-attribute-type
                             (if (null? cddddr)
                                 #f
                                 (let ((pointer-arg (memq pointer?: field)))
                                   (if pointer-arg (cadr field) #f)))))))
                (apply fn (process-arguments field))))))
         (accessor
          (attribute-builder
           (lambda (scheme-attribute
               scheme-attribute-type
               c-attribute
               c-attribute-type
               pointer?)
             (let ((_voidstar (if pointer? "_voidstar" ""))
                   (ampersand (if pointer? "&" ""))
                   (return-type (if pointer?
                                    `(pointer ,scheme-attribute-type ,(symbol-append scheme-attribute-type '*))
                                    scheme-attribute-type)))
               `(define ,(symbol-append scheme-type
                                        "-"
                                        scheme-attribute)
                  (c-lambda
                   (,scheme-pointer-type) ,return-type
                   ,(generic-string-append
                     "___result" _voidstar " = "
                     ampersand "(((" c-type  "*)___arg1_voidstar)->" c-attribute ");")))))))
         (mutator
          (attribute-builder
           (lambda (scheme-attribute
               scheme-attribute-type
               c-attribute
               c-attribute-type
               pointer?)
             (let ((cast (if pointer?
                             (generic-string-append "(" c-attribute-type "*)")
                             ""))
                   (dereference (if pointer? "*" "")))
               `(define ,(symbol-append
                          scheme-type "-" scheme-attribute "-set!")
                  (c-lambda
                   (,scheme-pointer-type ,scheme-attribute-type) void
                   ,(string-append
                     "(*(" c-type "*)___arg1_voidstar)." c-attribute " = ___arg2;"))))))))
    `(begin
       ;; Type definitions
       (c-define-type ,scheme-type (,struct-or-union ,c-type ,scheme-type))
       (c-define-type ,(symbol-append scheme-type '*) ,scheme-pointer-type)
       ;; Allocator
       (define ,(symbol-append "alloc-" scheme-type)
         (c-lambda (size-t) ,scheme-pointer-type
                   ,(generic-string-append "___result_voidstar = malloc(sizeof(" c-type ") * ___arg1);")))
       ;; Type predicate
       (define (,(symbol-append scheme-type "?") x)
         (and (foreign? x)
              (memq ',scheme-type (foreign-tags x))
              #t))
       ;; Pointer type predicate
       (define (,(symbol-append scheme-type "*?") x)
         (and (foreign? x)
              (memq ',(symbol-append scheme-type '*) (foreign-tags x))
              #t))
       ;; Pointer dereference
       (define ,(symbol-append "*->" scheme-type)
         (c-lambda (,scheme-pointer-type) ,scheme-type
                   "___result_voidstar = ___arg1_voidstar;"))
       ;; Take pointer
       (define ,(symbol-append scheme-type "->*")
         (c-lambda (,scheme-type) ,scheme-pointer-type
                   "___result_voidstar = ___arg1_voidstar;"))
       ;; Attribute accessors
       ,@(map accessor fields)
       ;; Attribute mutators
       ,@(map mutator fields))))

(##define-macro (build-c-struct type #!rest fields)
  (c-native 'struct type fields))
(##define-macro (build-c-union type #!rest fields)
  (c-native 'union type fields))
(##define-macro (test-build-c-struct type #!rest fields)
  (pp (c-native 'struct type fields)))

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
                "___result = ___arg1[___arg2];"))
   `(define ,(symbol-append scheme-type '*-set!)
      (c-lambda ((pointer ,scheme-type) size-t ,scheme-type) void
                "___arg1[___arg2] = ___arg3;"))
   `(define ,(symbol-append '*-> scheme-type)
      (c-lambda ((pointer ,scheme-type #f)) ,scheme-type
                "___result = *___arg1;"))
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

;;! Automatic memory freeing macro
;; (##define-macro (with-alloc ?b ?e . ?rest)
;;   `(let ((,?b ,?e))
;;      (let ((ret (begin ,@?rest)))
;;        (free ,(car expr))
;;        ret)))
