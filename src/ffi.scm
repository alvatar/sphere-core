;;; Copyright (c) 2012 by Álvaro Castro Castilla / Estevo Castro. All Rights Reserved.
;;; Foreign Function Interface functionality

;-------------------------------------------------------------------------------
; Code needed by FFI-generation macros
;-------------------------------------------------------------------------------

(##include "../src/ffi-header.scm")

(define ffi:references
  (if (table? ffi:references)
    ffi:references
    (make-table weak-keys: #t weak-values: #f test: eq?)))

(define ffi:link
  (if (procedure? ffi:link)
    ffi:link
    (lambda (parent child)
      (table-set! ffi:references child parent)
      (make-will child (lambda (x) (table-set! ffi:references x))))))

;-------------------------------------------------------------------------------
; Gambit memory
;-------------------------------------------------------------------------------

;; The C interface uses ___alloc_rc for all of the C structures that it allocates.
;; In particular, when a Scheme string is passed to a C function, the Scheme string
;; is copied to a block of memory allocated with ___alloc_rc.  After returning from
;; the C function, the C interface will execute ___release_rc on the pointer to the
;; C string.  Normally this will reclaim the C string.  However, the C function can
;; call ___addref_rc to prevent this ___release_rc from deallocating the C string.
;; A subsequent call to ___release_rc (somewhere else in the logic of the program)
;; will reclaim the C string.

;;! ___alloc_rc
;; allocates from the C heap a reference counted block of memory which
;; is able to contain n bytes and returns a pointer to the first byte of that
;; block.  The block of memory also contains a slot of type ___SCMOBJ, the
;; Scheme "data".  The reference count is initially 1 and the data is #f.  In
;; terms of implementation, the data slot is stored immediately before the
;; first byte of the block.
(define alloc-rc
  (c-lambda (int) void*
            "___result_voidstar = ___EXT(___alloc_rc)(___arg1);"))

;;! ___release_rc
;; decrements the reference count and reclaims the block of memory when the
;; reference count reaches 0.  So ___alloc_rc and ___release_rc are drop-in
;; replacements for malloc and free (but you must not mix ___alloc_rc and free).
(define release-rc
  (c-lambda ((pointer void #f)) void "___release_rc"))

;;! ___addref_rc
;; increments the reference count.
(define addref-rc
  (c-lambda ((pointer void #f)) void "___addref_rc"))

;;! ___set_data_rc(ptr, val)
;; sets the data slot to val.  As long as the reference count is positive, the
;; GC will consider the data slot to be a root (in other words the data will
;; remain live and will not be reclaimed by the GC).
(define set-data-rc!
  (c-lambda ((pointer void #f) scheme-object) void "___set_data_rc"))

;;! ___data_rc(ptr)
;; returns the data slot.
(define data-rc
  (c-lambda ((pointer void #f)) scheme-object "___data_rc"))

;-------------------------------------------------------------------------------
; C memory
;-------------------------------------------------------------------------------

(c-declare "#include <malloc.h>")

(define calloc
  (c-lambda (unsigned-int unsigned-int) (pointer void) "calloc"))

(define malloc
  (c-lambda (unsigned-int) (pointer void) "malloc"))

(define realloc
  (c-lambda ((pointer void) unsigned-int) (pointer void) "realloc"))

(define free
  (c-lambda ((pointer void #f)) void "free"))

;-------------------------------------------------------------------------------
; Memory operations
;-------------------------------------------------------------------------------

;;! offset
(define *-offset
  (c-lambda ((pointer void #f) int) (pointer void #f)
            "___result_voidstar = ((void*)___arg1_voidstar) + ___arg2;"))

;;! Any pointer to void* casting
(define *->void*
  (c-lambda ((pointer void #f)) void*
            "___result_voidstar = (void*)___arg1_voidstar;"))

;;! Integer to void* casting
(define integer->void*
  (c-lambda (unsigned-long-long) void*
            "___result_voidstar = (void*)___arg1;"))

;-------------------------------------------------------------------------------
; Type sizes
;-------------------------------------------------------------------------------

(c-sizeof unsigned-char "unsigned char")
(c-sizeof char "char")
(c-sizeof unsigned-short "unsigned short")
(c-sizeof short "short")
(c-sizeof unsigned-int "unsigned int")
(c-sizeof int "int")
(c-sizeof unsigned-long "unsigned long")
(c-sizeof long "long")
(c-sizeof float "float")
(c-sizeof double "double")
(c-sizeof size-t "size_t")

;-------------------------------------------------------------------------------
; Gambit-managed arrays
;-------------------------------------------------------------------------------

(c-declare "#include <stdint.h>")

;;!! char

(define char*->string
  (c-lambda (char*) char-string
            "___result = ___arg1_voidstar;"))

;;!! unsigned char

(define void*->unsigned-char*
  (c-lambda (void*) unsigned-char*
            "___result_voidstar = ___arg1_voidstar;"))

(define unsigned-char*->string
  (c-lambda (unsigned-char*) char-string
            "___result = ___arg1_voidstar;"))

;;!! int

(define make-int*
  (c-lambda (int) int*
            "___result_voidstar = ___EXT(___alloc_rc)( ___arg1*sizeof(int) );"))

(define int*-set!
  (c-lambda (int* int) void
            "*(int*)___arg1_voidstar = ___arg2;"))

(define *->int
  (c-lambda (int*) int
            "___result = *(int*)___arg1_voidstar;"))

;;!! unisgned-int32
(define make-unsigned-int32*
  (c-lambda (int) unsigned-int32*
            "___result_voidstar = ___EXT(___alloc_rc)( ___arg1*sizeof(uint32_t) );"))

(define *->unsigned-int32
  (c-lambda (unsigned-int32*) unsigned-int32
            "___result = *___arg1;"))

;;!! unsigned-int

(define make-unsigned-int*
  (c-lambda (int) unsigned-int*
            "___result_voidstar = ___EXT(___alloc_rc)( ___arg1*sizeof(unsigned int) );"))

;; (define unsigned-int*-ref
;;   (c-lambda (unsigned-int* int) unsigned-int
;;             "___result = ((unsigned int*)___arg1_voidstar)[___arg2];"))

(define unsigned-int*-set!
  (c-lambda (unsigned-int* int unsigned-int) void
            "((unsigned int*)___arg1_voidstar)[___arg2] = ___arg3;"))

(define (vector->unsigned-int* vec)
  (let* ((length (vector-length vec))
         (buf (make-unsigned-int* length)))
    (let loop ((i 0))
      (if (< i length)
          (begin
            (unsigned-int*-set! buf i (vector-ref vec i))
            (loop (+ i 1)))
          buf))))

;-------------------------------------------------------------------------------
; C-managed arrays
;-------------------------------------------------------------------------------

;;;;;;; MALLOC GENERICO
;;;;;;; CONVERSION *void->int/otros tipos ESPECIFICO
;;;;;;; FREE GENÉRICO
;;;;;;; ARRAY-REF ESPECIFICO
;;;;;;; ARRAY-SET ESPECIFICO

;; (define-macro (define-array-allocator-and-accessors type)

;;    (define (sym . syms)
;;      (string->symbol (apply string-append
;;                             (map symbol->string syms))))

;;    (let ((type-str (symbol->string type)))
;;      `(begin

;;         (define ,(sym 'alloc- type '-array)
;;           (c-lambda (int) (pointer ,type)
;;                     ,(string-append
;;                       "___result_voidstar = malloc( ___arg1 * sizeof("
;;                       type-str
;;                       ") );")))

;;         (define ,(sym 'free- type '-array)
;;           (c-lambda ((pointer ,type)) void
;;                     "free( ___arg1 );"))

;;         (define ,(sym type '-array-ref)
;;           (c-lambda ((pointer ,type) int) ,type
;;                     "___result = ___arg1[___arg2];"))

;;         (define ,(sym type '-array-set!)
;;           (c-lambda ((pointer ,type) int ,type) void
;;                     "___arg1[___arg2] = ___arg3;")))))

;; ;; test it:

;; (define-array-allocator-and-accessors int)
;; (define-array-allocator-and-accessors double)

;; (define a (alloc-int-array 5))
;; (define b (alloc-double-array 10))

;; (int-array-set! a 1 111)
;; (int-array-set! a 2 222)
;; (pp (+ (int-array-ref a 1) (int-array-ref a 2))) ;; 333

;; (double-array-set! b 1 1.5)
;; (double-array-set! b 2 2.0)
;; (pp (+ (double-array-ref b 1) (double-array-ref b 2))) ;; 3.5

;; (free-int-array a)
;; (free-double-array b)
