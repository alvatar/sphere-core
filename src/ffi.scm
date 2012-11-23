;;; Copyright (c) 2012 by Álvaro Castro Castilla / Estevo Castro. All Rights Reserved.
;;; Foreign Function Interface functionality

;-------------------------------------------------------------------------------
; Code needed by FFI-generation macros
;-------------------------------------------------------------------------------

(cond-expand
 ((or compile-to-o compile-to-c)
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
  (c-define-type unsigned-char* (pointer unsigned-char))
  (c-define-type int8* (pointer int8))
  (c-define-type unsigned-int8* (pointer unsigned-int8))
  (c-define-type int16* (pointer int16))
  (c-define-type unsigned-int16* (pointer unsigned-int16))
  (c-define-type int32* (pointer int32))
  (c-define-type unsigned-int32* (pointer unsigned-int32))
  (c-define-type int64* (pointer int64))
  (c-define-type unsigned-int64* (pointer unsigned-int64))

  (c-define-type size-t unsigned-int))
 (else))

(cond-expand
 (compile-to-o
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
             ))
 (compile-to-c
  (c-declare #<<c-declare-end

             ___SCMOBJ ffimacro__leave_alone(void *p);
             ___SCMOBJ ffimacro__free_foreign(void *p);

c-declare-end
             ))
 (else))

(cond-expand
 ((or compile-to-o compile-to-c)
  (c-declare "#include <malloc.h>"))
  (else))

(define references
  (if (table? references)
    references
    (make-table weak-keys: #t weak-values: #f test: eq?)))

(define link
  (if (procedure? link)
    link
    (lambda (parent child)
      (table-set! references child parent)
      (make-will child (lambda (x) (table-set! references x))))))

;-------------------------------------------------------------------------------
; sizeof
;-------------------------------------------------------------------------------

(define sizeof-unsigned-char
  ((c-lambda () int "___result = sizeof(unsigned char);")))

;-------------------------------------------------------------------------------
; Operations
;-------------------------------------------------------------------------------

(define void*-offset
  (c-lambda ((pointer void #f) int) (pointer void #f)
            "___result_voidstar = ((void*)___arg1) + ___arg2;"))

;-------------------------------------------------------------------------------
; Conversion/casting
;-------------------------------------------------------------------------------

(define ->void*
  (c-lambda ((pointer void #f)) (pointer void #f)
            "___result_voidstar = (void*)___arg1;"))

(define void*->unsigned-char*
  (c-lambda (void*) unsigned-char*
            "___result_voidstar = ___arg1;"))

;-------------------------------------------------------------------------------
; Standard C functions
;-------------------------------------------------------------------------------

(define calloc
  (c-lambda (unsigned-int unsigned-int) (pointer void) "calloc"))

(define malloc
  (c-lambda (unsigned-int) (pointer void) "malloc"))

(define realloc
  (c-lambda ((pointer void) unsigned-int) (pointer void) "realloc"))

(define free
  (c-lambda ((pointer void)) void "free"))

;-------------------------------------------------------------------------------
; Arrays
;-------------------------------------------------------------------------------

;;; unsigned-int array

(c-declare #<<end-of-c-declare
#include <stdint.h>
end-of-c-declare
)

(define make-int*
  (c-lambda (int) int*
            "___result_voidstar = malloc( ___arg1*sizeof(int) );"))

(define int*-set!
  (c-lambda (int* int) void
            "*___arg1 = ___arg2;"))

(define pointer->int
  (c-lambda (int*) int
            "___result = *___arg1;"))

(define make-unsigned-int32*
  (c-lambda (int) unsigned-int32*
            "___result_voidstar = malloc( ___arg1*sizeof(uint32_t) );"))

(define pointer->unsigned-int32
  (c-lambda (unsigned-int32*) unsigned-int32
            "___result = *___arg1;"))



;;; TODO: make from macro and apply to all types

(define make-unsigned-int*
  (c-lambda (int) unsigned-int*
            "___result_voidstar = malloc( ___arg1*sizeof(unsigned int) );"))

(define unsigned-int*-ref
  (c-lambda (unsigned-int* int) unsigned-int
            "___result = ((unsigned int*)___arg1)[___arg2];"))

(define unsigned-int*-set!
  (c-lambda (unsigned-int* int unsigned-int) void
            "((unsigned int*)___arg1)[___arg2] = ___arg3;"))

(define (vector->unsigned-int* vec)
  (let* ((length (vector-length vec))
         (buf (make-unsigned-int* length)))
    (let loop ((i 0))
      (if (< i length)
          (begin
            (unsigned-int*-set! buf i (vector-ref vec i))
            (loop (+ i 1)))
          buf))))

