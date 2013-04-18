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
; C arrays
;-------------------------------------------------------------------------------

(c-declare "#include <stdint.h>")

(build-c-sizeof unsigned-char c-type: "unsigned char")

(build-c-sizeof unsigned-short c-type: "unsigned short")
(build-c-sizeof short)
(build-c-sizeof unsigned-long c-type: "unsigned long")
(build-c-sizeof long)
(build-c-sizeof float)
(build-c-sizeof double)
(build-c-sizeof size-t c-type: "size_t")

;;!! char

(build-c-sizeof char)
(build-c-array-ffi char
                   scheme-vector: s8)

(define char*->string
  (c-lambda (char*) char-string
            "___result = ___arg1_voidstar;"))

;;!! unsigned char

(build-c-sizeof unsigned-char c-type: "unsigned char")
(build-c-array-ffi unsigned-char
                   c-type: "unsigned char"
                   scheme-vector: u8)

(define unsigned-char*->string
  (c-lambda (unsigned-char*) char-string
            "___result = ___arg1_voidstar;"))

;;!! int

(build-c-sizeof int)
(build-c-array-ffi int
                   scheme-vector: s32)

;;!! unsigned-int

(build-c-sizeof unsigned-int c-type: "unsigned int")
(build-c-array-ffi unsigned-int
                   c-type: "unsigned int"
                   scheme-vector: u32)
