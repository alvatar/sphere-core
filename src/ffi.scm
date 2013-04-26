;;; Copyright (c) 2013 by Álvaro Castro Castilla. All Rights Reserved.
;;; Foreign Function Interface functionality

;-------------------------------------------------------------------------------
; Code needed by FFI-generation macros
;-------------------------------------------------------------------------------

(##include "../src/ffi-header.scm")

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
;; (define alloc-rc
;;   (c-lambda (int) (pointer void #f)
;;             "___result_voidstar = ___EXT(___alloc_rc)(___arg1);"))

;;! ___release_rc
;; decrements the reference count and reclaims the block of memory when the
;; reference count reaches 0.  So ___alloc_rc and ___release_rc are drop-in
;; replacements for malloc and free (but you must not mix ___alloc_rc and free).
;; (define release-rc!
;;   (c-lambda ((pointer void #f)) void "___release_rc"))

;;! ___addref_rc
;; increments the reference count.
;; (define addref-rc!
;;   (c-lambda ((pointer void #f)) void "___addref_rc"))

;;! ___set_data_rc(ptr, val)
;; sets the data slot to val.  As long as the reference count is positive, the
;; GC will consider the data slot to be a root (in other words the data will
;; remain live and will not be reclaimed by the GC).
;; (define set-data-rc!
;;   (c-lambda ((pointer void #f) scheme-object) void "___set_data_rc"))

;;! ___data_rc(ptr)
;; returns the data slot.
;; (define data-rc
;;   (c-lambda ((pointer void #f)) scheme-object "___data_rc"))

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
; Memory operations and conversions
;-------------------------------------------------------------------------------

;;! offset
(define *-offset
  (c-lambda ((pointer void #f) int) (pointer void #f)
            "___result_voidstar = ((void*)___arg1_voidstar) + ___arg2;"))

;;! Any pointer to void* casting
(define *->void*
  (c-lambda ((pointer void #f)) (pointer void #f)
            "___result_voidstar = (void*)___arg1_voidstar;"))

;;! Integer to void* casting
(define integer->void*
  (c-lambda (unsigned-long-long) (pointer void #f)
            "___result_voidstar = (void*)___arg1;"))

(define *->string
  (c-lambda ((pointer void #f)) char-string
            "___result = ___arg1_voidstar;"))

;-------------------------------------------------------------------------------
; C arrays
;-------------------------------------------------------------------------------

(c-declare "#include <stdint.h>")

;;!! size_t
(c-define-sizeof size-t c-type: "size_t")

;;!! char
(c-define-type* char)
(c-define-sizeof char)
(c-define-array char
                scheme-vector: s8)

;;!! unsigned char
(c-define-type* unsigned-char)
(c-define-sizeof unsigned-char c-type: "unsigned char")
(c-define-array unsigned-char
                c-type: "unsigned char"
                scheme-vector: u8)

;;!! short
(c-define-type* short)
(c-define-sizeof short)
(c-define-array short
                scheme-vector: s16)

;;!! unsigned short
(c-define-type* unsigned-short)
(c-define-sizeof unsigned-short c-type: "unsigned short")
(c-define-array unsigned-short
                c-type: "unsigned short"
                scheme-vector: u16)

;;!! int
(c-define-type* int)
(c-define-sizeof int)
(c-define-array int
                scheme-vector: s32)

;;!! unsigned int
(c-define-type* unsigned-int)
(c-define-sizeof unsigned-int c-type: "unsigned int")
(c-define-array unsigned-int
                c-type: "unsigned int"
                scheme-vector: u32)

;;!! long
(c-define-type* long)
(c-define-sizeof long)
(c-define-array long
                scheme-vector: s64)

;;!! unsigned long
(c-define-type* unsigned-long)
(c-define-sizeof unsigned-long c-type: "unsigned long")
(c-define-array unsigned-long
                c-type: "unsigned long"
                scheme-vector: u64)

;;!! float
(c-define-type* float)
(c-define-sizeof float)
(c-define-array float
                scheme-vector: f32)

;;!! double
(c-define-type* double)
(c-define-sizeof double)
(c-define-array double
                scheme-vector: f32)
