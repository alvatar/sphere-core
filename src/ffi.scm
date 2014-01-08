;;; Copyright (c) 2013 by Álvaro Castro Castilla. All Rights Reserved.
;;; Foreign Function Interface functionality

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))

(##include "../src/ffi-header.scm")

;;-------------------------------------------------------------------------------

;; Gambit memory

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


;;------------------------------------------------------------------------------

;;!! C Types: readers/writers
;;
;; Use like this:
;; (call-with-output-file
;;      "f64test"
;;    (lambda (port)
;;      (write-f64 -1.5 port)
;;      (write-f64 +inf.0 port)
;;      (write-f64 3.1415926 port)))
;;
;; (call-with-input-file
;;      "f64test"
;;    (lambda (port)
;;      (let* ((a (read-f64 port))
;;             (b (read-f64 port))
;;             (c (read-f64 port))
;;             (d (read-f64 port)))
;;        (pp (list a b c d)))))

;;! u8vector reader/writer
;; Already defined in Gambit

;;! s8vector reader/writer
(define-writer write-s8 s8vector)
(define-reader read-s8 s8vector s8vector-subtype s8vector-ref 0)

;;! u16vector reader/writer
(define-writer write-u16 u16vector)
(define-reader read-u16 u16vector u16vector-subtype u16vector-ref 0)

;;! s16vector reader/writer
(define-writer write-s16 s16vector)
(define-reader read-s16 s16vector s16vector-subtype s16vector-ref 0)

;;! u32vector reader/writer
(define-writer write-u32 u32vector)
(define-reader read-u32 u32vector u32vector-subtype u32vector-ref 0)

;;! s32vector reader/writer
(define-writer write-s32 s32vector)
(define-reader read-s32 s32vector s32vector-subtype s32vector-ref 0)

;;! u64vector reader/writer
(define-writer write-u64 u64vector)
(define-reader read-u64 u64vector u64vector-subtype u64vector-ref 0)

;;! s64vector reader/writer
(define-writer write-s64 s64vector)
(define-reader read-s64 s64vector s64vector-subtype s64vector-ref 0)

;;! f32vector reader/writer
(define-writer write-f32 f32vector)
(define-reader read-f32 f32vector f32vector-subtype f32vector-ref 0.0)

;;! f64vector reader/writer
(define-writer write-f64 f64vector)
(define-reader read-f64 f64vector f64vector-subtype f64vector-ref 0.0)


;;------------------------------------------------------------------------------

;;!! FFI types serialization

;;! Adds ability to properly serialize FFI types on (write) and properly deserialize them on (read).
;; .author Mikael More. MIT License.
;; ref: https://mercure.iro.umontreal.ca/pipermail/gambit-list/2013-March/006510.html
;;
;; Usage:
;; (ffi-write-transformer-add!
;;  'name-of-your-ffi-type  
;;  (lambda (v) `(name-of-constructor-procedure-to-create-an-instance-of-this-ffi-type
;;                [ arguments needed to constructor to produce an instance exactly like
;;                the one in the v variable ])))
;; Example:
;;
;; (c-declare #<<end-of-c-declare
;; #include <stdlib.h>
;; typedef struct { int x, y; } point;
;; point *make_point( int x, int y ) {
;;   point *p = ___CAST(point*, malloc(sizeof(point)));
;;   p->x = x;
;;   p->y = y;
;;   return p;
;; }
;; int point_x( point* p ) { return p->x; }
;; int point_y( point* p ) { return p->y; }
;; end-of-c-declare
;; )
;;
;; (c-define-type point "point")
;; (c-define-type point* (pointer point))
;; (define make-point (c-lambda (int int) point* "make_point"))
;; (define point-x (c-lambda (point*) int "point_x"))
;; (define point-y (c-lambda (point*) int "point_y"))
;;
;; (ffi-write-transformer-add! 'point* (lambda (v) `(make-point ,(point-x v) ,(point-y v))))
;; REPL will show:
;; #.(make-point 2 1)
;; Instead of
;; #<point* #2 0x1160a90>

;; Serialize:
;; (object->string (make-point 3 4)) 
;; Deserialize:
;; (string->object "#.(make-point 2 1)")

(define *ffi-writer-transformers* (make-table test: eq?))
(define *writer-default* #f)

(define (ffi-write-transformer-add! type serializer-proc)
  (table-set! *ffi-writer-transformers* type serializer-proc))

(define (%%sexp-ext:wr we obj)
  (if (##foreign? obj)
      (let* ((name (let ((v (foreign-tags obj)))
                     (and (pair? v) (car v))))
             (transformer (table-ref *ffi-writer-transformers* name #f)))
        (if transformer
            (let ((transformed-to (transformer obj)))
              (##wr-str we "#.")
              ;; (##wr-pair we transformed-to) - transformed-to may be sth else for instance a symbol
              ;; so instead go with the universal:
              (*writer-default* we transformed-to))
            (##wr-foreign we obj)))
      (*writer-default* we obj)))

;;! Initialize FFI serialization extension
(define (%%sexp-ext-install!)
  (and (not (eq? *writer-default* ##wr))
       (begin
         (set! *writer-default* ##wr)
         (set! ##wr %%sexp-ext:wr)
         (let* ((port (repl-input-port))
                (rt (input-port-readtable port)))
           ;; (##readtable-char-sharp-handler-set! rt #\< sexp-ext:read-sharp-less)
           (input-port-readtable-set! port (readtable-eval-allowed?-set rt #t))
           (void)))))
;;! Call to initialize FFI serialization extension
(%%sexp-ext-install!)

;;! Object from string
(define (string->object s)
  (call-with-input-string
   s
   (lambda (port)
     (input-port-readtable-set!
      port (readtable-eval-allowed?-set (input-port-readtable port) #t))
     (read port))))


;;------------------------------------------------------------------------------

;;!! C memory

(c-declare "#include <malloc.h>")

(define calloc
  (c-lambda (unsigned-int unsigned-int) (pointer void) "calloc"))

(define malloc
  (c-lambda (unsigned-int) (pointer void) "malloc"))

(define realloc
  (c-lambda ((pointer void) unsigned-int) (pointer void) "realloc"))

(define free
  (c-lambda ((pointer void #f)) void "free"))


;;------------------------------------------------------------------------------

;;!! Memory operations and conversions

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


;;------------------------------------------------------------------------------

;;! C arrays

(c-declare "#include <stdint.h>")

;;! size_t
(c-define-sizeof size-t c-type: "size_t")

;;! char
(c-define-type* char)
(c-define-sizeof char)
(c-define-array char
                scheme-vector: s8)

;;! unsigned char
(c-define-type* unsigned-char)
(c-define-sizeof unsigned-char c-type: "unsigned char")
(c-define-array unsigned-char
                c-type: "unsigned char"
                scheme-vector: u8)

;;! short
(c-define-type* short)
(c-define-sizeof short)
(c-define-array short
                scheme-vector: s16)

;;! unsigned short
(c-define-type* unsigned-short)
(c-define-sizeof unsigned-short c-type: "unsigned short")
(c-define-array unsigned-short
                c-type: "unsigned short"
                scheme-vector: u16)

;;! int
(c-define-type* int)
(c-define-sizeof int)
(c-define-array int
                scheme-vector: s32)

;;! unsigned int
(c-define-type* unsigned-int)
(c-define-sizeof unsigned-int c-type: "unsigned int")
(c-define-array unsigned-int
                c-type: "unsigned int"
                scheme-vector: u32)

;;! long
(c-define-type* long)
(c-define-sizeof long)
(c-define-array long
                scheme-vector: s64)

;;! unsigned long
(c-define-type* unsigned-long)
(c-define-sizeof unsigned-long c-type: "unsigned long")
(c-define-array unsigned-long
                c-type: "unsigned long"
                scheme-vector: u64)

;;! float
(c-define-type* float)
(c-define-sizeof float)
(c-define-array float
                scheme-vector: f32)

;;! double
(c-define-type* double)
(c-define-sizeof double)
(c-define-array double
                scheme-vector: f32)

