;;; Copyright (c) 2013 by Álvaro Castro Castilla. All Rights Reserved.
;;; Foreign Function Interface functionality

;;!! Basic utilities

(define^ (->string o)
  (cond ((string? o) o)
        ((symbol? o) (symbol->string o))
        ((keyword? o) (keyword->string o))
        (else (error (string-append "->string: unrecognized type -- " (object->string o))))))

(define^ (generic-string-append #!rest ol)
  (apply string-append (map ->string ol)))

(define^ (symbol-append #!rest ol)
  (string->symbol (apply generic-string-append ol)))

(define^ (begin-top-level-forms #!rest define-blocks)
  (cons 'begin
        (let recur ((ds define-blocks))
          (cond ((null? ds) '())
                ((null? (car ds)) (recur (cdr ds)))
                (else (cons (car ds)
                            (recur (cdr ds))))))))

(define^ (sym #!rest strs)
    (string->symbol (apply string-append strs)))

;;! Turn a scheme-name into a c_name, simply changing the - to _
(define^ (scheme-name->c-name name)
  (let* ((name-str (cond ((string? name) name)
                         ((symbol? name) (symbol->string name))
                         (else (object->string name))))
         (new-name (string-copy name-str))
         (name-length (string-length new-name)))
    (let recur ((i 0))
      (if (< i name-length)
          (begin (if (char=? (string-ref new-name i) #\-)
                     (string-set! new-name i #\_))
                 (recur (+ i 1)))
          new-name))))

;;! Schemify a name: turn-it-into-a-scheme-name
;; .author Fred LeMaster
(define^ (schemify-name symbol-or-string)
  ((lambda (str)
     (letrec
         ((str-length (string-length str))
          (case-changed?
           (lambda (i)
             (let ((h (- i 1)))
               (cond ((< h 0) #f)
                     ((and (char-lower-case? (string-ref str h))
                           (char-upper-case? (string-ref str i)))
                      #t)
                     (else #f)))))
          (char-loop
           (lambda (i out)
             (cond ((>= i str-length) out)
                   ((char=? (string-ref str i) #\_)
                    (char-loop (+ i 1) (cons #\- out)))
                   ((and (char=? (string-ref str i) #\:)
                         (< (+ i 1) str-length)
                         (char=? (string-ref str (+ i 1)) #\:))
                    (char-loop (+ i 2) (cons #\- out)))
                   ((case-changed? i)
                    (char-loop (+ i 1)
                               (cons (char-downcase
                                      (string-ref str i))
                                     (cons #\-
                                           out))))
                   (else (char-loop (+ i 1)
                                    (cons
                                     (char-downcase
                                      (string-ref str i))
                                     out)))))))
       (list->string (reverse (char-loop 0 '())))))
   (cond ((symbol? symbol-or-string) (symbol->string symbol-or-string))
         ((string? symbol-or-string) symbol-or-string)
         (else (error "schemify-name: expected symbol or string")))))


;;------------------------------------------------------------------------------

;;!! C Types

(c-define-type size-t unsigned-long-long)


;;------------------------------------------------------------------------------

;;!! C Types: readers/writers generation

(define u8vector-subtype (##subtype (u8vector)))
(define s8vector-subtype (##subtype (s8vector)))
(define u16vector-subtype (##subtype (u16vector)))
(define s16vector-subtype (##subtype (s16vector)))
(define u32vector-subtype (##subtype (u32vector)))
(define s32vector-subtype (##subtype (s32vector)))
(define u64vector-subtype (##subtype (u64vector)))
(define s64vector-subtype (##subtype (s64vector)))
(define f32vector-subtype (##subtype (f32vector)))
(define f64vector-subtype (##subtype (f64vector)))

;;! Macros for defining readers and writers.
;; .author Marco Benelli
;; ref: https://mercure.iro.umontreal.ca/pipermail/gambit-list/2009-June/003671.html
(define-macro (define-writer name vtype)
  `(define (,name x #!optional (port (current-output-port)))
     (let ((v (,vtype x)))
       (##subtype-set! v u8vector-subtype)
       (write-subu8vector v 0 (u8vector-length v) port))))

(define-macro (define-reader name vtype vsubtype vtype-ref init)
  `(define (,name #!optional (port (current-input-port)))
     (let ((v (,vtype ,init)))
       (##subtype-set! v u8vector-subtype)
       (let ((n (read-subu8vector v 0 (u8vector-length v) port)))
         (if (= n (u8vector-length v))
             (begin
               (##subtype-set! v ,vsubtype)
               (,vtype-ref v 0))
             #!eof)))))


;;------------------------------------------------------------------------------

;;!! FFI generation

;;! define types for structs, unions and arrays
;; .author Álvaro Castro-Castilla, based on code by Estevo Castro
;; (c-define-type* (struct MyStruct))
;; (c-define-type* (union MyUnion))
;; (c-define-type* myType)
(define-macro (c-define-type* type/struct/union)
  (let* ((type (if (pair? type/struct/union)
                   (cadr type/struct/union)
                   type/struct/union))
         (struct-or-union (if (pair? type/struct/union)
                              (car type/struct/union)
                              #f))
         (type-str (symbol->string type))
         (release-type-str (string-append "___release_" (scheme-name->c-name type-str)))
         (type* (sym type-str "*"))
         (type*/nonnull (sym type-str "*/nonnull"))
         (type*/release-rc (sym type-str "*/release-rc")))
    (let ((expansion
           (begin-top-level-forms
            (if struct-or-union
                `(c-define-type ,type (,struct-or-union ,type-str))
                '())
            `(c-define-type ,type* (pointer ,type (,type*)))
            `(c-define-type ,type*/nonnull (nonnull-pointer ,type (,type*)))
            `(c-define-type ,type*/release-rc (nonnull-pointer ,type (,type*) ,release-type-str)))))
      (if #f ;; #t for debugging
          (pp `(definition:
                 (c-define-extended-type ,type)
                 expansion:
                 ,expansion)))
      expansion)))

;; Helper for define-c-struct and define-c-union
(define^ (%%c-define-struct-or-union struct-or-union type fields)
  (let* ((type-str (symbol->string type))
         (struct-type-str
          (string-append (case struct-or-union
                           ((struct) "struct ")
                           ((union) "union ")
                           (else (error "%%c-define-struct-or-union: first parameter must be 'struct or 'union")))
                         type-str))
         (struct-type*-str (string-append struct-type-str "*"))
         (release-type-str (string-append "___release_" type-str))
         (type* (sym type-str "*"))
         (type*/nonnull (sym type-str "*/nonnull"))
         (type*/release-rc (sym type-str "*/release-rc")))
    (define (field-getter-setter field-spec)
      (let* ((field (car field-spec))
             (field-str (symbol->string field))
             (field-type (cadr field-spec)))
        (if (and (pair? field-type)
                 (eq? (car field-type) 'array))
            ;; Field is an array
            (let* ((elem-type
                    (cadr field-type))
                   (elem-type-str
                    (symbol->string elem-type)))
              (if (table-ref c-define-struct-table elem-type #f)
                  ;; array element is a struct =>
                  ;; only generate a getter returning struct address
                  `((define ,(sym type-str "-" field-str "-ref")
                      (c-lambda (,type*/nonnull int)
                                ,(sym elem-type-str "*/nonnull")
                                ,(string-append "___result_voidstar = &___arg1->" field-str "[___arg2];"))))

                  ;; array element is not a struct =>
                  ;; generate a getter and a setter
                  `((define ,(sym type-str "-" field-str "-ref")
                      (c-lambda (,type*/nonnull int)
                                ,elem-type
                                ,(string-append "___result = ___arg1->" field-str "[___arg2];")))
                    (define ,(sym type-str "-" field-str "-set!")
                      (c-lambda (,type*/nonnull int ,elem-type)
                                void
                                ,(string-append "___arg1->" field-str "[___arg2] = ___arg3;"))))))
            ;; Field is not an array
            (if (table-ref c-define-struct-table field-type #f)
                ;; Field is a struct (and not an array)
                `((define ,(sym type-str "-" field-str)
                    (c-lambda (,type*/nonnull)
                              ,(symbol-append field-type "*/nonnull")
                              ,(string-append "___result_voidstar = &___arg1->" field-str ";")))

                  (define ,(sym type-str "-" field-str "-set!")
                    (c-lambda (,type*/nonnull ,field-type)
                              void
                              ,(string-append "___arg1->" field-str " = ___arg2;"))))
                ;; Field is not a struct (and not an array)
                `((define ,(sym type-str "-" field-str)
                    (c-lambda (,type*/nonnull)
                              ,field-type
                              ,(string-append "___result = ___arg1->" field-str ";")))

                  (define ,(sym type-str "-" field-str "-set!")
                    (c-lambda (,type*/nonnull ,field-type)
                              void
                              ,(string-append "___arg1->" field-str " = ___arg2;"))))))))
    (table-set! c-define-struct-table type #t) ;; remember it is a struct
    (let ((expansion
           `(begin
              ;; Define the release function which is called when the
              ;; object is no longer accessible from the Scheme world.
              (c-declare
               ,(string-append
                 "static ___SCMOBJ " release-type-str "( void* ptr )\n"
                 "{\n"
                 "  ___EXT(___release_rc)( ptr );\n"
                 "  return ___FIX(___NO_ERR);\n"
                 "}\n"))
              ;; Define type allocator procedure.
              (define ,(sym "alloc-" type-str)
                (c-lambda ()
                          ,type*/release-rc
                          ,(string-append "___result_voidstar = ___EXT(___alloc_rc)( sizeof( " struct-type-str " ) );")))
              ;; Dereference
              (define ,(sym "*->" type-str)
                (c-lambda (,type*/nonnull)
                          ,type
                          ,(string-append "___result_voidstar = (" type-str "*)___arg1;")))
              ;; Define field getters and setters.
              ,@(apply append (map field-getter-setter fields)))))
      (if #f ;; #t for debugging
          (pp `(definition:
                 (c-define-struct ,type ,@fields)
                 expansion:
                 ,expansion)))
      expansion)))

;;! Defines the c-define-struct macro, which extends the Gambit FFI to
;; interface to C structures.
(define-macro (c-define-struct type . fields)
  (%%c-define-struct-or-union 'struct type fields))

;;! Defines the c-define-union macro, which extends the Gambit FFI to
;; interface to C structures.
(define-macro (c-define-union type . fields)
  (%%c-define-struct-or-union 'union type fields))

(define-macro (c-define-struct-initialize!)
  ;; Define c-define-struct-table at macro expansion time.
  (eval '(define c-define-struct-table (make-table)))
  `(begin))
(c-define-struct-initialize!)

;;! C constants generation macro
;; Creating the bindings in a simple C function makes for more compact
;; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2012-February/005688.html
(define-macro (c-define-constants . names)
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

;;! Build a size-of value equivalent to the C operator
;; c-build-sizeof float -> sizeof-float
(define-macro (c-define-sizeof scheme-type #!key (c-type (symbol->string scheme-type)))
  `(define ,(symbol-append scheme-type '-size)
     ((c-lambda () size-t
                ,(string-append "___result = sizeof(" c-type ");")))))

;;! Build FFI procedures for C type arrays. Only for use with basic types, not structs.
;; (build-c-array-ffi float f32) ->
;; alloc-float*
;; float*-ref
;; float*-set!
;; *->float*
;; f32vector->float*
(define-macro (c-define-array scheme-type #!key (c-type scheme-type) scheme-vector)
  (let ((release-type-str (string-append  "___release_" (scheme-name->c-name scheme-type)))
        (type scheme-type)
        (type*
         (symbol-append scheme-type "*"))
        (type*/nonnull
         (symbol-append scheme-type "*/nonnull"))
        (type*/release-rc
         (symbol-append scheme-type "*/release-rc")))
    (let ((expansion
           (begin-top-level-forms
            `(c-declare
              ,(string-append
                "static ___SCMOBJ " release-type-str "( void* ptr )\n"
                "{\n"
                ;; " printf(\"GC called free()!\\n\");\n"
                ;; "  ___EXT(___release_rc)( ptr );\n"
                "  free( ptr );\n"
                "  return ___FIX(___NO_ERR);\n"
                "}\n"))
            ;; Alloc managed by Gambit's GC
            `(define ,(symbol-append 'alloc- scheme-type '*/unmanaged)
               (c-lambda (size-t)
                         ,type*/nonnull
                         ,(generic-string-append "___result_voidstar = malloc(___arg1*sizeof(" c-type "));")))
            ;; Alloc unmanaged by Gambit's GC
            `(define ,(symbol-append 'alloc- scheme-type '*)
               (c-lambda (size-t)
                         ,type*/release-rc
                         ;; ,(generic-string-append "___result_voidstar = ___EXT(___alloc_rc)(___arg1*sizeof(" c-type "));")
                         ,(generic-string-append "___result_voidstar = malloc(___arg1*sizeof(" c-type "));")))
            `(define ,(symbol-append scheme-type '*-ref)
               (c-lambda (,type*/nonnull size-t)
                         ,scheme-type
                         "___result = ___arg1[___arg2];"))
            `(define ,(symbol-append scheme-type '*-set!)
               (c-lambda (,type*/nonnull size-t ,scheme-type)
                         void
                         "___arg1[___arg2] = ___arg3;"))
            `(define ,(symbol-append '*-> scheme-type)
               (c-lambda (,type*/nonnull)
                         ,scheme-type
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
                '()))))
      (if #f ;; #t for debugging
          (pp `(definition:
                 (c-define-array scheme-type: ,scheme-type c-type: ,c-type scheme-vector: ,scheme-vector)
                 expansion:
                 ,expansion)))
      expansion)))

