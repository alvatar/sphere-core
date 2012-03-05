;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;; BSD 2-Clause License, for more information see LICENSE

;;; Enum definition
;;; (variation on a code proposed by Marc Feeley)

(define-macro
  (import-enum-constants scheme-type c-type . names)
  (define (interval lo hi)
    (if (< lo hi) (cons lo (interval (+ lo 1) hi)) '()))
  (let ((c-type-str (symbol->string c-type))
        (nb-names (length names))
        (wrapper (gensym)))
    `(begin
       (define ,wrapper
         (c-lambda (int)
                   ,scheme-type
                   ,(string-append
                      "static " c-type-str " _tmp_[] = {\n"
                      (apply string-append
                             (map (lambda (i name)
                                    (let ((name-str (symbol->string name)))
                                      (string-append
                                        (if (> i 0) "," "")
                                        name-str)))
                                  (interval 0 nb-names)
                                  names))
                      "};\n"
                      "___ASSIGN_NEW_WITH_INIT(___result_voidstar," c-type-str ",_tmp_[___arg1]);\n")))
       ,@(map (lambda (i name)
                `(define ,name (,wrapper ,i)))
              (interval 0 nb-names)
              names))))

;;; Access a field from a struct

(define-macro (make-field-ref c-type scheme-type field-name)
  `(c-lambda
    (,c-type)
    ,scheme-type
    ,(string-append "___result = ___arg1->" field-name ";")))