#!/usr/bin/env gsi-script


(define (substring-search-maker pattern-string)
  (define num-chars-in-charset 256) ;; Update this, e.g. for ISO Latin 1
  (define (build-shift-vector pattern-string)
    (let* ((pat-len (string-length pattern-string))
           (shift-vec (make-vector num-chars-in-charset (+ pat-len 1)))
           (max-pat-index (- pat-len 1)))
      (let loop ((index 0))
        (vector-set! shift-vec
                     (char->integer (string-ref pattern-string index))
                     (- pat-len index))
        (if (< index max-pat-index)
            (loop (+ index 1))
            shift-vec))))
  (let ((shift-vec (build-shift-vector pattern-string))
        (pat-len (string-length pattern-string)))
    (lambda (target-string start-index)
      (let* ((tar-len (string-length target-string))
             (max-tar-index (- tar-len 1))
             (max-pat-index (- pat-len 1)))
        (let outer ((start-index start-index))
          (if (> (+ pat-len start-index) tar-len)
              #f
              (let inner ((p-ind 0) (t-ind start-index))
                (cond
                 ((> p-ind max-pat-index) ; nothing left to check
                  #f)                     ; fail
                 ((char=? (string-ref pattern-string p-ind)
                          (string-ref target-string  t-ind))
                  (if (= p-ind max-pat-index)
                      start-index ;; success -- return start index of match
                      (inner (+ p-ind 1) (+ t-ind 1)))) ; keep checking
                 ((> (+ pat-len start-index) max-tar-index) #f) ; fail
                 (else
                  (outer (+ start-index
                            (vector-ref shift-vec
                                        (char->integer
                                         (string-ref target-string
                                                     (+ start-index pat-len)))))))))))))))

(define (split sep)
    (lambda (str)
      (call-with-input-string
       str
       (lambda (p)
         (read-all p (lambda (p) (read-line p sep)))))))

(define (read-file f separator)
  (call-with-input-file f
    (lambda (p) (read-all p (lambda (f) (read-line f separator))))))

;; Extremely dumb bindings generator. Works for OpenGL.
;; TODO: doesn't handle at all when * is sticked to the next token
(define (generate-function-binding declaration)
  (define (clean-str str)
    (list->string
     (let recur ((rest (string->list str)))
       (cond ((null? rest)
              '())
             ((char=? #\newline (car rest))
              (recur (cdr rest)))
             ((or (char=? #\; (car rest))
                  (char=? #\, (car rest)))
              (recur (cdr rest)))
             (else (cons (car rest)
                         (recur (cdr rest))))))))
  (define (tokenize str)
    ((split #\space) str))
  (define (analyze list-tokens)
    (let recur ((tok list-tokens)
                (state 'searching-return-type))
      (cond ((null? tok) '()) ; This is just a lazy (and completely wrong) way
            ((or (string=? "" (car tok))
                 (string=? " " (car tok))
                 (string=? "," (car tok))
                 (string=? "(" (car tok))
                 (string=? ")" (car tok))
                 (string=? ";" (car tok))
                 (string=? "\n" (car tok))
                 (string=? ");" (car tok))
                 (string=? "extern" (car tok))
                 (string=? "const" (car tok)))
             (recur (cdr tok) state))
            (else
             (let* ((head (car tok))
                    (tail (cdr tok))
                    (head-len (string-length head)))
               (case state
                 ((searching-return-type)
                  (if (string=? "const" head) ; discard cons qualifier
                      (recur tail 'searching-return-type)
                      (cons (list return-type: (string->symbol (clean-str head)))
                            (recur tail 'searching-function-name))))
                 ((searching-function-name)
                  (cons (list function-name: head)
                        (recur tail 'searching-parameters)))
                 ((searching-parameters)
                  (cond ((string=? "const" head) ; discard const qualifier
                         (recur tail 'searching-parameters))
                        ((char=? #\( (string-ref head 0)) ; deal with (
                         (if (or (and (> head-len 2) (string=? "()" (substring head 0 2)))
                                 (and (> head-len 6) (string=? "(void)" (substring head 0 6))))
                             (list (list parameters: '()))
                             (recur (cons (substring head 1 head-len)
                                          tail)
                                    'searching-parameters)))
                        (else (cons (list parameter: (string->symbol (clean-str head)))
                                    (recur tail 'discard-next-token)))))
                 ((discard-next-token)
                  (recur tail 'searching-parameters))))))))
  (let* ((tokens (tokenize declaration))
         (sl (analyze tokens)))
    (if (not (null? sl))
        (pp `(define ,(string->symbol (clean-str (cadr (assq function-name: sl))))
               (c-lambda (,@(let recur ((rest sl))
                              (cond ((null? rest) '())
                                    ((eq? parameter: (caar rest))
                                     (if (eq? 'void (cadar rest))
                                         '()
                                         (cons (cadar rest) (recur (cdr rest)))))
                                    (else (recur (cdr rest))))))
                         ,(cadr (assq return-type: sl))
                         ,(cadr (assq function-name: sl))))))))


(define (clean-up-defines str)
  (let ((str-define-searcher (substring-search-maker "#define")))
    (let find-defines ((found-position (str-define-searcher str 0))
                       (defines '()))
      (if found-position
          (let grab-define ((current-pos (+ found-position 8))
                            (state 'find-beginning))
            (find-defines (str-define-searcher str current-pos)
                          (cons (string-ref str current-pos)
                                defines)))
          (reverse defines)))))

(define (main . args)
  (if (null? args)
      (error "Arguments required. Use 'help' command for more info."))
  (if (null? (cdr args))
      (error "Command requires argument. Use 'help' command for more info."))
  (let ((file (cadr args)))
   (cond
    ((string=? (car args) "help")
     (println "Available commands: defines functions types"))
    ((string=? (car args) "defines")
     (println (clean-up-defines (read-file file))))
    ((string=? (car args) "functions")
     (println (car (map generate-function-binding (read-file file #\;)))))
    ((string=? (car args) "types")
     (error "Not implemented"))
    (else
     (error "Unknown command")))))
