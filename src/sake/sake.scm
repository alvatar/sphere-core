;;; @file sake.scm
;;; this file is just a little wrapper around the function
;;; sake in sake/common.scm
;;; @author francesco bracchi <frbracch@gmail.com>

(##namespace ("sake#"))
(##include "~~/lib/gambit#.scm")
(##include "sakelib.scm")

(define (assc e es)
  (let assc ((es es))
  (cond
   ((null? es) #f)
   ((equal? (car es) e) es)
   (else (assc (cdr es))))))

(##define (get-parameter parameters p #!optional (default #f))
  (cond
   ((assc p parameters) => (lambda (e)
                             (if (null? (cdr e))
                                 'no-arguments
                                 (cadr e))))
   (else default)))

(##define (get-tasks parameters #!optional (default #f))
  (define (flag? el) (and (>= 2 (string-length el))
                          (string=? "--" (substring el 0 2))))
  (let remove-flags ((lst parameters))
    (cond
     ((null? lst) default)
     ((flag? (car lst))
      (if (and (not (null? (cdr lst)))
               (flag? (cdr lst)))
          (remove-flags (cddr lst))
          (remove-flags (cdr lst))))
     (else (map string->symbol lst)))))

(define *help* #<<end-of-help
  
sake [--file <sake-file>] [<initial-task>]
sake --help

sake is an utility like make for scheme.
<sake-file> is the file containing tasks description (defaults to sakefile.scm)
<initial-task> is the entry point task

end-of-help
)

(define (main command-line) 
  (let ((parameters (cdr command-line)))
    (if (get-parameter parameters "--help")
       (display *help*)
       (let ((tasks (get-tasks parameters '(all)))
             (file (get-parameter parameters "--file" "sakefile.scm")))
         (sake file: file tasks: tasks)))))

(main (command-line))
