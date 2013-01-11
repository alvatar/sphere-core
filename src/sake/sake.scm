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

(##define (get-parameter p #!optional (default #f))
  (cond
   ((assc p (command-line)) => cadr)
   (else default)))

(##define (get-tasks #!optional (default #f))
  (let remove-flags ((lst (cdr (command-line))))
    (cond
     ((null? lst) default)
     ((char=? (string-ref (car lst) 0) #\-) (remove-flags (cddr lst)))
     (else (map string->symbol lst)))))

(define *help* #<<end-of-help
sake [-file <sake-file>] [<initial-task>]
sake -help

sake is an utility like make for scheme.
<sake-file> is the file containing tasks description (defaults to sake-file.scm)
<initial-task> is the entry point task, (defaults to make)
end-of-help
)

(define (main) 
  ;(load "~~spheres/sake/sakelib")
  (if (get-parameter "-help") (display help)
      (let((tasks (get-tasks '(all)))
           (dir (get-parameter "-dir" (current-directory)))
           (file (get-parameter "-file" "sakefile.scm")))
        (sake dir: dir file: file tasks: tasks))))

(main)
