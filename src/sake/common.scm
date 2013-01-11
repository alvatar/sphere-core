;-------------------------------------------------------------------------------
; Parameters and globals
;-------------------------------------------------------------------------------

(define current-build-directory 
  (make-parameter 
   (string-append (current-directory) "build/")))

(define current-source-directory 
  (make-parameter 
   (string-append (current-directory) "src/")))

(define current-lib-directory
  (make-parameter (string-append (current-directory) "lib/")))

(define current-project-name
  (make-parameter
   (path-strip-directory
    (path-strip-trailing-directory-separator (current-directory)))))

(define current-module-name
  (make-parameter (current-project-name)))

;-------------------------------------------------------------------------------
; Output
;-------------------------------------------------------------------------------

(define (log type . message)
  (display "*** ")
  (display type)
  (display " -- ")
  (for-each print message)
  (newline))

(define (info . message)
  (apply log (cons "INFO" message)))

(define (info/color color . message)
  (let ((color-string
         (case color
           ((black) "\033[00;30m")
           ((dark-gray) "\033[01;30m")
           ((blue) "\033[00;34m")
           ((light-blue) "\033[01;34m")
           ((green) "\033[00;32m")
           ((light-green) "\033[01;32m")
           ((cyan) "\033[00;36m")
           ((light-cyan) "\033[01;36m")
           ((red) "\033[00;31m")
           ((light-red) "\033[01;31m")
           ((purple) "\033[00;35m")
           ((light-purple) "\033[01;35m")
           ((brown) "\033[00;33m")
           ((yellow) "\033[01;33m")
           ((light-gray) "\033[00;37m")
           ((white) "\033[01;37m")
           (else ""))))
    (apply log (append `("INFO" ,color-string) message))
    (display "\033[00m")))

(define (warn . message)
  (display "\033[00;33m")
  (apply log (cons "WARNING" message))
  (display "\033[00m"))

(define (err . message)
  (display "\033[00;31m")
  (apply log (cons "ERROR" message))
  (display "\033[00m")
  (apply error (cons "sake error" message)))

;-------------------------------------------------------------------------------
; Util
;-------------------------------------------------------------------------------

(define (reduce f i l)
  (let reduce ((i i) (l l))
    (if (null? l) i
        (reduce (f i (car l)) (cdr l)))))

;-------------------------------------------------------------------------------
; Main
;-------------------------------------------------------------------------------

(##define (sake #!key 
                (dir (current-directory))
                (file "sakefile.scm")
                (tasks '(all)))
  (info "entering directory " dir)
  (eval `(begin
           (##namespace (,(string-append (symbol->string (gensym 'sakefile)) "#")))
           (##include "~~lib/gambit#.scm")
           (##include "~~spheres/core/src/sake/sakelib#.scm")
           (##namespace ("" alexpand))
           ,(let ((prelude-file "~~spheres/spheres#.scm")
                  (sake-extensions "~~spheres/core/src/sake/extensions.scm"))
              (if (file-exists? prelude-file)
                  `(begin (include ,prelude-file)
                          (include ,sake-extensions))))
           (include ,(string-append dir file))
           ,@(map (lambda (t) `(task-run ,t)) tasks)
           ;;  (with-exception-catcher
           ;;    (lambda (ex)
           ;;      (if (unbound-global-exception? ex) 
           ;;        (err ,(string-append "task '" (symbol->string task) "' not found in " file))
           ;;        (raise ex)))
           ;;    (task-run ,task))
           ))
  (info "exiting directory " dir))

