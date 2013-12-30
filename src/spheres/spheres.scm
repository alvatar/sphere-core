;;; Copyright (c) 2013 by Álvaro Castro-Castilla
;;; Spheres: manage packages from SchemeSpheres.org

(include "../arguments.scm")


(define *help:general* #<<end-help-string

Usage: spheres [command] [operand]

Commands:
    install [sphere]
        install all dependencies in current project (not implemented)
        [sphere]        install sphere
    uninstall [sphere]
        [sphere]        uninstall sphere
    update
        update all dependencies in current project
        --all           update all spheres in the system
        [sphere]        install a sphere
    search
        not implemented
    set
        not implemented

end-help-string
)

;; List of options
;; 1 means that takes an argument, 0 it doesn't
(define *options*
  '((#\g 0 "global")))

;; Command: HELP
(define (help-cmd cmd opts args)
  (define help-topics
    `(("install" ,@*help:general*)
      ("uninstall" ,@*help:general*)))
  (define num-args (length args))
  (handle-opts! opts `(("help" ,@(lambda (val) #t))))
  (cond
   ((zero? num-args)
    (println *help:general*))
   ((= 1 num-args)
    (let* ((arg (car args))
           (res (assoc arg help-topics)))
      (if res
          (println (cdr res))
          (die/error "Unknown help topic:" arg))))
   (else
    (die/error "Invalid arguments passed to help:" args))))

;; Command: INSTALL
(define (install-cmd cmd opts args)
  (define version #t)
  (define compile #t)
  (define ignore-dependencies #f)

  (handle-opts!
   opts
   `(("version"
      ,@(lambda (val)
          (set! version
                (with-input-from-string val read))))
     ("ignore-dependencies"
      ,@(lambda (val)
          (set! ignore-dependencies (not (equal? val "no")))))))

  (if (and (not (eqv? #t version))
           (not (= 1 (length args))))
      (die/error "When specifying a version, only one package can be \
                  installed at a time:" args))
  ;; Check arguments
  (if (null? args)
      (die/error "Automatic installation of dependencies NOT IMPLEMENTED"))
  (for-each (lambda (arg)
              (if (string=? "core" arg)
                  (die/error "Core Sphere is not intended to be installed with the 'spheres' program.")))
            args)
  ;; Do the task
  (with-input-from-process
   (list path: "curl"
         arguments: '("https://raw.github.com/alvatar/spheres/master/universe.scm"))
   (lambda ()
     (let ((index (read-all)))
      (for-each
       (lambda (target-id)
         (let recur ((i index))
           (cond ((null? i)
                  (die/error "No Sphere found with name" target-id "in the repository"))
                 ((and (equal? sphere: (caar i))
                       (assq id: (cdar i))                                
                       (string=? (cadr (assq id: (cdar i))) target-id))
                  ;; Clone or update
                  (if (file-exists? (string-append (path-expand "~~spheres/") target-id))
                      (begin
                        (println (string-append "*** INFO -- Sphere " target-id " is already installed. Pulling latest changes."))
                        (shell-command
                         (string-append "cd " (path-expand "~~spheres/") target-id " && git pull")))
                      (shell-command
                       (string-append "git clone "
                                      (cadr (assq repository: (cdar i)))
                                      " "
                                      (path-expand "~~spheres/")
                                      target-id)))
                  ;; Run Sake
                  (shell-command
                   (string-append
                    "cd " (path-expand "~~spheres/") target-id " && sake")))
                 (else (recur (cdr i))))))
       args))))
  ;; End info
  (println (string-append "*** INFO -- The following Spheres have been successfully installed:"))
  (for-each (lambda (target-id) (println (string-append "*** INFO --      * " target-id))) args))

;; Command: UNINSTALL
(define (uninstall-cmd cmd opts args)
  (define version #t)
  (handle-opts!
   opts
   `(("version"
      ,@(lambda (val)
          (set! version
                (with-input-from-string val read))))))
  (ensure-args! args)
  (if (and (not (eqv? #t version))
           (not (= 1 (length args))))
      (die/error "When specifying a version, only one package can be \
                  uninstalled at a time:" args))
  ;; Special handling of the Core Sphere
  (if (string=? "core" (car args))
      (die/error "Core Sphere is not intended to be uninstalled with the 'spheres' program. Please do so manually: remove " (path-expand "~~spheres/")))
  ;; Do the task
  (let ((sphere-path (string-append (path-expand "~~spheres") (car args))))
    (if (file-exists? sphere-path)
        ;; All this just to remove files recursively
        (begin
          (let ((files (directory-files (list path: sphere-path
                                              ignore-hidden: 'dot-and-dot-dot))))

            (let recur ((files files)
                        (path (path-expand sphere-path)))
              (if (null? files)
                  'done
                  (let ((full-path (string-append path "/" (car files))))
                    (if (eqv? (file-info-type (file-info full-path)) 'directory)
                        (begin (recur (directory-files (list path: full-path
                                                             ignore-hidden: 'dot-and-dot-dot))
                                      full-path)
                               (delete-directory full-path)
                               (recur (cdr files) path))
                        (begin
                          (delete-file full-path)
                          (recur (cdr files) path)))))))
          (delete-directory sphere-path))
        (begin
          (println (string-append "Sphere " (car args) " is not installed"))
          (exit 1))))
  ;; End info
  (println (string-append "*** INFO -- The following Spheres have been successfully uninstalled:"))
  (for-each (lambda (target-id) (println (string-append "*** INFO --      * " target-id))) args))

;; Command: INSTALL
(define (update-cmd cmd opts args)
  (die/error "COMMAND NOT IMPLEMENTED. Want to help? a@fourthbit.com"))

;; Command: SEARCH
(define (search-cmd cmd opts args)
  (die/error "COMMAND NOT IMPLEMENTED. Want to help? a@fourthbit.com"))

;; Command: SET
(define (set-cmd cmd opts args)
  (die/error "COMMAND NOT IMPLEMENTED. Want to help? a@fourthbit.com"))

;; Command Unknown
(define (unknown-cmd cmd opts args-sans-opts)
  (die/error "Unknown command:"
             cmd
             "To get a list of options, type 'bh help'"))

(define (main args)
  (let ((commands
         `(("install" ,@install-cmd)
           ("uninstall" ,@uninstall-cmd)
           ("help" ,@help-cmd)
           ("update" ,@update-cmd)
           ("search" ,@search-cmd)
           ("set" ,@set-cmd)
           ("unknown-command" ,@unknown-cmd))))
    (parse-arguments
     (cdr args)
     (lambda (actual-args-sans-opts opts)
       (let* ((args-sans-opts (if (null? actual-args-sans-opts)
                                  '("help")
                                  actual-args-sans-opts))
              (cmd-pair (assoc (car args-sans-opts) commands))
              (cmd (if cmd-pair
                       (cdr cmd-pair)
                       (cdr (assoc "unknown-command" commands)))))
         (cmd (car args-sans-opts)
              opts
              (cdr args-sans-opts))))
     *options*)))

;; Run!
(main (command-line))
