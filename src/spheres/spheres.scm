;;#!/usr/bin/env gsi-script
;;; Copyright (c) 2013-2014 by Álvaro Castro-Castilla
;;; Spheres: manage packages from SchemeSpheres.org

(include "../internal/arguments.scm")
(include "../internal/tiny.scm")


(define *help:general* #<<end-help-string

Usage: spheres [command] [operand]

Commands:
    install [flags] [sphere]
        install all dependencies in current project (not implemented)
        -b [branch]     for development purposes, forces a git branch to be used
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
  '((#\g 0 "global")
    (#\b 1 "branch")
    (#\u 0 "update")))

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
  (define branch "master")
  (define update #f)
  (define (index-spheres index)
    (map cdr (filter (lambda (e) (and (pair? e) (eq? (car e) sphere:))) index)))
  (define index-find-sphere
    (lambda (index target)
      (call/cc (lambda (done)
                 (foldl (lambda (x sphere)
                          (let ((id (assq id: sphere)))
                            (and id (string=? target (cadr id)) (done sphere))))
                        #f (index-spheres index))))))
  (define sphere-find-attribute
    (lambda (sphere attr)
      (let ((attr (assq attr sphere))) (and attr (cdr attr)))))
  (define compute-dependencies
    (let ((result '()))
      (lambda (index root-spheres)
        (letrec
            ((recur
              (lambda (sphere)
                (let* ((deps-sym (sphere-find-attribute
                                  (or (index-find-sphere index sphere)
                                      (die/error "Sphere" sphere "not found in the repository")) dependencies:))
                       (deps (and deps-sym (map symbol->string deps-sym))))
                  (if deps
                      (for-each (lambda (dep)
                                  (if (not (member dep result))
                                      (begin
                                        (set! result (cons dep result))
                                        (for-each (lambda (dep) (recur dep)) deps))))
                                deps))))))
          (for-each (lambda (s)
                      (recur s)
                      ;; After adding all the dependencies of a root sphere, add it
                      (if (not (member s result))
                          (set! result (cons s result))))
                    root-spheres)
          (reverse result)))))
  (handle-opts!
   opts
   `(("version"
      ,@(lambda (val)
          (set! version
                (with-input-from-string val read))))
     ("branch"
      ,@(lambda (val)
          (set! branch val)))
     ("update"
      ,@(lambda (val)
          (set! update #t)))
     ("ignore-dependencies"
      ,@(lambda (val)
          (error "Not implemented")
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
  (let* ((curl-process (open-process
                        (list
                         path: "curl"
                         arguments: '("https://raw.github.com/alvatar/spheres/master/universe.scm"))))
         (index (read-all curl-process)))
    (close-port curl-process)
    (if (not (zero? (process-status curl-process)))
        (die/error "Error getting repository. Network problem?"))
    (for-each
     (lambda (target-id)
       (let recur ((i index))
         (cond ((null? i)
                (die/error "Sphere" target-id "not found in the repository"))
               ((and (equal? sphere: (caar i))
                     (assq id: (cdar i))                                
                     (string=? (cadr (assq id: (cdar i))) target-id))
                ;; Clone or update
                (if (file-exists? (string-append (path-expand "~~spheres/") target-id))
                    ;; It seems the sphere is already installed
                    (if (or (member target-id args)
                            update)
                        (begin
                          (println (string-append "*** INFO -- Sphere " target-id " is already installed. Pulling latest changes."))
                          (if (not (zero? (shell-command
                                           (string-append "cd " (path-expand "~~spheres/") target-id " && git pull"))))
                              (die/error "Git error: Check network connectivity. If error persists, uninstall the Sphere and try again.")))
                        (println (string-append "*** INFO -- Not updating " target-id ". Force with --update if you wish otherwise.")))
                    ;; First try with Git protocol, then Https
                    (if (not (or (zero? (shell-command
                                         (string-append "git clone "
                                                        (cadr (assq repository: (cdar i)))
                                                        " "
                                                        (path-expand "~~spheres/")
                                                        target-id)))
                                 (zero? (shell-command
                                         (string-append "git clone "
                                                        (cadr (assq http-repository: (cdar i)))
                                                        " "
                                                        (path-expand "~~spheres/")
                                                        target-id)))))
                        (die/error "Git error: Check network connectivity. If error persists, uninstall the Sphere and try again.")))
                ;; Run Sake (if mentioned in the command line or forcing with --update)
                (if (or (member target-id args)
                        update)
                    (if (not (zero? (shell-command
                                     (string-append
                                      "cd " (path-expand "~~spheres/") target-id
                                      (if branch (string-append " && git checkout " branch))
                                      " && ssake"))))
                        (die/error "Error running Sake. Please contact the Sphere maintainer."))
                    (println (string-append "*** INFO -- Not compiling " target-id ". Force with --update if you wish otherwise."))))
               (else (recur (cdr i))))))
     (compute-dependencies index args)))
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
  (let ((results
         (map
          (lambda (sphere-id)
            (let ((sphere-path (string-append (path-expand "~~spheres") sphere-id)))
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
                    (delete-directory sphere-path)
                    (list sphere-id "OK"))
                  (list error: sphere-id "\033[00;31mERROR -- Sphere not found in the system\033[00m"))))
          args)))
    ;; End info
    (println (string-append "*** INFO -- The following Spheres have been uninstalled:"))
    (foldl (lambda (unused result)
             ;; Errors are encoded as (error: sphere-id 
             (if (eq? error: (car result))
                 (println (string-append "*** INFO --      * "
                                         (cadr result)
                                         ": "
                                         (caddr result)))
                 (println (string-append "*** INFO --      * "
                                         (car result)
                                         ": "
                                         (cadr result)))))
           #f
           results)))

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
             "To get a list of options, type 'spheres help'"))

(define (main . args)
  (let ((commands
         `(("install" ,@install-cmd)
           ("uninstall" ,@uninstall-cmd)
           ("help" ,@help-cmd)
           ("update" ,@update-cmd)
           ("search" ,@search-cmd)
           ("set" ,@set-cmd)
           ("unknown-command" ,@unknown-cmd))))
    (parse-arguments
     args
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
(apply main (cdr (command-line)))

