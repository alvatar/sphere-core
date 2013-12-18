;;; Copyright (c) 2013 by Álvaro Castro-Castilla
;;; Spheres: manage packages from SchemeSpheres.org

(define help:general
  "
Usage: spheres [command] [operand]

Commands:
    install [sphere]
        install all dependencies in current project (not implemented)
        [sphere]        install sphere
    uninstall [sphere]
        [sphere]        uninstall sphere
    update
        install all dependencies in current project
        --all           update all spheres in the system
        [sphere]        install a sphere
    search
        not implemented
    set
        not implemented

")

(define (main)
  (cond
   ((or (null? (cdr (command-line)))
        (string=? (cadr (command-line)) "help"))
    (display help:general))
   ;; Command: install
   ((string=? (cadr (command-line)) "install")
    (if (null? (cddr (command-line)))
        (begin
          (println "install: Install dependencies NOT IMPLEMENTED")
          (exit 1))
        (let ((target-id (caddr (command-line))))
          (with-input-from-process
           (list path: "curl"
                 arguments: '("https://raw.github.com/alvatar/spheres/master/universe.scm"))
           (lambda () (let ((index (read-all)))
                   (let recur ((i index))
                     (cond ((null? i) #f)
                           ((and (equal? sphere: (caar i))
                                 (assq id: (cdar i))                                
                                 (string=? (cadr (assq id: (cdar i))) target-id))
                            (if (file-exists? (string-append (path-expand "~~spheres/") target-id))
                                (println (string-append "Sphere " target-id " is already installed"))
                                (shell-command
                                 (string-append "git clone "
                                                (cadr (assq repository: (cdar i)))
                                                " "
                                                (path-expand "~~spheres/")
                                                target-id)))
                            (shell-command
                             (string-append
                              "cd " (path-expand "~~spheres/") target-id " && sake")))
                           (else (recur (cdr i)))))))))))
   ;; Command: uninstall
   ((string=? (cadr (command-line)) "uninstall")
    (if (null? (cddr (command-line)))
        (display help:general)
        (let ((sphere-path (string-append (path-expand "~~spheres") (caddr (command-line)))))
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
                                            (string-append full-path "/"))
                                     (delete-directory full-path)
                                     (recur (cdr files) path))
                              (begin
                                (delete-file full-path)
                                (recur (cdr files) path)))))))
                (delete-directory sphere-path))
              (begin
                (println (string-append "Sphere " (caddr (command-line)) " is not installed"))
                (exit 1))))))
   ;; Command: update
   ((string=? (cadr (command-line)) "update")
    (println "update: NOT IMPLEMENTED"))
   ;; Command: search
   ((string=? (cadr (command-line)) "search")
    (println "search: NOT IMPLEMENTED"))
   ;; Command: set
   ((string=? (cadr (command-line)) "set")
    (println "set: NOT IMPLEMENTED"))
   ;; Unrecognized command
   (else
    (println "spheres: unrecognized command. Try \"spheres help\" for more information. ")
    (exit 1)))
  (exit 0))

;; Run!
(main)
