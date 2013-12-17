;;; Copyright (c) 2013 by Álvaro Castro-Castilla
;;; Spheres: manage packages from SchemeSpheres.org

(define help:general
  "
Usage: spheres [command] [operand]

Commands:
    install [operand]
        [<none>] install all dependencies in current project
        [sphere] install sphere
    update
        not implemented
    search
        not implemented

")

(define (main)
  (cond
   ((or (null? (cdr (command-line)))
        (string=? (cadr (command-line)) "help"))
    (display help:general))
   ;; Command: install
   ((string=? (cadr (command-line)) "install")
    (println "install"))
   ;; Command: update
   ((string=? (cadr (command-line)) "update")
    (println "update"))
   ;; Command: search
   ((string=? (cadr (command-line)) "search")
    (println "search"))
   ;; Unrecognized command
   (else
    (println "spheres: unrecognized command. Try \"spheres help\" for more information. ")
    (exit 1)))
  (exit 0))

;; Run!
(main)
