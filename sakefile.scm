(with-exception-catcher
 (lambda (e)
   (if (unbound-global-exception? e)
       (begin (info/color 'brown "Bootstrapping Scheme Spheres")
              (load "src/alexpander.scm"))))
 (lambda () ##current-expander))
(include "src/spheres#.scm")
(include "src/sake/extensions.scm")

;;; Stage 1

(define-task init ()
  (make-directory (current-build-directory))
  (make-directory (current-lib-directory)))

(define-task compile-stage-1 (init)
  (if ((newer-than? "") (string-append (current-source-directory) "sake/sake.scm"))
      (gambit-compile-file
       (string-append (current-source-directory) "sake/sake.scm")
       output: (string-append (current-build-directory) "sake")
       options: "-exe")))

(define-task install-stage-1 ()
  (delete-file "~~/bin/sake")
  (make-directory "~~spheres/core/src/sake")
  (copy-file (string-append (current-build-directory) "sake") "~~/bin/sake")
  (copy-files (fileset dir: (string-append (current-source-directory) "sake")
                       test: (ends-with? ".scm")
                       recursive: #t)
              "~~spheres/core/src/sake"))

(define-task stage-1 (compile-stage-1 install-stage-1)
  'stage-1)

;;; Stage 2

(define prelude-module-system-path "~~spheres/prelude.scm")
(define spheres-module-system-path "~~spheres/spheres#.scm")
(define alexpander-module-system-path "~~spheres/core/lib/alexpander.o1")

(define-task compile-stage-2 (init)
  (println "Compiling Alexpander...")
  (shell-command "gsc -f -prelude '(declare (not safe))' -o lib/alexpander.o1 src/alexpander.scm"))

(define-task install-stage-2 ()
  ;; Install prelude and spheres# directly in the spheres directory
  (copy-file "src/prelude.scm" prelude-module-system-path)
  (copy-file "src/spheres#.scm" spheres-module-system-path)
  (make-directory "~~spheres/core/lib")
  ;; Install compiled Alexpander if newer than source, otherwise remove from installation
  (let ((ofile "lib/alexpander.o1")
        (scmfile "src/alexpander.scm"))
    (if (file-exists? ofile)
        (if (< (time->seconds
                (file-info-last-modification-time (file-info scmfile)))
               (time->seconds
                (file-info-last-modification-time (file-info ofile))))
            (begin
              (info/color 'green "Installing compiled Alexpander")
              (copy-file ofile alexpander-module-system-path))
            (if (file-exists? alexpander-module-system-path)
                (begin
                  (warn "Removing old Alexpander (no compiled version installed)")
                  (delete-file alexpander-module-system-path)))))))

(define-task stage-2 (compile-stage-2 install-stage-2)
  'stage-2)

;;; Stage 3

(define modules
  '(ffi
    testing))

(define-task compile-stage-3 ()
  (for-each (lambda (m) (sake#compile-c-to-o (sake#compile-to-c m compiler-options: '(debug)))) modules)
  (for-each (lambda (m) (sake#compile-c-to-o (sake#compile-to-c m))) modules))

(define-task install-stage-3 ()
  (for-each (lambda (m) (sake#install-compiled-module m versions: '(() (debug)))) modules)
  (sake#install-sphere-to-system))

(define-task stage-3 (compile-stage-3 install-stage-3)
  'stage-3)

;;;

(define-task uninstall ()
  (sake#uninstall-sphere-from-system)
  (delete-file prelude-module-system-path)
  (delete-file spheres-module-system-path)
  (delete-file "~~bin/sake"))

(define-task test ()
  (sake#test-all))

(define-task clean ()
  (sake#default-clean))
