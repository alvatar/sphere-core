(info/color 'brown "Bootstrapping Scheme Spheres")
(##include "src/spheres#.scm")
(##include "src/sake/extensions/core.scm")

;;------------------------------------------------------------------------------
;;!! Stage 1: Sake and Spheres program

(define sphere-path "~~spheres/core/")

(define-task init ()
  (make-directory (current-build-directory))
  (make-directory (current-lib-directory)))

(define-task compile-stage-1 (init)
  ;; Compile Sake program
  (println "Compiling Sake...")
  (if ((newer-than? "") (string-append (current-source-directory) "sake/sake.scm"))
      (gambit-compile-file
       (string-append (current-source-directory) "sake/sake.scm")
       output: (string-append (current-build-directory) "sake")
       options: "-exe"))
  ;; Compile Spheres program
  (println "Compiling Spheres...")
  (gambit-compile-file
   (string-append (current-source-directory) "spheres/spheres.scm")
   output: (string-append (current-build-directory) "spheres")
   options: "-exe"))

(define-task post-compile-stage-1 ()
  ;; Install Sake program
  (delete-file "~~/bin/sake")
  (make-directory "~~spheres/core/src/sake")
  (copy-file (string-append (current-build-directory) "sake") "~~/bin/sake")
  (copy-files (fileset dir: (string-append (current-source-directory) "sake")
                       test: (ends-with? ".scm")
                       recursive: #t)
              "~~spheres/core/src/sake")
  ;; Install Spheres program
  (delete-file "~~/bin/spheres")
  (copy-file (string-append (current-build-directory) "spheres") "~~/bin/spheres"))

(define-task stage-1 (compile-stage-1 post-compile-stage-1)
  'stage-1)

;;------------------------------------------------------------------------------
;;!! Stage 2: Spheres pipeline and Macro Expansion

(define prelude-module-system-path "~~spheres/prelude.scm")
(define spheres-module-system-path "~~spheres/spheres#.scm")
(define riaxpander-module-system-path "~~spheres/core/src/riaxpander/riaxpander-gambit.scm")

(define-task compile-stage-2 (init)
  'compile-nothing)

(define-task post-compile-stage-2 ()
  ;; Install prelude and spheres# directly in the spheres directory
  (copy-file "src/prelude.scm" prelude-module-system-path)
  (copy-file "src/spheres#.scm" spheres-module-system-path)
  ;; Install Riaxpander files
  (info/color 'green "Installing macro expander")
  (make-directory "~~spheres/core/src/riaxpander")
  (copy-files (fileset dir: (string-append (current-source-directory) "riaxpander")
                       test: (ends-with? ".scm")
                       recursive: #t)
              "~~spheres/core/src/riaxpander")
  ;; Create .gambcini
  (call-with-output-file
      (string-append (user-info-home (user-info (user-name))) "/.gambcini")
    (lambda (f)
      (pp '(let ((spheres-file "~~spheres/spheres#.scm"))
             (if (file-exists? spheres-file)
                 (eval `(include ,spheres-file))
                 (println "spheres#.scm missing -- Did you install Core Sphere?")))
          f))))

(define-task stage-2 (compile-stage-2 post-compile-stage-2)
  'stage-2)

;;------------------------------------------------------------------------------
;;!! Stage 3: Modules

(define modules
  '(arguments
    base
    ffi
    testing))

(define-task compile-stage-3 ()
  (for-each (lambda (m) (sake#compile-module m compiler-options: '(debug))) modules)
  (for-each sake#compile-module modules))

(define-task post-compile-stage-3 ()
  (for-each (lambda (m) (sake#make-module-available m versions: '(() (debug)))) modules))

(define-task stage-3 (compile-stage-3 post-compile-stage-3)
  (sake#install-sphere-to-system))

;;------------------------------------------------------------------------------
;;!! General tasks

(define-task test ()
  (sake#test-all))

(define-task clean ()
  (sake#default-clean))

(define-task install (post-compile-stage-1 post-compile-stage-2)
  (sake#install-sphere-to-system))

(define-task uninstall ()
  (sake#uninstall-sphere-from-system)
  (delete-file prelude-module-system-path)
  (delete-file spheres-module-system-path)
  (delete-file (string-append (user-info-home (user-info (user-name))) "/.gambcini"))
  (delete-file "~~bin/sake")
  (delete-file "~~bin/spheres")
  (delete-file "~~/#spheres")
  (delete-file "~~/prelude.scm"))
