;;------------------------------------------------------------------------------
;;!! Stage 1: Sake and Spheres program

(define sphere-path
  (make-parameter "~~spheres/core/"))
(define sake-extensions-path
  (make-parameter "~~spheres/sake-extensions/src/"))

(define-task init ()
  (sake#default-clean)
  (make-directory (current-build-directory))
  (make-directory (current-lib-directory)))

(define-task compile-stage-1 (init)
  ;; Compile Sake program
  (info/color 'green "Compiling Sake...")
  (if ((newer-than? "") (string-append (current-source-directory) "sake/sake.scm"))
      (gambit-compile-file
       (string-append (current-source-directory) "sake/sake.scm")
       output: (string-append (current-build-directory) "sake")
       options: "-exe"))
  ;; Compile Spheres program
  (info/color 'green "Compiling Spheres...")
  (gambit-compile-file
   (string-append (current-source-directory) "spheres/spheres.scm")
   output: (string-append (current-build-directory) "spheres")
   options: "-exe"))

(define-task post-compile-stage-1 ()
  ;; Install Sake program
  (info/color 'green "Installing Sake...")
  (delete-file "~~/bin/sake")
  (make-directory "~~spheres/core/src/sake")
  (if (not (file-exists? (sake-extensions-path)))
      (make-directory (sake-extensions-path)))
  (copy-file (string-append (current-build-directory) "sake") "~~/bin/sake")
  (copy-files (fileset dir: (string-append (current-source-directory) "sake")
                       test: (ends-with? ".scm")
                       recursive: #t)
              "~~spheres/core/src/sake")
  ;; Install some Sake extensions
  (info/color 'green "Installing Sake extensions...")
  (copy-file (string-append (current-source-directory) "internal/tiny.scm")
             (string-append (sake-extensions-path) "tiny.scm"))
  (copy-file (string-append (current-source-directory) "sake/extensions/core-macros.scm")
             (string-append (sake-extensions-path) "core-macros.scm"))
  (copy-file (string-append (current-source-directory) "sake/extensions/core.scm")
             (string-append (sake-extensions-path) "core.scm"))
  
  ;; Install Spheres program
  (info/color 'green "Installing Spheres...")
  (delete-file "~~/bin/spheres")
  (copy-file (string-append (current-build-directory) "spheres") "~~/bin/spheres"))

(define-task stage-1 (compile-stage-1 post-compile-stage-1)
  'stage-1)

;;------------------------------------------------------------------------------
;;!! Stage 2: Spheres pipeline and Macro Expansion

(define spheres-module-system-path "~~spheres/spheres#.scm")
(define riaxpander-module-system-path "~~spheres/core/src/riaxpander/riaxpander-gambit.scm")

(define-task compile-stage-2 ()
  ;; Compile Riaxpander
  (info/color 'green "Compiling Riaxpander...")
  (gambit-compile-file
   (string-append (current-source-directory) "riaxpander/riaxpander.scm")
   output: (string-append (current-build-directory) "riaxpander.o1")))

(define-task post-compile-stage-2 ()
  ;; Install spheres# directly in the spheres directory
  (copy-file "src/spheres#.scm" spheres-module-system-path)
  ;; Install Riaxpander
  (info/color 'green "Installing macro expander")
  (copy-file (string-append (current-build-directory) "riaxpander.o1")
             "~~spheres/riaxpander.o1")
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
  '(base
    functional
    ffi
    lazy
    testing))

(define-task compile-stage-3 ()
  (for-each (lambda (m)
              (sake#compile-module m cond-expand-features: '(debug) version: '(debug))
              (sake#compile-module m cond-expand-features: '(optimize)))
            modules))

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
  (delete-file spheres-module-system-path)
  (delete-file (string-append (user-info-home (user-info (user-name))) "/.gambcini"))
  (delete-file (string-append (sake-extensions-path) "core.scm"))
  (delete-file "~~bin/sake")
  (delete-file "~~bin/spheres")
  (delete-file "~~/#spheres"))
