(with-exception-catcher
 (lambda (e)
   (if (unbound-global-exception? e)
       (begin (info/color 'brown "Bootstrapping Base")
              (load "src/alexpander.scm"))))
 (lambda () ##current-expander))
(include "src/sphere#.scm")
(include "src/sake-extensions.scm")

(define modules
  '(ffi
    testing))

(define prelude-system-path "~~spheres/prelude#.scm")
(define alexpander-system-path "~~spheres/core/lib/alexpander.o1")

(define-task compile ()
  (println "Compiling Alexpander...")
  (shell-command "gsc -prelude '(declare (not safe))' -o lib/alexpander.o1 src/alexpander.scm")
  (for-each (lambda (m) (sake:compile-c-to-o (sake:compile-to-c m))) modules))

(define-task test ()
  (sake:test-all))

(define-task clean ()
  (sake:default-clean))

(define-task install ()
  ;; Install compiled module files
  (for-each sake:install-compiled-module modules)
  (sake:install-system-sphere)
  ;; Install prelude directly in the spheres directory
  (copy-file "src/prelude#.scm" prelude-system-path)
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
              (copy-file ofile alexpander-system-path))
            (if (file-exists? alexpander-system-path)
                (begin
                  (warn "Removing old Alexpander (no compiled version installed)")
                  (delete-file alexpander-system-path)))))))

(define-task uninstall ()
  (sake:uninstall-system-sphere)
  (delete-file prelude-system-path))

(define-task all (compile install)
  'all)
