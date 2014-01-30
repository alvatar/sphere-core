;-------------------------------------------------------------------------------
; Filesets and combinators
;-------------------------------------------------------------------------------

(define (extension=? ext)
  (ends-with? ext))

(define (ends-with? end)
  (lambda (name)
    (and (>= (string-length name) (string-length end))
         (string=? (substring name (- (string-length name) (string-length end)) (string-length name))
                   end))))

(define (newer-than? filename1 #!key
                     (dir (current-build-directory)))
  (lambda (filename2)
    (> (time->seconds (file-last-modification-time filename2))
       (time->seconds (file-last-modification-time filename1)))))

(define (newer-than/extension? ext #!key
                               (dir (current-build-directory)))
  (lambda (name)
    (let ((name0 (string-append 
                  dir
                  (path-strip-extension (path-strip-directory name))
                  ext)))
      (or (not (file-exists? name0))
          (>= (time->seconds (file-last-modification-time name))
              (time->seconds (file-last-modification-time name0)))))))

(define (f-and . ts)
  (lambda (name)
    (let f-and ((ts ts))
      (or (null? ts)
          (and ((car ts) name)
               (f-and (cdr ts)))))))

(define (f-or . ts)
  (lambda (name)
    (let f-or ((ts ts))
      (and (pair? ts)
           (or ((car ts) name)
               (f-or (cdr ts)))))))

(define (shift fn)
  (lambda (t)
    (lambda (name)
      (fn (t name)))))
    
(define f-not (shift not))

(define (any? name) #t)
(define (none? name) #f)

(##define (fileset #!key 
                   (dir (current-directory))
                   (test any?)
                   (recursive #f))
  (let ((dir (path-add-trailing-directory-separator dir)))
    (reduce append '() 
            (map (lambda (name) 
                   (let* ((f (string-append dir name))
                          (childs (if (and recursive (directory? f))
                                      (fileset dir: (path-add-trailing-directory-separator f)
                                               test: test
                                               recursive: recursive)
                                      '())))
                     (if (test f)
                         (cons f childs)
                         childs)))
                 (directory-files `(path: ,dir ignore-hidden: dot-and-dot-dot))))))

;-------------------------------------------------------------------------------
; File handling
;-------------------------------------------------------------------------------

(define (path-add-trailing-directory-separator dir)
  (string-append (path-strip-trailing-directory-separator dir) "/"))

(define (directory? name)
  (eq? (file-type name) 'directory))

(define (regular? name)
  (eq? (file-type name) 'regular))

;;; Make directory

(define (sake#make-directory dir)
  (let ((dir0 (path-strip-trailing-directory-separator dir)))
    (if (file-exists? dir0) #t
        (begin
          (sake#make-directory (path-directory dir0))
          (create-directory dir0)))))

;;; Improved delete-file

(define (sake#delete-file file #!key (recursive #f) (force #t))
  (let ((file (path-expand file)))
    (info "deleting " file)
    (cond
     ((not (file-exists? file)) 'ok)
     ((directory? file)
      (sake#delete-directory file recursive: recursive force: force))
     (else
      (##delete-file file)))))

;;; Improved delete-directory

(define (sake#delete-directory dir #!key (recursive #t) (force #t))
  (if force (for-each ##delete-file (fileset dir: dir recursive: #f test: regular?)))
  (if recursive (for-each (lambda (dir) (sake#delete-file
                                    (path-add-trailing-directory-separator dir)
                                    recursive: recursive
                                    force: force))
                          (fileset dir: dir recursive: #t test: directory?)))
  (if (null? (fileset dir: dir recursive: #t test: any?)) 
      (##delete-directory dir)
      (warn dir " is not empty")))

;;; Delete a list of files

(define (sake#delete-files files)
  (for-each (lambda (f) (sake#delete-file f recursive: #t)) files))

;;; Improved copy-file

(define (sake#copy-file file dest #!key (force #t))
  (let ((file (path-expand file)))
    (cond
     ((directory? file)
      (info "copying " file " to " dest)
      (sake#copy-directory file dest force: force))
     ((and force (file-exists? dest))
      (sake#delete-file dest recursive: #t)
      (sake#copy-file file dest force: #f))
     ((not (file-exists? dest))
      (info "copying " file " to " dest)
      (##copy-file file dest))
     (else
      (warn dest " already exists")))))

;;; Copy a directory

(define (sake#copy-directory file dest #!key (force #t))
  (cond
   ((and force (file-exists? dest))
    (sake#delete-file dest recursive: #t force: #t)
    (sake#copy-directory file dest force: force))
   ((not (file-exists? dest))
    (create-directory dest)
    (for-each
     (lambda (filename)
       (sake#copy-file filename
                       (string-append
                        (path-strip-trailing-directory-separator dest)
                        "/" (path-strip-directory filename))))
     (fileset dir: file recursive: #f)))
   (else
    (warn dest " already exists"))))

;;; Copy a list of files and directories

(define (sake#copy-files files dest #!key (force #t))
  (for-each
   (lambda (file) 
     (sake#copy-file file
                     (string-append (path-strip-trailing-directory-separator dest)
                                    "/"
                                    (path-strip-directory file))
                     force: force))
   files))

(define (sake#read-file file)
  (call-with-input-file (path-expand file)
    (lambda (in) (read-line in #f))))

(define (sake#read-files files)
  (call-with-output-string
   ""
   (lambda (out)
     (for-each (lambda (file) (display (sake#read-file file) out))
               files))))

(define (sake#append-files files dest)
  (call-with-output-file dest
    (lambda (out)
      (for-each (lambda (file) (display (sake#read-file file) out))
                files))))

