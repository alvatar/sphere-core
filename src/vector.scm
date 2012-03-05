;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

(declare (standard-bindings)
         (extended-bindings)
         (block))

;;; for-each for vectors
;;; TODO: optimize

(define (vector-for-each proc vec . vecs)
  (let ((lists (append (list (vector->list vec))
                       (map vector->list vecs))))
    (apply for-each (cons proc lists))))

;;; map for vectors
;;; TODO: optimize

(define (vector-map proc vec)
  (list->vector (map proc (vector->list vec))))

;;; u8vector reverse

(define (u8vector-reverse v)
  (let* ((l (u8vector-length v))
         (r (make-u8vector l)))
    (let loop ((src-idx 0) (target-idx (- l 1)))
      (u8vector-set! r target-idx (u8vector-ref v src-idx))
      (if (> target-idx 0)
          (loop (+ src-idx 1) (- target-idx 1))))
    r))

;;; Move sub-u8vector 
;;; TODO: TEST

(define (subu8vector-move! src src-start src-end dst dst-start)
  ;; Copy direction must be selected in case src and dst are the same
  ;; vector.
  (if (< src-start dst-start)
      (let loop1 ((i (- src-end 1))
                  (j (- (+ dst-start (- src-end src-start)) 1)))
        (if (< i src-start)
            dst
            (begin
              (u8vector-set! dst j (u8vector-ref src i))
              (loop1 (- i 1)
                     (- j 1)))))
      (let loop2 ((i src-start)
                  (j dst-start))
        (if (< i src-end)
            (begin
              (u8vector-set! dst j (u8vector-ref src i))
              (loop2 (+ i 1)
                     (+ j 1)))
            dst))))

;;; u8vector-invert! 

(define (u8vector-invert! v)
  (let loop ((i (u8vector-length v)))
      (if (not (zero? i))
          (let ((i (- i 1)))
            (u8vector-set! v i (##fixnum.bitwise-xor
                                (u8vector-ref v i)
                                255))
            (loop i)))))

;;; dump-u8vector-port-to-other-u8vector-port 

(define (dump-u8vector-port-to-other-u8vector-port content-in
                                                   #!optional
                                                   (content-out '()))
  (if (null? content-out)
      (call-with-output-u8vector
       '()
       (lambda (port)
         (dump-u8vector-port-to-other-u8vector-port content-in port)))

      (let* ((tmp-bufsize (* 50 1024))
             (tmp-buffer (make-u8vector tmp-bufsize)))
        (let loop ()
          (let ((n (read-subu8vector tmp-buffer 0 tmp-bufsize content-in)))
            (if (> n 0)
                (begin
                  (write-subu8vector tmp-buffer 0 n content-out)
                  (loop))))))))
