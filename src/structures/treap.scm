;;;============================================================================
;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;;============================================================================

;; Treaps are a variant of balanced binary tree. By making some use of
;; randomness, we have a BBT that is expected to be of logarithmic height
;; This is a functional implementation
;; http://pavpanchekha.com/blog/treap.html
;; http://en.wikipedia.org/wiki/Treap

(define-structure node p key val left right)


;;; TODO: MAKE OPTIONAL >, <, equal?

;; Transform the treap into a list

(define (treap->list root)
  (if (node? root)
      `(,(node-key root) ,(treap->list (node-left root)) ,(treap->list (node-right root)))
      root))

;; Left rotate

(define (treap:left-rotate node)
  (let ((left (node-left node)))
    (make-node (node-p left)
               (node-key left)
               (node-val left)
               (node-left left)
               (make-node (node-p node)
                          (node-key node)
                          (node-val node)
                          (node-right left)
                          (node-right node)))))

;; Right rotate

(define (treap:right-rotate node)
  (let ((right (node-right node)))
    (make-node (node-p right)
               (node-key right)
               (node-val right)
               (make-node (node-p node)
                          (node-key node)
                          (node-val node)
                          (node-left node)
                          (node-left right))
               (node-right right))))

;; Containment test

(define (treap:contains? root key)
  ;; TODO: let (node-key)
  (cond
   ((not root)
    #f)
   ((< key (node-key root))
    (treap:contains? (node-left root) key))
   ((> key (node-key root))
    (treap:contains? (node-right root) key))
   ((equal? key (node-key root))
    #t)
   (else (error "This code shouldn't be reached -- treap:contains?"))))

;; Get a node

(define (treap:get root key)
  ;; TODO: let (node-key)
  (cond
   ((not root)
    (raise 'not-found))
   ((< key (node-key root))
    (treap:get (node-left root) key))
   ((> key (node-key root))
    (treap:get (node-right root) key))
   ((equal? key (node-key root))
    (node-val root))
   (else (error "This code shouldn't be reached -- treap:get"))))

;; Set a node

(define (treap:set root key val #!key (p (random-integer 1000000000)))
  (cond
   ((not root)
    (make-node p key val #f #f))
   ((< key (node-key root))
    (let ((new (make-node (node-p root)
                          (node-key root)
                          (node-val root)
                          (treap:set (node-left root)
                                     key
                                     val
                                     p: p)
                          (node-right root))))
      (if (< (node-p (node-left new))
             (node-p new))
          (treap:left-rotate new)
          new)))
   ((> key (node-key root))
    (let ((new (make-node (node-p root)
                          (node-key root)
                          (node-val root)
                          (node-left root)
                          (treap:set (node-right root)
                                     key
                                     val
                                     p: p))))
      (if (< (node-p (node-right new))
             (node-p new))
          (treap:right-rotate new)
          new)))
   ((equal? key (node-key root))
    root
    (make-node (node-p root)
               (node-key root)
               val
               (node-left root)
               (node-right root)))
   (else (error "This code shouldn't be reached -- treap:set"))))

;; Split treap

(define (treap:split root key)
  (let ((ins (treap:set root key #f p: -1)))
    `(,(node-left ins) ,(node-right ins))))

;; Merge treaps

(define (treap:merge left right)
  (cond
   ((not left)
    right)
   ((not right)
    left)
   ((< (node-p left) (node-p right))
    (make-node (node-p left)
               (node-key left)
               (node-val left)
               (node-left left)
               (treap:merge (node-right left)
                            right)))
   (else
    (make-node (node-p right)
               (node-key right)
               (node-val right)
               (treap:merge (node-left right)
                            left)
               (node-right right)))))

;; Deletion

(define (treap:delete root key)
  (cond
   ((not root)
    (raise 'not-found))
   ((< key (node-key root))
    (make-node (node-p root)
               (node-key root)
               (node-val root)
               (treap:delete (node-left root)
                             key)
               (node-right root)))
   ((> key (node-key root))
    (make-node (node-p root)
               (node-key root)
               (node-val root)
               (node-left root)
               (treap:delete (node-right root)
                             key)))
   ((equal? key (node-key root))
    (treap:merge (node-left root)
                 (node-right root)))))

;; (let ((treap #f))
;;  (set! treap (treap:set treap 5 'a))
;;  (set! treap (treap:set treap 7 'b))
;;  (assert (equal? (treap:get treap 5) 'a))
;;  (assert (equal? (treap:get treap 7) 'b))
;;  (set! treap (treap:set treap 2 'c))
;;  (assert (equal? (treap:get treap 2) 'c))
;;  (set! treap (treap:set treap 2 'd))
;;  (assert (equal? (treap:get treap 2) 'd))
;;  (set! treap (treap:delete treap 5))
;;  (assert (not (treap:contains? treap 5))))

;; (let ((treap #f))
;;   (let recur ((i 0))
;;     (unless (> i 100)
;;             (recur (+ 1 i))
;;             (set! treap (treap:set treap i #f))))
;;   (assert (< (treap:depth treap) 40)))