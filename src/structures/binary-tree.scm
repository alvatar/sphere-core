;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binary tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; TODO: Argument checks
;;;;;; TODO: define-type??

(define (binary-tree:make-node content left right)
  (list content left right))

(define (binary-tree:make-leaf content)
  (list content #f #f))

(define (binary-tree:node-content node)
  (car node))

(define (binary-tree:node-left node)
  (cadr node))

(define (binary-tree:node-right node)
  (caddr node))

(define (binary-tree:node? e)
  (and (list? e)
       (= (length e) 3)))

(define (binary-tree:leaf? e)
  (and (binary-tree:node? e)
       (not (binary-tree:node-left e))
       (not (binary-tree:node-right e))))

(define (binary-tree:empty? e)
  (null? e))

;;; Create a binary-tree node from a list of three elements

(define (list->binary-tree-node l)
  (if (and (not (null? l)) (= (length l) 3))
    (binary-tree:make-node (car l)
                     (cadr l)
                     (caddr l))
    (error "This list can't be converted to a binary-tree-node")))

;;; Create a b-tree from a list
;; TODO: Consider these cases:
;; N-ary-tree
;; binary tree
;; Ordering?

(define (list->binary-tree l)
  (error "unimplemented"))

;;; Is a member of the tree?

(define (binary-tree:member? e node)
  (if (binary-tree:leaf? node)
      (eq? (binary-tree:content node) e)
      (or (eq? (binary-tree:content node) e)
          (binary-tree:member? e (binary-tree:left node))
          (binary-tree:member? e (binary-tree:right node)))))

;;; Left-most element of the tree

(define (binary-tree:leftmost node)
  (let recur ((current-node node))
    (if (binary-tree:node? current-node)
        (recur (binary-tree:left current-node))
        (binary-tree:node-content current-node))))

;;; Find the left-most node in a level

(define (binary-tree:leftmost-in-level node n)
  (let recur ((current-level 0)
              (current-node node))
    (if (and (< current-level n)
             (binary-tree:node? current-node))
        (recur (add1 current-level)
               (binary-tree:node-left current-node))
        (binary-tree:node-content current-node))))

;;; Find the right-most node in a level

(define (binary-tree:rightmost-in-level node n)
  (let recur ((current-level 0)
              (current-node node))
    (if (and (< current-level n)
             (binary-tree:node? current-node))
        (recur (add1 current-level)
               (binary-tree:node-right current-node))
        (binary-tree:node-content current-node))))

;;; Right-most element of the tree

(define (binary-tree:rightmost node)
  (let recur ((current-node node))
    (if (binary-tree:node? current-node)
        (recur (binary-tree:right current-node))
        (binary-tree:node-content current-node))))

;;; Reverse tree

(define (binary-tree:reverse node)
  (if (binary-tree:leaf? node)
      node
      (binary-tree:make-node
       (binary-tree:node-content node)
       (binary-tree:reverse
        (binary-tree:node-right node))
       (binary-tree:reverse
        (binary-tree:node-left node)))))

(define (binary-tree:preorder node)
  (let recur ((current node)
              (lis '()))
    (if (binary-tree:leaf? current)
        (cons (binary-tree:node-content current) lis)
        (cons (binary-tree:node-content current)
              (recur (binary-tree:node-left current)
                     (recur (binary-tree:node-right current)
                            lis))))))

;;; Get the b-tree root node

(define (binary-tree:root tree)
  (car tree))

;;; Extract a list of all nodes in the same level

(define (binary-tree:level tree n)
  (define (extract-level tree-pos target current)
    (cond
     ((equal? tree-pos 'leaf)
      '())
     ((and (< current target) (not (null? (binary-tree:node-left tree-pos))))
      (append
       (extract-level (list->binary-tree (binary-tree:node-left tree-pos)) target (add1 current))
       (extract-level (list->binary-tree (binary-tree:node-right tree-pos)) target current)))
     (else
      (cons (binary-tree:node-content tree-pos)
            (extract-level (list->binary-tree (binary-tree-sibling tree-pos))
                           target
                           current)))))
  (extract-level tree n 0))

;;; Btree pretty printing

(define (binary-tree-pp tree)
  (error "unimplemented"))
