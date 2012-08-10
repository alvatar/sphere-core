;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binary Search Tree (specializes binary-tree)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Well, <? should 

;;; Create an empty BST

(define (bst:make-empty)
  '())

;;; BST member query

(define (bst:member? node elem <? fail found)
  (let recur ((current node))
    (if current
        (let ((content (binary-tree:node-content current)))
          (cond
           ((<? elem content)
            (recur (binary-tree:node-left current)))
           ((<? content elem)
            (recur (binary-tree:node-right current)))
           (else
            (found elem))))
        (fail))))

;;; BST insert element

(define (bst:insert node elem <? already-there)
  (define (node-insert node)
    (if (binary-tree:leaf? node)
        (leaf-insert node)
        (let ((content (binary-tree:node-content node))
              (left (binary-tree:node-left node))
              (right (binary-tree:node-right node)))
          (cond
           ((<? elem content)
            (binary-tree:make-node content
                                   (node-insert left)
                                   right))
           ((<? content elem)
            (binary-tree:make-node content
                                   left
                                   (node-insert right)))
           (else (already-there node))))))
  (define (leaf-insert leaf)
    (let ((content (binary-tree:node-content leaf)))
      (cond
       ((<? elem content)
        (binary-tree:make-node elem
                               (binary-tree:make-leaf elem)
                               (binary-tree:make-leaf content)))
       ((<? content elem)
        (binary-tree:make-node content
                               (binary-tree:make-leaf content)
                               (binary-tree:make-leaf elem)))
       (else (already-there leaf)))))
  (if node
      (node-insert node)
      (binary-tree:make-leaf elem)))

;;; BST remove element
;;; TODO: INCOMPLETE!!!

(define (bst:remove node elem <? already-there)
  (define (leaf-remove leaf)
    (let ((content (binary-tree:node-content leaf)))
      (cond
       ((<? elem content)
        (bst:make-empty))
       ((<? content elem)
        (bst:make-empty))
       (else
        leaf))))
  (define (root-remove parent left right <?)
    (cond
     ((not right) left)
     ((not left) right)
     (else
      (let ((min-elm (tree-min right)))
        (binary:tree:make-node min-elm
                               left
                               (tree-delete-min right))))))
  (define (node-remove node)
    (let ((content (binary-tree:node-content node))
          (left (binary-tree:node-left node))
          (right (binary-tree:node-right node)))
      (cond
       ((<? elem content)
        (binary-tree:make-node content
                               (node-remove left)
                               right))
       ((<? content elem)
        (binary-tree:make-node content
                               left
                               (node-remove right)))
       (else
        ))))
  (cond
   ((binary-tree:empty? node)
    node)
   ((binary-tree:leaf? node)
    (leaf-remove node))
   (else
    (node-remove node))))
