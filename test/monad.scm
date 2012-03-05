;;; The "functional world":
;;
;; (define (paint x y)
;;   (lambda (world)
;;     (values 'asdf (+ world 1))))
;;
;; (do->
;;  (world <- get)
;;  (paint 9 9)
;;  (return world))



;;; The 'option or maybe' monad
;;
;; (define fail (vector 'fail))
;;
;; (define (unit x) x)
;;
;; (define (bind m f)
;;   (if (eq? m fail)
;;       fail
;;       (f m)))
;;
;; (define (foo-bar-baz x)
;;   (let ((foo-val (foo x)))
;;     (if (eq? foo-val fail)
;;         fail
;;         (let ((bar-val (bar foo-val)))
;;           (if (eq? bar-val fail)
;;               fail
;;               (baz bar-val))))))
;;
;; (define (foo-bar-baz x) (compose* baz (compose* bar (foo x))))
;; ; ...where compose* is the bind operation of the monad


;;; Example by Neelakantan Krishnaswami using do-> notatoin
;;
;; Thread the state through some calls
;;
;; (define (>>= m-a f)
;;   (lambda (state)
;;     (receive (a state*) (m-a state)
;;              ((f a) state*))))
;;
;; ;; The type constructor (which could be a record also): M[a] = state -> (a, state)
;;
;; (define (return x)
;;   (lambda (state)
;;     (values x state)))
;;
;; ;; Takes a state and returns it as a value
;;
;; (define (get state)
;;   (values state state))
;;
;; ;; Takes a state and returns a monadic value that replaces the old state with the new state
;;
;; (define (set new-state)
;;   (lambda (state)
;;     (values #f new-state)))
;;
;; (define (run m state)
;;   (m state)) 
;;
;; (define (number-tree tree)
;;   (if (pair? tree) 
;;       (begin (>>= (number-tree (car tree))
;;                   (lambda (left-subtree) 
;;                     (>>= (number-tree (cdr tree))
;;                          (lambda (right-subtree) 
;;                            (return (cons left-subtree right-subtree))))))) 
;;       (begin (>>= get
;;                   (lambda (n)
;;                     (>>= (set (+ n 1))
;;                          (lambda (dont-care) 
;;                            (return n))))))))
;;
;; (define (number-tree2 tree) 
;;   (if (pair? tree) 
;;       (do-> (left-subtree <- (number-tree2 (car tree))) 
;;             (right-subtree <- (number-tree2 (cdr tree))) 
;;             (return (cons left-subtree right-subtree))) 
;;       (do-> (n <- get) 
;;             (set (+ n 1)) 
;;             (return n)))) 
;;
;; (receive (value state) (run-> (number-tree2 '(a ((a)) (a) (a) a)) 1)
;;          (pp value)
;;          (pp state))


;;; Example using letM* notatoin
;; 
;; (define (make-numbered-value tag val) (cons tag val))
;;                                         ; accessors of the components of the value
;; (define (nvalue-tag tv) (car tv))
;; (define (nvalue-val tv) (cdr tv))
;; 
;; (define (return val)
;;   (lambda (curr_counter)
;;     (make-numbered-value curr_counter val)))
;; 
;; (define (>>= m f)
;;   (lambda (curr_counter)
;;     (let* ((m_result (m curr_counter))
;;            (n1 (nvalue-tag m_result))   ; result of the delayed computation
;;            (v  (nvalue-val m_result))   ; represented by m
;;            
;;            (m1 (f v))                   ; feed the result to f, get another m1
;;            )
;;       (m1 n1))))   
;; 
;; (define incr 
;;   (lambda (n)
;;     (make-numbered-value (+ 1 n) n)))
;; 
;; (define (make-node val kids)
;;   (>>=
;;    incr
;;    (lambda (counter)
;;      (return (cons (make-numbered-value counter val) kids)))))
;; 
;; (define (build-btree-r depth)
;;   (if (zero? depth) (make-node depth '())
;;       (>>=
;;        (build-btree-r (- depth 1))
;;        (lambda (left-branch)
;;          (>>=
;;           (build-btree-r (- depth 1))
;;           (lambda (right-branch)
;;             (make-node depth (list left-branch right-branch))))))))
;; 
;; (runM (build-btree 3) 100)
;; 
;; (define (build-btree depth)
;;   (if (zero? depth) (make-node depth '())
;;       (letM* ((left-branch (build-btree (- depth 1)))
;;               (right-branch (build-btree (- depth 1))))
;;              (make-node depth (list left-branch right-branch)))))
