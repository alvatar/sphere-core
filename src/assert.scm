;; TODO: make assertion type hierarchy:

;; (define &condition (make-rtd '&condition '#()))
;; (define &compound (make-rtd '&compound '#((immutable conditions)) &condition))
;; (define &serious (make-rtd '&serious '#() &condition))
;; (define &violation (make-rtd '&violation '#() &serious))
;; (define &assertion (make-rtd '&assertion '#() &violation))
;; (define &who (make-rtd '&who '#((immutable who)) &condition))
;; (define &message (make-rtd '&message '#((immutable message)) &condition))
;; (define &irritants (make-rtd '&irritants '#((immutable irritants)) &condition))

;; (define make-who-condition (rtd-constructor &who))
;; (define make-message-condition (rtd-constructor &message))
;; (define make-irritants-condition (rtd-constructor &irritants))
;; (define make-syntax-violation (rtd-constructor &syntax))
;; (define make-assertion-violation (rtd-constructor &assertion))

;; (define condition
;;   (let ((constructor (rtd-constructor &compound)))
;;     (lambda conditions
;; 	(constructor (apply append (map simple-conditions conditions)))))) 

;; (define simple-conditions
;;   (let ((compound? (rtd-predicate &compound))
;; 	(conditions (rtd-accessor &compound 'conditions)))
;;     (lambda (condition)
;;       (cond
;; 	((compound? condition) (conditions condition))
;; 	(else (list condition))))))

;; (define (assertion-violation who message . irritants)
;;   (raise 
;;     (if who
;;     (condition (make-who-condition who) 
;; 	       (make-message-condition message) 
;; 	       (make-irritants-condition irritants)
;; 	       (make-assertion-violation))
;;     (condition (make-message-condition message) 
;; 	       (make-irritants-condition irritants)
;; 	       (make-assertion-violation)))))

(define (assertion-errors-display . args)
  (for-each (lambda (x)
              (if (procedure? x)
                  (x)
                  (display x (current-error-port))))
            args))

(define (assertion-violation who message . irritants)
  (raise
   (if who
       `(who message ,@irritants)
       `(message ,@irritants))))
