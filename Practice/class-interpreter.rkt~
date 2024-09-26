; Constructors
(define make-@
  (lambda (e1 e2)
    (list e1 '@ e2)
    ))

(define make-!
  (lambda (e1 e2)
    (list e1 '! e2)
    ))

(define make-#
  (lambda (e1 e2)
    (list e1 '$ e2)
    ))

; Selectors

(define first-operand
  (lambda (e)
    (car e)
    ))

(define second-operand
  (lambda (e)
    (caddr e)
    ))

(define operator
  (lambda (e)
    (cadr e)
    ))

; Classifiers

(define @-exp?
  (lambda (e)
    (eq? (operator e) '@)
    ))

(define !-exp?
  (lambda (e)
    (eq? (operator e) '!)
    ))

(define $-exp?
  (lambda (e)
    (eq? (operator e) '$)
    ))

(define @ +)
(define $ *)
(define ! expt)

(define natnum?
  (lambda (x)
    (and (integer? x)
         (>= x 0))))

(define value
  (lambda (aexp)

    (define val
      (lambda (aexp)(value aexp @ $ !)))

    (cond ((natnum? aexp) aexp)
          (else (cond
                  ((@-exp? aexp)
                   (@ (val (first operand aexp)) (val (second-operand aexp))))
                   ((!-exp? aexp)
                   (! (val (first operand aexp)) (val (second-operand aexp))))
                   (($-exp? aexp)
                   ($ (val (first operand aexp)) (val (second-operand aexp))))
                   )))))


(define improved-value
  (lambda (aexp @ $ !)
    (define val
      (lambda (aexp) (value aexp @ $ !)))

    (define atom-to-function
      (lambda (x)
        (cond
          ((eq? x '@) @)
          ((eq? x '$) $)
          ((eq? x '!) !)
          )))

    (cond ((natnum? aexp) aexp)
          (else ((atom-to-function (operator aexp))
                 (val (first-operand aexp))
                 (val (second-operand aexp))
                 )))))