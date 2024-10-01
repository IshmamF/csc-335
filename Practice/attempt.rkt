(define (get-pwr x)
  (cond
    ((eq? (quotient x 10) 0) 0)
    (else (+ 1 (get-pwr (quotient x 10))))
    ))

(define (cube x)
  (* (* x x) x))

(define (get-left-most x)
  (quotient x (expt 10 (get-pwr x))))

(define (remove-left-most x)
  (- x (* (get-left-most x) (expt 10 (get-pwr x))))) 
  

(define (mapnum n ans)
  (cond
    ((eq? n 0) ans)
    (else (mapnum (remove-left-most n) (+ (* (expt 10 (get-pwr (cube (get-left-most n)))) ans)(cube (get-left-most n)))))
    ))


(define (reverseList x)
  (cond ((null? x) '())
        (else (append
               (reverseList (cdr x))
               (list (car x))))))