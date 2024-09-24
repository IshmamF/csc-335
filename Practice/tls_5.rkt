(define one? 
  (lambda (n)
    (= n 1)))

(define (sub1 x)
  (- x 1))

(define rempick 
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat)) ; if it's 1, we know we have to remove the current word
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat))))))) ; decrement n until it hits 1 aka the word we want to remove

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (rember* a l)
  (cond
    ((null? l) (quote()))
    ((and (atom? (car l)) (eq? (car l) a)) (rember* a (cdr l)))
    ((not (atom? (car l))) (cons (rember* a (car l)) (rember* a (cdr l))))
    (else (cons (car l) (rember* a (cdr l))))
    ))

(define (subst* new old lat)
  (cond
    ((null? lat) (quote()))
    ((atom? (car lat))
     (cond
       ((eq? (car lat) old) (subst* new old (cons new (cdr lat))))
       (else (cons (car lat) (subst* new old (cdr lat))))))
    (else (cons (subst* new old (car lat)) (subst* new old (cdr lat))))
    ))

(define (insertL new old lat)
  (cond
    ((null? lat) (quote()))
    ((atom? (car lat))
     (cond
       ((eq? (car lat) old)
        (cons new (cons old (insertL new old (cdr lat)))))
       (else (cons (car lat) (insertL new old (cdr lat))))))
    (else (cons (insertL new old (car lat)) (insertL new old (cdr lat))))
    ))

(define (leftmost lat)
  (cond
    ((atom? (car lat)) (car lat))
    (else (leftmost (car lat)))))