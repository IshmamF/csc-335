(define atom? 
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
   (cond
     ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define (rember a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat))))))

(define (firsts lat)
  (cond
    ((null? lat) (quote()))
    (else (cons (car (car lat))
                (firsts (cdr lat))))))

(define (insertR new old lat)
  (cond
    ((null? lat)(quote()))
    ((eq? (car lat) old) (cons old (cons new (cdr lat))))
    (else (cons (car lat)(insertR new old (cdr lat))))))

(define (insertL new old lat)
  (cond
    ((null? lat)(quote()))
    ((eq? (car lat) old) (cons new lat))
    (else (cons (car lat)(insertL new old (cdr lat))))))

(define (subst new old lat)
  (cond
    ((null? lat)(quote()))
    ((eq? (car lat) old) (cons new (cdr lat)))
    (else (cons (car lat)(subst new old (cdr lat))))))

(define (subst2 new o1 o2 lat)
  (cond
    ((null? lat)(quote()))
    ((or (eq? (car lat) o1) (eq? (car lat) o2))(cons new (cdr lat)))
    (else (cons (car lat)(subst2 new old (cdr lat))))))

(define (multirember a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat))))))