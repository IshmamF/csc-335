(define my-and
  (lambda (x y)
    (if (eq? x #f)
        #f
        (if (eq? y #f)
            #f
            #t))))

(define my-or
  (lambda (x y)
    (if (eq? x #t)
        #t
        (if (eq? y #t)
            #t
            #f))))

(define my-not
  (lambda (x)
    (if (eq? x #t)
        #f
        #t)))

(define largest-squared
  (lambda (x y z)
    (cond
      ((my-and (> x y) (> y z)) (+ (* x x) (* y y)))
      ((my-and (> x z) (> z y)) (+ (* x x) (* z z)))
      ((my-and (> y x) (> x z)) (+ (* x x) (* y y)))
      ((my-and (> y z) (> z x)) (+ (* z z) (* y y)))
      ((my-and (> z y) (> y x)) (+ (* z z) (* y y)))
      (else (+ (* x x) (* z z))))))

(define temp-list (quote()))

(define sort-three
  (lambda (a b c max)
    (cond
      ((and (and (<= a b) (<= a c)) (<= b c))(list a b c max))
      ((and (and (<= a b) (<= a c)) (>= b c))(list a c b max ))
      ((and (and (<= b a) (<= b c)) (<= a c))(list b a c max))
      ((and (and (<= b a) (<= b c)) (>= a c))(list b c a max))
      ((and (and (<= c a) (<= c b)) (<= a b))(list c a b max))
      ((and (and (<= c a) (<= c b)) (>= a b))(list c b a max))
    )))

(define (largest a b c d e)
  (cond
    ((and (and(>= a b)(>= a c)) (and(>= a e)(>= a d)))a)
    ((and (and(>= b a)(>= b c)) (and(>= b e)(>= b d)))b)
    ((and (and(>= c a)(>= c b)) (and(>= c e)(>= c d)))c)
    ((and (and(>= d b)(>= d c)) (and(>= d e)(>= d a)))d)
    ((and (and(>= e b)(>= e c)) (and(>= e a)(>= e d)))e)
  ))

(define (smallest a b c d e)
  (cond
    ((and (and(<= a b)(<= a c)) (and(<= a e)(<= a d)))a)
    ((and (and(<= b a)(<= b c)) (and(<= b e)(<= b d)))b)
    ((and (and(<= c a)(<= c b)) (and(<= c e)(<= c d)))c)
    ((and (and(<= d b)(<= d c)) (and(<= d e)(<= d a)))d)
    ((and (and(<= e b)(<= e c)) (and(<= e a)(<= e d)))e)
  ))

(define (sort max min a b c d e)
  (cond
    ((and (or (eq? a max) (eq? a min))(or (eq? b max) (eq? b min)))
     (cons min (sort-three c d e max)))
    ((and (or (eq? a max) (eq? a min))(or (eq? c max) (eq? c min)))
     (cons min (sort-three b d e max)))
    ((and (or (eq? a max) (eq? a min))(or (eq? d max) (eq? d min)))
     (cons min (sort-three c b e max)))
    ((and (or (eq? a max) (eq? a min))(or (eq? e max) (eq? e min)))
     (cons min (sort-three c d b max)))
    ((and (or (eq? c max) (eq? c min))(or (eq? b max) (eq? b min)))
     (cons min (sort-three a d e max)))
    ((and (or (eq? d max) (eq? d min))(or (eq? b max) (eq? b min)))
     (cons min (sort-three c a e max)))
    ((and (or (eq? e max) (eq? e min))(or (eq? b max) (eq? b min)))
     (cons min (sort-three c d a max)))
    ((and (or (eq? c max) (eq? c min))(or (eq? d max) (eq? d min)))
     (cons min (sort-three a b e max)))
    ((and (or (eq? c max) (eq? c min))(or (eq? e max) (eq? e min)))
     (cons min (sort-three a d b max)))
    ((and (or (eq? d max) (eq? d min))(or (eq? e max) (eq? e min)))
     (cons min (sort-three c a b max)))
    ))

(define (sort-five a b c d e)
  (sort (largest a b c d e) (smallest a b c d e) a b c d e))

(define (sort-two a b)
  (if (>= a b)
      (list b a)
      (list a b)
      ))

(define (sort-three-v2 a b c)
  (if (<= (car(sort-two a b)) (car(sort-two b c)))
      (cons (car (sort-two a b)) (sort-two (car (cdr (sort-two a b))) c))
      (cons
       c
       (sort-two a b))))

(define (sort-four a b c d)
  (if (<= (car (sort-three-v2 a b c)) (car (sort-three-v2 b c d)))
      (cons
       (car (sort-three-v2 a b c))
       (sort-three-v2 (car (cdr (sort-three-v2 a b c))) (car (cdr (cdr (sort-three-v2 a b c)))) d))
      (cons
       d
       (sort-three-v2 a b c))))

(define (sort-five-v2 a b c d e)
  (if (<= (car (sort-four a b c d)) (car (sort-four b c d e)))
      (cons
       (car (sort-four a b c d))
       (sort-four (car (cdr (sort-four a b c d))) (car (cdr (cdr (sort-four a b c d)))) (car (cdr (cdr (cdr (sort-four a b c d))))) e))
      (cons
       e
       (sort-four a b c d))))
