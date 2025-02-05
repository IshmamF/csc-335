; creates a function that does the same logic as AND operator, however must have pre condition that it's a boolean value

(define my-and
  (lambda (x y)
    (if (eq? x #f)
        #f
        (if (eq? y #f)
            #f
            #t))))

; creates a function that does the same logic as OR operator, however must have pre condition that it's a boolean value
(define my-or
  (lambda (x y)
    (if (eq? x #t)
        #t
        (if (eq? y #t)
            #t
            #f))))

; creates a function that does the same logic as NOT operator, however must have pre condition that it's a boolean value
(define my-not
  (lambda (x)
    (if (eq? x #t)
        #f
        #t)))

; creates a function that takes the two largest numbers out of three numbers, and sums the square of each. 
(define largest-squared
  (lambda (x y z)
    (cond
      ((my-and (> x y) (> y z)) (+ (* x x) (* y y)))
      ((my-and (> x z) (> z y)) (+ (* x x) (* z z)))
      ((my-and (> y x) (> x z)) (+ (* x x) (* y y)))
      ((my-and (> y z) (> z x)) (+ (* z z) (* y y)))
      ((my-and (> z y) (> y x)) (+ (* z z) (* y y)))
      (else (+ (* x x) (* z z))))))

; First attempt at sorting 5 numbers
; I noticed it's relatively easy to get the smallest and the largest number, and sorting 3 numbers has at most 6 possibilities.
; If I find the smallest, and the largest, I would just have to sort the middle 3 values.

; Sort three goes through all possibilities of the 3 numbers and returns a list of the 3 values sorted
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

; returns the largest number out of 5 
(define (largest a b c d e)
  (cond
    ((and (and(>= a b)(>= a c)) (and(>= a e)(>= a d)))a)
    ((and (and(>= b a)(>= b c)) (and(>= b e)(>= b d)))b)
    ((and (and(>= c a)(>= c b)) (and(>= c e)(>= c d)))c)
    ((and (and(>= d b)(>= d c)) (and(>= d e)(>= d a)))d)
    ((and (and(>= e b)(>= e c)) (and(>= e a)(>= e d)))e)
  ))

; returns the smallest number out of 5
(define (smallest a b c d e)
  (cond
    ((and (and(<= a b)(<= a c)) (and(<= a e)(<= a d)))a)
    ((and (and(<= b a)(<= b c)) (and(<= b e)(<= b d)))b)
    ((and (and(<= c a)(<= c b)) (and(<= c e)(<= c d)))c)
    ((and (and(<= d b)(<= d c)) (and(<= d e)(<= d a)))d)
    ((and (and(<= e b)(<= e c)) (and(<= e a)(<= e d)))e)
  ))

; we don't know which of the 5 is the max or minimum, so we go through each possibility for which two numbers can be those values
; we would then return a list that has min , three numbers sorted, and max
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

; finally, this function puts all previous functions together
(define (sort-five a b c d e)
  (sort (largest a b c d e) (smallest a b c d e) a b c d e))

; Second attempt at sorting 5 numbers
; We can break down sort 5 into it's smallest possible case, which is sort 2 and build up.

; Here we're comparing two numbers and returning a list where they're sorted
(define (sort-two a b)
  (if (>= a b)
      (list b a)
      (list a b)
      ))

; We then use sort two for our sort three function
; We first check which half has the smallest value, since it's possible that the values on the right
; could have a smaller value than the values on the left
; If the left side has the smaller value , then we take the 'car' of the sorted list on the left
; to get the smallest value, then we sort the bigger value with the last value, giving us a list of the 3 numbers sorted
; If the right side has the smaller value, then we just append the last value with a sorted left side.
; Since there's two halfs, if we know that the right side has the smaller value and that the last value is the value
; thats not on the first half, then we know that the last value would be the smallest value. 
(define (sort-three-v2 a b c)
  (if (<= (car(sort-two a b)) (car(sort-two b c)))
      (cons (car (sort-two a b)) (sort-two (car (cdr (sort-two a b))) c))
      (cons
       c
       (sort-two a b))))

; Sort 4 follows the same principles as Sort 3
(define (sort-four a b c d)
  (if (<= (car (sort-three-v2 a b c)) (car (sort-three-v2 b c d)))
      (cons
       (car (sort-three-v2 a b c))
       (sort-three-v2 (car (cdr (sort-three-v2 a b c))) (car (cdr (cdr (sort-three-v2 a b c)))) d))
      (cons
       d
       (sort-three-v2 a b c))))

; Finally Sort 5 follows the same principles as Sort 4
(define (sort-five-v2 a b c d e)
  (if (<= (car (sort-four a b c d)) (car (sort-four b c d e)))
      (cons
       (car (sort-four a b c d))
       (sort-four (car (cdr (sort-four a b c d))) (car (cdr (cdr (sort-four a b c d)))) (car (cdr (cdr (cdr (sort-four a b c d))))) e))
      (cons
       e
       (sort-four a b c d))))
