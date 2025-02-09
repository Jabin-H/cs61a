(define (sum_iter n total)
   (if (zero? n)
       total
       (sum_iter (- n 1) (+ n total))))

(define (sum n) (sum_iter n 0))

(define (factorial n)
    (define (factorial_iter n k)
        (if (zero? n)
            k
            (factorial_iter (- n 1) (* k n))))
    (factorial_iter n 1))
