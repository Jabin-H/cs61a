(define (over-or-under num1 num2)
    (if (< num1 num2) -1
        (if (= num1 num2) 0 1)
    )
    ; (cond
    ;     ((< num1 num2) -1)
    ;     ((= num1 num2) 0)
    ;     ((> num1 num2) 1)
    ; )
)

(define (make-adder num)
    (begin (define (adder inc) (+ num inc)) adder)
    ; (lambda (inc) (+ num inc))
)

(define (composed f g)
    (begin (define (func x) (f (g x))) func)
    ; (lambda (x) (f (g x)))
)

(define (repeat f n)
    (begin (define (func x)
               (if (= n 0) x
                ((composed (repeat f (- n 1)) f) x)
               )
           )
        
        func
    )
    ; (lambda (x)
    ;     (if (= n 0) x
    ;         ((repeat f (- n 1)) (f x))
    ;     )
    ; )
)

(define (max a b)
  (if (> a b)
      a
      b))

(define (min a b)
  (if (> a b)
      b
      a))

(define (gcd a b)
    (cond
        ((zero? a) b)
        ((zero? b) a)
        ((zero? (modulo (max a b) (min a b))) (min a b))
        (else (gcd (min a b) (modulo (max a b) (min a b))))
    )
    ; (define larger (max a b))
    ; (define smaller (min a b))
    ; (define remainder (modulo larger smaller))
    ; (if (zero? remainder) smaller
    ;     (gcd smaller remainder)
    ; )
)
