(define (ascending? s)
    (cond
        ((null? s) #t)
        ((null? (cdr s)) #t)
        ((and (<= (car s) (car (cdr s))) (ascending? (cdr s))) #t)
        (else #f)
    )
)

(define (my-filter pred s)
    (if (null? s) '()
        (begin
            (define first (car s))
            (define rest (my-filter pred (cdr s)))
            (if (pred first) (cons first rest) rest)
        )
    )
)

(define (interleave lst1 lst2)
    (cond
        ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else
            (begin
                (define rest (interleave (cdr lst1) (cdr lst2)))
                (cons (car lst1) (cons (car lst2) rest))
            )
        )
    )
)

(define (no-repeats s)
    (if (null? s) s
        (cons (car s) 
              (no-repeats
                  (my-filter (lambda (x) (not (= (car s) x))) (cdr s))))
    )
    ; (cond
    ;     ((null? s) ())
    ;     ((null? (cdr s)) s)
    ;     (else
    ;         (begin
    ;             (define first (car s))
    ;             (define rest (no-repeats (cdr s)))
    ;             (cons first (filter (lambda(x) (not (= x first))) rest))
    ;         )
    ;     )
    ; )
)
