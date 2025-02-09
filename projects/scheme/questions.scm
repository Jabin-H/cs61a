(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (enumerate_helper s k)
      (if (null? s) s 
          (cons (list k (car s)) (enumerate_helper (cdr s) (+ k 1))))
      )
  (enumerate_helper s 0)
  )
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists S1 and S2 according to ORDERED? and return
;; the merged lists.
(define (merge ordered? s1 s2)
  ; BEGIN PROBLEM 16
  (cond
      ((null? s1) s2)
      ((null? s2) s1)
      (else
          (if (ordered? (car s1) (car s2)) (cons (car s1) (merge ordered? (cdr s1) s2))
          (cons (car s2) (merge ordered? s1 (cdr s2))))
          )
      )
  )
  ; END PROBLEM 16

;; Optional Problem 2

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN OPTIONAL PROBLEM 2
         expr
         ; END OPTIONAL PROBLEM 2
         )
        ((quoted? expr)
         ; BEGIN OPTIONAL PROBLEM 2
         expr
         ; END OPTIONAL PROBLEM 2
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN OPTIONAL PROBLEM 2
           (cons form (cons params (map let-to-lambda body)))
           ; END OPTIONAL PROBLEM 2
           ))
        ((let? expr)
         (let ((values (cadr expr))
              (body   (cddr expr)))
          ; BEGIN OPTIONAL PROBLEM 2
          (let ((formals (car (zip values)))
                 (args (cadr (zip values))))
            (cons (cons 'lambda (cons formals (map let-to-lambda body))) (map let-to-lambda args))
             )
          ; END OPTIONAL PROBLEM 2
          ))
        (else
         ; BEGIN OPTIONAL PROBLEM 2
         (map let-to-lambda expr)
         ; END OPTIONAL PROBLEM 2
         )))

; Some utility functions that you may find useful to implement for let-to-lambda

(define (zip pairs)
  (if (null? pairs)
      (list nil nil)
      (let ((zipped (zip (cdr pairs))))
          (list (cons (caar pairs) (car zipped))
                 (append (cdar pairs) (cadr zipped)))))
             )

; for test
(define let-to-lambda-code
    '(define (let-to-lambda expr)
         (cond ((atom? expr)
         expr
         )
        ((quoted? expr)
         expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           (cons form (cons params (map let-to-lambda body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
              (body   (cddr expr)))
          (let ((formals (car (zip values)))
                 (args (cadr (zip values))))
            (cons (cons 'lambda (cons formals (map let-to-lambda body))) (map let-to-lambda args))
             )
          ))
        (else
         (map let-to-lambda expr)
         ))))
     
; (define let-to-lambda-without-let
;  (let-to-lambda let-to-lambda-code))


; (define (let-to-lambda-without-let expr)
;   (cond
;     ((atom? expr) expr)
;     ((quoted? expr) expr)
;     ((or (lambda? expr) (define? expr))
;      ((lambda (form params body)
;         (cons form (cons params (map let-to-lambda body))))
;       (car expr) (cadr expr) (cddr expr)))
;     ((let? expr)
;      ((lambda (values body)
;         ((lambda (formals args)
;           (cons (cons (quote lambda)
;                       (cons formals (map let-to-lambda body)))
;                  (map let-to-lambda args)))
;          (car (zip values)) (cadr (zip values))))
;       (cadr expr) (cddr expr)))
;     (else
;      (map let-to-lambda expr))))

;  (define let-to-lambda let-to-lambda-without-let)
