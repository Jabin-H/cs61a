(define (curry-cook formals body)
    (cond
        ((null? formals) (print 'cannot 'curry))
        ((= (length formals) 1) (list 'lambda formals body))
        (else (list 'lambda
                    (cons (car formals) nil)
                    (curry-cook (cdr formals) body)))))

(define (curry-consume curry args)
    (if (null? args) curry
        (curry-consume (apply curry (cons (car args) nil)) (cdr args))))

(define-macro (switch expr options)
  (switch-to-cond (list 'switch expr options)))

(define (switch-to-cond switch-expr)
  (cons 'cond
        (map (lambda (option)
               (cons (list 'equal? (car (cdr switch-expr)) (car option)) (cdr option)))
             (car (cdr (cdr switch-expr))))))
