#lang racket
(provide expr-compare)
;Taken from TA slides
(define LAMBDA (string->symbol "\u03BB") )

(define (bind-variable x y)
    (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
)

(define (replace-var old new expr)
  (define (replace e)
    (cond ((and (list? e) (not (single-lambda? e)) (not (equal? (car e) 'quote)) ) (map replace e))
          ((eq? e old) new)
          (#t e)))
  (replace expr)
)

; Changes the names of the lambda functions as needed
(define (process-lambda-name x y)
    (if (or (and (equal? x 'lambda) (equal? y LAMBDA)) 
            (and (equal? x LAMBDA) (equal? y 'lambda)))
    (list LAMBDA)
    (list x)
    )
)
 
(define (process-lambda-args x y xtl ytl acc)
    (cond [(and (empty? x) (empty? y))
          (list (reverse acc) (process-lambda-function xtl ytl))]
          [(equal? (car x) (car y))
          (process-lambda-args (cdr x) (cdr y) xtl ytl (cons (car x) acc))]
          [else
          (process-lambda-args (cdr x) (cdr y) (replace-var (car x) (bind-variable (car x) (car y)) xtl) (replace-var (car y) (bind-variable (car x) (car y)) ytl) (cons (bind-variable (car x) (car y)) acc))]
    )
)

(define (process-lambda-function x y)
    (expr-compare x y)
)


(define (single-lambda? x)
      (if (and (or (and (equal? (car x) 'lambda))
                   (and (equal? (car x) LAMBDA))
              )
              (list? (car (cdr x)))
          )
      #t
      #f
      )
)

; Checks if both functions are lambda
(define (lambda? x y) 
    (if (and (or (and (equal? (car x) 'lambda) (equal? (car y) LAMBDA))
              (and (equal? (car x) LAMBDA) (equal? (car y) 'lambda))
              (and (equal? (car x) LAMBDA) (equal? (car y) LAMBDA))
              (and (equal? (car x) 'lambda) (equal? (car y) 'lambda))
              )
              (list? (car (cdr x)))
              (list? (car (cdr y)))
              (equal? (length (car (cdr x))) (length (car (cdr y))))
        )
    #t
    #f
    )
)

(define (unequal-lambda? x y) 
    (if (and (or (and (equal? (car x) 'lambda) (equal? (car y) LAMBDA))
              (and (equal? (car x) LAMBDA) (equal? (car y) 'lambda))
              (and (equal? (car x) LAMBDA) (equal? (car y) LAMBDA))
              (and (equal? (car x) 'lambda) (equal? (car y) 'lambda))
              )
              (list? (car (cdr x)))
              (list? (car (cdr y)))
              (not (equal? (length (car (cdr x))) (length (car (cdr y)))))
        )
    #t
    #f
    )
)

(define (process-lambda x y)
    (define lambda-tail (process-lambda-args (car (cdr x)) (car (cdr y)) (car (cdr (cdr x))) (car (cdr (cdr y))) '()))
    (list (car (process-lambda-name (car x) (car y))) (car lambda-tail) (car (cdr lambda-tail))
    )
)

;check if both of the elements are lists of the same length
(define (check-list-element x y)
  (if (and (list? x) (list? y) (equal? (length x) (length y)))
      #t
      #f
  )
)

; Create the list to be returned
(define (create-list x y)
  (define (create-list-helper a b acc)
      (if (or (empty? a)
              (empty? b))
        (reverse acc)
        (if (equal? (car a) 
                    (car b))
        (create-list-helper (cdr a) (cdr b) (cons (car a) acc))
        (if (check-list-element (car a) (car b))
            (create-list-helper (cdr a) (cdr b) (cons (expr-compare (car a) (car b)) acc))
        (if (and (boolean? (car a)) (boolean? (car b)))
            (create-list-helper (cdr a) (cdr b) (cons (if (car a) '% '(not %)) acc))
        (create-list-helper (cdr a) (cdr b) (cons (list 'if '% (car a) (car b)) acc)))))))
(create-list-helper x y '())
)

;Check the list 
(define (expr-compare x y)
  (cond [(equal? x y) x] ;x == y
        [(and (boolean? x) (boolean? y)) 
        (if x '% '(not %))] ; x and y are boolean
        ; if one of them is not list - which means that not function
        [(or (not (list? x))
             (not (list? y)))
        (list 'if '% x y)] ; x or y is not a list
        [(and (or (equal? (car x) 'if) (equal? (car y) 'if)) 
              (not (and (equal? (car x) 'if) (equal? (car y) 'if))))
        (list 'if '% x y)] ; x or y has an if in the beginning, but not both
        [(and (or (equal? (car x) 'cons) (equal? (car y) 'cons)) 
              (not (and (equal? (car x) 'cons) (equal? (car y) 'cons)))
              (not (or (equal? (car x) 'list) (equal? (car y) 'list))))
        (list 'if '% x y)]
        [(or (equal? (car x) 'quote) (equal? (car y) 'quote))
        (list 'if '% x y)] ; x or y has a quote to start
        [(lambda? x y)
        (process-lambda x y)]
        [(unequal-lambda? x y)
        (list 'if '% x y)]
        [(and (list? x)
                (list? y)
                (equal? (length x) (length y)))
        (create-list x y)] ; x and y are lists of equal lengths
        [(and (list? x)
                (list? y)
                (not (equal? (length x) (length y))))
        (list 'if '% x y)] ; x and y are lists of different lengths
  ))

;Question 2 ---------------
;Used from TA hint code
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

;-------------Question 3 -------------------
(define test-expr-x '(+ 5 #t (lambda (a c) (g c a)) c (b a c) (if x y z) '(1 2 3)  ))
(define test-expr-y '(+ 3 #f (λ (a d) (g d d)) t (if a c) (if x c z) '(1 2 4)))
;-------------------------------------------
;-----------------------


; (expr-compare '(lambda (a b) a) '(λ (a) a)) ; # 28 '(if % (lambda (a b) a) (λ (a) a))

; (expr-compare test-expr-x test-expr-y)
;  (expr-compare     '(λ (x) ((λ (x) x) x))
;                    '(λ (y) ((λ (x) y) x)))
;                    ;'(λ (x!y) ((λ (x) (if % x x!y)) (if % x!y x)))) ; # 34
;         (expr-compare '(lambda (x y) (+ (x y) '(y x)))
;                       '(lambda (y x) (+ (y x) '(x y))))
;                    ;'(lambda (x!y y!x) (+ (x!y y!x) (if % '(y x) '(x y))))) ; # 35
;         (expr-compare '(((λ (g)
;                         ((λ (x) (g (λ () (x x))))     ; This is the way we define a recursive function
;                          (λ (x) (g (λ () (x x))))))   ; when we don't have 'letrec'
;                       (λ (r)                               ; Here (r) will be the function itself
;                         (λ (n) (if (= n 0)
;                                    1
;                                    (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
;                      10)
;                    '(((λ (x)
;                         ((λ (n) (x (λ () (n n))))
;                          (λ (r) (x (λ () (r r))))))
;                       (λ (g)
;                         (λ (x) (if (= x 0)
;                                    1
;                                    (* x ((g) (- x 1)))))))
;                      9))
                   ;'(((λ (g!x)
                    ;    ((λ (x!n) (g!x (λ () (x!n x!n))))
                     ;    (λ (x!r) (g!x (λ () (x!r x!r))))))
                      ;(λ (r!g)
                       ; (λ (n!x) (if (= n!x 0)
                        ;             1
                         ;            (* n!x ((r!g) (- n!x 1)))))))
                     ;(if % 10 9))) ; # 36
;Test cases used to check validity of program
; (equal? (expr-compare 12 12) 12)
; (expr-compare 12 20)
; (expr-compare #t #t)
; (expr-compare #f #f)
; (expr-compare #t #f) 
; (test-expr-compare #f #t)
; (expr-compare 'a '(cons a b))
; (expr-compare '(cons a b) '(cons a b))
; (expr-compare '(cons a lambda) '(cons a λ))
; (test-expr-compare '(cons a lambda) '(cons a λ))
; (expr-compare '(cons (cons a b) (cons b c))
;               '(cons (cons a c) (cons a c)))
; (expr-compare '(cons a b) '(list a b))
; (expr-compare '(list) '(list a))
; (expr-compare ''(a b) ''(a c))
; (expr-compare '(quote (a b)) '(quote (a c)))
; (expr-compare '(quoth (a b)) '(quoth (a c)))
; (expr-compare '(if x y z) '(if x z z))
; (expr-compare '(if x y z) '(g x y z))
; (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
; (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
; (expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
; (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
; (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
;               '(+ #t ((lambda (a c) (f a c)) 1 2)))
; (expr-compare '((λ (a b) (f a b)) 1 2)
;               '((λ (a b) (f b a)) 1 2))
; (expr-compare '((λ (a b) (f a b)) 1 2)
;               '((λ (a c) (f c a)) 1 2))
; (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
;               '((lambda (if) (+ if if (f λ))) 3))
; (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
;                                     a (lambda (a) a))))
;                 (lambda (b a) (b a)))
;               '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
;                                 a (λ (b) a))))
;                 (lambda (a b) (a b))))

; (equal? (expr-compare 12 12)   12)
; (equal? (expr-compare 12 20)  '(if % 12 20))
; (equal? (expr-compare #t #t)     #t)
; (equal? (expr-compare #f #f)     #f)
; (equal? (expr-compare #t #f)     '%)
; (equal? (expr-compare #f #t)     '(not %))
; (equal? (expr-compare 'a '(cons a b))     '(if % a (cons a b)))
; (equal? (expr-compare '(cons a b) '(cons a b))     '(cons a b))
; (equal? (expr-compare '(cons a lambda) '(cons a λ))     '(cons a (if % lambda λ)))
; (equal? (expr-compare '(cons (cons a b) (cons b c))
;               '(cons (cons a c) (cons a c)))
;     '(cons (cons a (if % b c)) (cons (if % b a) c)))
; (equal? (expr-compare '(cons a b) '(list a b))     '((if % cons list) a b))
; (equal? (expr-compare '(list) '(list a))     '(if % (list) (list a)))
; (equal? (expr-compare ''(a b) ''(a c))     '(if % '(a b) '(a c)))
; (equal? (expr-compare '(quote (a b)) '(quote (a c)))     '(if % '(a b) '(a c)))
; (equal? (expr-compare '(quoth (a b)) '(quoth (a c)))     '(quoth (a (if % b c))))
; (equal? (expr-compare '(if x y z) '(if x z z))     '(if x (if % y z) z))
; (equal? (expr-compare '(if x y z) '(g x y z))
;     '(if % (if x y z) (g x y z)))
; (equal? (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
;     '((lambda (a) ((if % f g) a)) (if % 1 2)))
; (equal? (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
;     '((λ (a) ((if % f g) a)) (if % 1 2)))
; (equal? (expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
;     '((lambda (a!b) a!b) (if % c d)))
; (equal? (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
;     '(if % '((λ (a) a) c) '((lambda (b) b) d)))
; (equal? (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
;               '(+ #t ((lambda (a c) (f a c)) 1 2)))
;     '(+
;      (not %)
;      ((λ (a b!c) (f a b!c)) 1 2)))
; (equal? (expr-compare '((λ (a b) (f a b)) 1 2)
;               '((λ (a b) (f b a)) 1 2))
;     '((λ (a b) (f (if % a b) (if % b a))) 1 2))
; (equal? (expr-compare '((λ (a b) (f a b)) 1 2)
;               '((λ (a c) (f c a)) 1 2))
;     '((λ (a b!c) (f (if % a b!c) (if % b!c a)))
;      1 2))
; (equal? (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
;               '((lambda (if) (+ if if (f λ))) 3))
;     '((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ)))) 3))
; (equal? (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
;                                     a (lambda (a) a))))
;                 (lambda (b a) (b a)))
;               '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
;                                 a (λ (b) a))))
;                 (lambda (a b) (a b))))
;     '((λ (a)
;       ((if % eq? eqv?)
;        a
;        ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))
;         a (λ (a!b) (if % a!b a)))))
;      (lambda (b!a a!b) (b!a a!b))))