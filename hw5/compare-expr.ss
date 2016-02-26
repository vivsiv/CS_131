(define compare-literals
	(lambda (lit1 lit2) 
		(if (equal? lit1 lit2)
			lit1
			(cond
				[(and (equal? lit1 #t) (equal? lit2 #f)) 'TCP]
				[(and (equal? lit1 #f) (equal? lit2 #t)) '(not TCP)]
				[else (cons 'if (cons 'TCP (cons lit1 (cons lit2 '()))))]
			)
		)
	)
)

(define compare-lists
	(lambda (l1 l2)
		(cond
			[(or (null? l1) (null? l2)) '()]
			[else (cons (compare-expr (car l1) (car l2)) (compare-lists (cdr l1) (cdr l2)))]
		)
	)
)

(define compare-bindings
	(lambda (b1 b2)
		(cond
			[(and (equal? b1 '()) (equal? b2 '())) #t]
			[(equal? (car (car b1)) (car (car b2))) (compare-bindings (cdr b1) (cdr b2))]
			[else #f]
		)
	)

)

(define compare-lets 
	(lambda (l1 l2)
		(let (
				[bind1 (car (cdr l1))]
				[bind2 (car (cdr l2))]
			)
			(if (compare-bindings bind1 bind2)
				(compare-lists l1 l2)
				(compare-literals l1 l2)
			)
		)
	)
)

(define compare-lambdas
	(lambda (l1 l2)
		(let (
				[form1 (car (cdr (car l1)))]
				[form2 (car (cdr (car l2)))]
				[body1 (car (cdr (cdr (car l1))))]
				[body2 (car (cdr (cdr (car l2))))]
				[res1 (cdr l1)]
				[res2 (cdr l2)]
			)
			(if (equal? form1 form2)
				(append
					(cons (list 'lambda form1 (compare-lists body1 body2)) '())
					(compare-lists res1 res2)
				)
				(append (cons (compare-literals (car l1) (car l2)) '())
						(compare-lists res1 res2)
				)
			)
		)
	)
)

(define compare-expr
	(lambda (e1 e2)
		(if (and (list? e1) (list? e2))
			(cond 
				[(equal? (length e1) (length e2))
					;equal first elements 
					(if (equal? (car e1) (car e2))
						(cond
							;handling quotes
							[(equal? (car e1) 'quote) (compare-literals e1 e2)]
							;handling conditionals
							[(equal? (car e1) 'if) (compare-lists e1 e2)]
							;handling let's
							[(equal? (car e1) 'let) (compare-lets e1 e2)]
							;handling lambda's
							; [(equal? (car e1) 'lambda) (compare-lambdas e1 e2)]
							;any other list
							[else (compare-lists e1 e2)]
						)
						;equal first elements of first lists
						; (if (equal? (car (car e1)) (car (car e2)))
						; 	;handling lambda's
						; 	((equal? (car (car e1)) 'lambda) (compare-lambdas e1 e2))
						; 	;unequal elements
						; 	(compare-literals e1 e2)

						; )
						;unequal first elements
						(compare-literals e1 e2)
					)
				]
				;unequal length lists
				[else (compare-literals e1 e2)] 
			)
			;not lists
			(compare-literals e1 e2)
		)
	)
)

(equal? (compare-expr 12 12) 12)
(equal? (compare-expr 12 20) '(if TCP 12 20))
(equal? (compare-expr #t #t) #t)
(equal? (compare-expr #f #f) #f)
(equal? (compare-expr #t #f) 'TCP)
(equal? (compare-expr #f #t) '(not TCP))

(equal? (compare-expr '(12 12) '(12 12)) '(12 12))
(equal? (compare-expr '(12 12) '(12 20)) '(12 (if TCP 12 20)))
(equal? (compare-expr '(12 (12 24)) '(12 (12 20))) '(12 (12 (if TCP 24 20))))
(equal? (compare-expr '(12 (12 24) 48) '(12 (12 20) 40)) '(12 (12 (if TCP 24 48)) (if TCP 48 40)))

(equal? (compare-expr 'a '(cons a b)) '(if TCP a (cons a b)))
(equal? (compare-expr '(cons a b) '(cons a b)) '(cons a b))
(equal? (compare-expr '(cons a b) '(cons a c)) '(cons a (if TCP b c)))
(equal? (compare-expr '(cons (cons a b) (cons b c)) '(cons (cons a c) (cons a c))) '(cons (cons a (if TCP b c)) (cons (if TCP b a) c)))
(equal? (compare-expr '(cons a b) '(list a b)) '((if TCP cons list) a b))
(equal? (compare-expr '(list) '(list a)) '(if TCP (list) (list a)))
(equal? (compare-expr '(quoth (a b)) '(quoth (a c))) '(quoth (a (if TCP b c))))

(equal? (compare-expr ''(a b) ''(a c)) '(if TCP '(a b) '(a c)))
(equal? (compare-expr '(quote (a b)) '(quote (a c))) '(if TCP '(a b) '(a c)))
(equal? (compare-expr '(quote (a b)) '(f (a c))) '(if TCP '(a b) (f (a c))))

(equal? (compare-expr '(if x y z) '(if x z z)) '(if x (if TCP y z) z))
(equal? (compare-expr '(if x y z) '(g x y z)) '(if TCP (if x y z) (g x y z)))

(equal? (compare-expr '(let ((a 1)) (f a)) '(let ((a 2)) (g a))) '(let ((a (if TCP 1 2))) ((if TCP f g) a)))

(compare-expr '(+ #f (let ((a 1) (b 2)) (f a b))) '(+ #t (let ((a 1) (c 2)) (f a c)))) 
(equal? (compare-expr '(let ((a 1) (b 2)) (f a b)) '(let ((a 1) (c 2)) (f a c))) 
	'(if TCP (let ((a 1) (b 2)) (f a b)) (let ((a 1) (c 2)) (f a c))))
	; '(+ (not TCP) (if TCP (let ((a 1) (b 2)) (f a b)) (let ((a 1) (c 2)) (f a c))))

(equal? (compare-lambdas '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)) '((lambda (a) ((if TCP f g) a)) (if TCP 1 2)))
(equal? (compare-lambdas '((lambda (a b) (f a b)) 1 2) '((lambda (a b) (f b a)) 1 2))
	'((lambda (a b) (f (if TCP a b) (if TCP b a))) 1 2))
(equal? (compare-lambdas '((lambda (a b) (f a b)) 1 2) '((lambda (a c) (f c a)) 1 2))
 	'((if TCP (lambda (a b) (f a b)) (lambda (a c) (f c a))) 1 2))

; (equal? (compare-expr '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)) '((lambda (a) ((if TCP f g) a)) (if TCP 1 2)))
; (equal? (compare-expr '((lambda (a b) (f a b)) 1 2) '((lambda (a b) (f b a)) 1 2))
; 	'((lambda (a b) (f (if TCP a b) (if TCP b a))) 1 2))
; (equal? (compare-expr '((lambda (a b) (f a b)) 1 2) '((lambda (a c) (f c a)) 1 2))
;  	'((if TCP (lambda (a b) (f a b)) (lambda (a c) (f c a))) 1 2))


; (car (cdr (car '((lambda (a b) (f a b)) 1 2))))
; (car (cdr (cdr (car '((lambda (a b) (f a b)) 1 2)))))
; (cdr '((lambda (a b) (f a b)) 1 2))
; (car '((lambda (a b) (f a b)) 1 2))

; (car (cdr (car '((lambda (a b) (f b a)) 1 2))))

