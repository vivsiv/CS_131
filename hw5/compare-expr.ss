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
			;compare heads as expressions 
			[else (cons (compare-expr (car l1) (car l2)) (compare-lists (cdr l1) (cdr l2)))]
		)
	)
)

(define compare-built-ins
	(lambda (e1 e2 proc)
		(let ([vars1 (car (cdr e1))] [vars2 (car (cdr e2))])
			(cond 
				;compare as lists if variables are same
				[(proc vars1 vars2) (compare-lists e1 e2)]
				;otherwise compare as literals
				[else (compare-literals e1 e2)]
			)
		)
	)
)

(define contains-built-in
	(lambda (e1 e2)
		(cond
			[(equal? (car e1) 'quote) #t]
			[(equal? (car e2) 'quote) #t]
			[(equal? (car e1) 'if) #t]
			[(equal? (car e2) 'if) #t]
			[(equal? (car e1) 'let) #t]
			[(equal? (car e2) 'let) #t]
			[(equal? (car e1) 'lambda) #t]
			[(equal? (car e2) 'lambda) #t]
			[else #f]
		)
	)
)

(define compare-expr
	(lambda (e1 e2)
		;if expressions are both lists of the same length try to evaluate as lists
		(if (and 
				(and (list? e1) (list? e2)) 
				(equal? (length e1) (length e2))
			)
			(cond
				;equal first elements try as built-in or plain list
				[(equal? (car e1) (car e2))
					;handle built-ins
					(case (car e1)
						;handling quotes
						['quote (compare-literals e1 e2)]
						;handling conditionals
						['if (compare-lists e1 e2)]
						;handling let's
						['let (compare-built-ins e1 e2 (lambda (x y) (equal? (map car x) (map car y))))]
						;handling lambda's
						['lambda (compare-built-ins e1 e2 (lambda (x y) (equal? x y)))]
						;any other list
						[else (compare-lists e1 e2)]
					)
				]
				;unequal first elements, try as lists unless one side is a built-in
				[(contains-built-in e1 e2) (compare-literals e1 e2)]
				[else (compare-lists e1 e2)]
			)
			;if not lists of the same length just evaluate as literals
			(compare-literals e1 e2)
		)
	)
)

(define test-compare-expr 
	(lambda (x y)
	 	(let ([tcp-true (list 'let '((TCP #t)) (compare-expr x y))]
	 		  [tcp-false (list 'let '((TCP #f)) (compare-expr x y))]
	 		)
		    (cond 
		    	[(and (equal? (eval tcp-true) (eval x)) (equal? (eval tcp-false) (eval y))) #t]
		    	[else #f]
		    )
		)
	)
)


; (display "1. ") (display (equal? (compare-expr 12 12) 12)) (newline)
; (display "2. ") (display (equal? (compare-expr 12 20) '(if TCP 12 20))) (newline)
; (display "3. ") (display (equal? (compare-expr #t #t) #t)) (newline)
; (display "4. ") (display (equal? (compare-expr #f #f) #f)) (newline)
; (display "5. ") (display (equal? (compare-expr #t #f) 'TCP)) (newline)
; (display "6. ") (display (equal? (compare-expr #f #t) '(not TCP))) (newline)

; (display "7. ") (display (equal? (compare-expr '(12 12) '(12 12)) '(12 12))) (newline)
; (display "8. ") (display (equal? (compare-expr '(12 12) '(12 20)) '(12 (if TCP 12 20)))) (newline)
; (display "9. ") (display (equal? (compare-expr '(12 (12 24)) '(12 (12 20))) '(12 (12 (if TCP 24 20))))) (newline)
; (display "10. ") (display (equal? (compare-expr '(12 (12 24) 48) '(12 (12 20) 40)) '(12 (12 (if TCP 24 20)) (if TCP 48 40)))) (newline)

; (display "11. ") (display (equal? (compare-expr 'a '(cons a b)) '(if TCP a (cons a b)))) (newline)
; (display "12. ") (display (equal? (compare-expr '(cons a b) '(cons a b)) '(cons a b))) (newline)
; (display "13. ") (display (equal? (compare-expr '(cons a b) '(cons a c)) '(cons a (if TCP b c)))) (newline)
; (display "14. ") (display (equal? (compare-expr '(cons (cons a b) (cons b c)) '(cons (cons a c) (cons a c))) '(cons (cons a (if TCP b c)) (cons (if TCP b a) c)))) (newline)
; (display "15. ") (display (equal? (compare-expr '(cons a b) '(list a b)) '((if TCP cons list) a b))) (newline)
; (display "16. ") (display (equal? (compare-expr '(list) '(list a)) '(if TCP (list) (list a)))) (newline)
; (display "17. ") (display (equal? (compare-expr '(quoth (a b)) '(quoth (a c))) '(quoth (a (if TCP b c))))) (newline)

; (display "18. ") (display (equal? (compare-expr ''(a b) ''(a c)) '(if TCP '(a b) '(a c)))) (newline)
; (display "19. ") (display (equal? (compare-expr '(quote (a b)) '(quote (a c))) '(if TCP '(a b) '(a c)))) (newline)
; (display "20. ") (display (equal? (compare-expr '(quote (a b)) '(f (a c))) '(if TCP '(a b) (f (a c))))) (newline)

; (display "21. ") (display (equal? (compare-expr '(if x y z) '(if x z z)) '(if x (if TCP y z) z))) (newline)
; (display "22. ") (display (equal? (compare-expr '(if x y z) '(g x y z)) '(if TCP (if x y z) (g x y z)))) (newline)

; (display "23. ") (display (equal? (compare-expr '(let ((a 1)) (f a)) '(let ((a 2)) (g a))) '(let ((a (if TCP 1 2))) ((if TCP f g) a)))) (newline)
; (display "24. ") (display (equal? (compare-expr '(+ #f (let ((a 1) (b 2)) (f a b))) '(+ #t (let ((a 1) (c 2)) (f a c))))
; 	'(+ (not TCP) (if TCP (let ((a 1) (b 2)) (f a b)) (let ((a 1) (c 2)) (f a c)))))) (newline)


; (display "25. ") (display (equal? (compare-expr '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)) '((lambda (a) ((if TCP f g) a)) (if TCP 1 2)))) (newline)
; (display "26. ") (display (equal? (compare-expr '((lambda (a b) (f a b)) 1 2) '((lambda (a b) (f b a)) 1 2))
; 	'((lambda (a b) (f (if TCP a b) (if TCP b a))) 1 2))) (newline)
; (display "27. ") (display (equal? (compare-expr '((lambda (a b) (f a b)) 1 2) '((lambda (a c) (f c a)) 1 2))
;  	'((if TCP (lambda (a b) (f a b)) (lambda (a c) (f c a))) 1 2))) (newline)
