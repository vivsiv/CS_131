; Scheme examples
"Hi Mom!"
42
22/7
3.141592
+
(+ 76 31)
(* -12 10)
' (a b c d)

(car '(a b c))
(cdr '(a b c))
(cons 'a '(b c))
(cons (car '(a b c)) (cdr '(d e f)))

(define square
	(lambda (n)
		(* n n)
	)
)
(square 5)
(square -200)
(square 0.5)
(square -1/2)

(define reciprocal
	(lambda (n)
		(if (= n 0)
			"oops!"
			(/ 1 n)
		)
	)
)
(reciprocal 10)
(reciprocal 1/10)
(reciprocal 0)
(reciprocal (reciprocal 1/10))

(quote (1 2 3 4))

(let ((x 2)) (+ x 3))
(let ((x 2) (y 1)) (+ x y))

(let ([x 1])
	(let ([new-x (+ x 1)])
		(+ new-x new-x)
	)
)

((lambda (x) (+ x x)) 4)
((lambda (x) (+ x x)) (* 3 4))

(let ([double (lambda (x) (+ x x))])
	(list (double (* 3 4))
		  (double (/ 99 11))
		  (double (- 2 7))
	)
)

(let ([double-any (lambda (f x) (f x x))])
	(list (double-any + 13)
		  (double-any cons 'a)
	)
)

; x is a 'free variable' y is not since it is bound to the lambda
(let ([x 2])
	(let ([f (lambda (y) (+ x y))])
		(f 4)
	)
)

; x in lambda shadows x in outer let
(let ([x 2])
	(let ([f (lambda (x) (+ x x))])
		(f 4)
	)
)

;x is bound to the list (1 2 3 4)
(let ([f (lambda x x)])
  (f 1 2 3 4))

(let ([f (lambda x x)])
  (f))

(let ([g (lambda (x . y) (list x y))])
  (g 1 2 3 4))

(let ([h (lambda (x y . z) (list x y z))])
  (h 'a 'b 'c 'd))

(define cadr
	(lambda (x) (car (cdr x)))
)
(cadr '(1 2 3 4))

(define cddr
	(lambda (x) (cdr (cdr x)))
)
(cddr '(1 2 3 4))

(define abs
	(lambda (n) 
		(if (< 0 n) n (- 0 n))
	)
)

(define sign
	(lambda (n)
		(cond 
			[(< n 0) '-]
			[(= n 0) '0]
			[(> n 0) '+]
		)
	)
)
(sign -1)
(sign 0)
(sign +1)

(define length
	(lambda (ls)
		(if (null? ls)
			0
			(+ 1 (length (cdr ls)))
		)
	)
)
(length '())
(length '(a))
(length '(a b))

(define map
	(lambda (f ls)
		(if (null? ls)
			'()
			(cons (f (car ls)) (map f (cdr ls)))
		)
	)
)
(map (lambda (x) (* x x)) '(1 2 3 4))

(+ (* 1.2 (- 2 (/ 1 3))) (- 0 8.7))

(let ([ts (* 3 a)])
	(+ (- ts b) (+ ts b))
)

(let ([l (list 'a 'b 'c)])
	(cons (car l) (cdr l))
)

	
(let ([x 'a] [y 'b])
  (list (let ([c 'c]) (cons c y))
        (let ([d 'd]) (cons x d))))

(let ([x 5])
	(lambda (y x) (+ y x))
	3
)

(define compose
	(lambda (p1 p2)
		(lambda (x)
			(p1 (p2 x))
		)
	)
)

(define cadr
	(lambda (l)
		((compose car cdr) l)
	)
)

(define contains
	(lambda (l e)
		(cond
			[(null? l) #f]
			[(equal? (car l) e) #t]
			[else (contains (cdr l) e)]
		)
	)
)

(define subset
	(lambda (s1 s2)
		(cond
			[(null? s1) #t]
			[else 
				(if (contains s2 (car s1))  
					(subset (cdr s1) s2)
					#f
				)
			]
		)
	)
)

(define equal-sets
	(lambda (s1 s2)
		(and (subset s1 s2) (subset s2 s1))
	)
)

(define factorial
	(lambda (f)
		(let factor ([num f] [prod 1])
			(cond
				[(eq? num 0) prod]
				[else (factor (- num 1) (* prod num))]
			)
		)
	)
)

(define product
	(lambda (ls)
		(call/cc 
			(lambda (break)
				(let prod ([rem ls] [acc 1])
					(cond
						[(null? rem) acc]
						[(eq? (car rem) 0) (break 0)]
						[else (prod (cdr rem) (* acc (car rem)))]
					)
				)
			)
		)
	)
)


