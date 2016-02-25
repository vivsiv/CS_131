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