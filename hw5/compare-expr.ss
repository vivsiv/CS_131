(define compare-literals
	(lambda (lit1 lit2) 
		(if (equal? lit1 lit2)
			lit1
			(cond
				[(and (equal? lit1 #t) (equal? lit2 #f)) 'TCP]
				[(and (equal? lit1 #f) (equal? lit2 #t)) '(not TCP)]
				[(cons 'if (cons 'TCP (cons lit1 (cons lit2 '()))))]
			)
		)
	)
)

(equal? (compare-literals 12 12) 12)
(equal? (compare-literals 12 20) '(if TCP 12 20))
(equal? (compare-literals #t #t) #t)
(equal? (compare-literals #f #f) #f)
(equal? (compare-literals #t #f) 'TCP)
(equal? (compare-literals #f #t) '(not TCP))

