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

(define compare-lists
	(lambda (l1 l2)
		(cond
			[(or (null? l1) (null? l2)) '()]
			[(cons (compare-expr (car l1) (car l2)) (compare-lists (cdr l1) (cdr l2)))]
		)
	)
)

(define compare-expr
	(lambda (e1 e2)
		(if (and (list? e1) (list? e2))
			(cond 
				[(equal? (length e1) (length e2)) (compare-lists e1 e2)]
				[(compare-literals e1 e2)] 
			)
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




