; Copyright (C) 2022 Daniil Shvachkin
; Released under the terms of the 3-clause BSD license 
(import srfi-1 (chicken irregex) (chicken string))

(define (parser expr) (car
	(foldl (lambda(v c)
		(if (or (number? c) (irregex-match "[a-z]" (symbol->string c)))
			(append v (list c)) 
			(append (take v (- (length v) 2)) (list (cons c (drop v (- (length v) 2))))))) 
		'() (map
			(lambda(k) ((if (irregex-match "[0-9]+" k) string->number string->symbol) k))
			(string-split expr " ")))))
