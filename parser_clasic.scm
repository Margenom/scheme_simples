; Copyright (C) 2022 Daniil Shvachkin
; Released under the terms of the 3-clause BSD license 
(import srfi-1 (chicken irregex) (chicken string))

(define op_pri (reverse '(* / ^ + -)))

(define (parse form #!optional prior) (let*(
		(fm_sp (map
			(lambda(k) ((if (irregex-match "[0-9]+" k) string->number string->symbol) k))
			(string-split form " ")))
		(fm (let lp((f fm_sp) (norm fm_sp) (b '()) (e '())) 
			(if (null? f) (let ((p (map cons (reverse b) e)))
				(if (null? p) norm
					(let*((last (car p))
								(list-out (lambda(ls f t) (list (take ls f) (drop ls (+ 1 t)))))
								(last-out (list-out norm (car last) (cdr last)))
								(list-in (lambda(ls f t) (drop (take ls t) (+ f 1))))
								(last-in (list-in norm (car last) (cdr last)))
								(last-comp (append (car last-out) (list last-in) (cadr last-out))))
						(lp last-comp last-comp '() '()))))
			(lp (cdr f) norm 
				(if (eq? (car f) '|(|) (cons (- (length norm) (length f)) b) b) 
				(if (eq? (car f) '|)|) (cons (- (length norm) (length f)) e) e)))))
		(oppri (if prior prior op_pri))
		(addend (lambda(ls k) (append ls (list k))))
		(issc (lambda(sk) (and (list? sk) (= (length sk) 1) (list? (car sk)))))
		(list-split (lambda(ls k) (cons (take ls k) (drop ls k)))))

	(let opcatch((f fm) (n fm) (op (car oppri)) (args '()) (arg '()))
		(if (null? f)
			(if (null? args) 
				(if (eq? op (list-ref oppri (- (length oppri) 1))) 
					(if (not (list? (car n))) (car n) n)
					(opcatch n n (cadr (member op oppri)) '() '()))
				(let loop((a (reverse (addend args arg))))
					(if (null? a) (list op)
						(let ((a1 (if (issc (car a)) (caar a) (car a))))
							(addend (loop (cdr a)) (opcatch a1 a1 (car oppri) '() '()))))))
			(opcatch (cdr f) n op 
				(if (eq? (car f) op) (addend args arg) args)
				(if (eq? (car f) op) '() (addend arg (car f))))))))

