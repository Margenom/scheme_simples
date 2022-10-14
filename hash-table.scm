; Copyright (C) 2022 Daniil Shvachkin
; Released under the terms of the 3-clause BSD license 
(define*(hasher String Size (Base 64))
	(let ((Out 0)) (map (lambda(K) 
		(set! Out (modulo (+ (* Base Out) (char->integer K)) Size))) String) Out))

; From chicken srfi-1
(define (filter pred lis)     ; Sleazing with EQ? makes this 
	(define null-list? null?)
	;  (check-arg procedure? pred filter)   
	; one faster.  
	(let recur ((lis lis)) (if (null-list? lis) lis      
	; Use NOT-PAIR? to handle dotted lists.  
	(let ((head (car lis)) (tail (cdr lis))) 
		(if (pred head) (let ((new-tail (recur tail)))  
	; Replicate the RECUR call so 
	(if (eq? tail new-tail) lis (cons head new-tail))) 
			(recur tail))))))     
; this one can be a tail call.  filter

; Hash-table with list
(define (ht-make Size) (make-vector Size #f))
(define (ht-set! HT Key Val)
	(let*((Hkey (hasher Key (length HT))) (Pair (list Key Val)) (Tval (HT Hkey)))
		(vector-set! HT Hkey (if Val (if Tval (cons Pair Tval) (list Pair))
			(filter (lambda(K) (not (string=? (car K) Key))) Tval))))) ; delete
(define (ht-get HT Key) (let*((Hkey (hasher Key (length HT))) (Pairs (HT Hkey))) 
	(if Pairs (cadr (assoc Key Pairs)) #f)))

; Open adress hash-table
(define htoa-make ht-make)
(define (_htoa-get-index HTA Key Size) (let* (
		(Hkey (hasher Key Size))
		(Exst (and (HTA Hkey) (string=? Key (car (HTA Hkey)))))
		(Hdis (or Exst (hasher Key (- Size 3))))
		(Nind (and Hdis (let rec((Di Hdis)) (let ((P (HTA (modulo (+ Hkey Di) Size)))) (if (and P (not (string=? (car P) Key))) (rec (* Di Hdis)) Di))))))
	(or Nind Hkey)))
(define (htoa-set! HTA Key Val)
	(vector-set! HTA (_htoa-get-index HTA Key (length HTA)) (cons Key Val)))
(define (htoa-get HTA Key) (cdr (HTA (_htoa-get-index HTA Key (length HTA)))))
(define (htoa-del HTA Key) (htoa-set! HTA Key #f)) 
