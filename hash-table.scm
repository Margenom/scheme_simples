
(define (hasher String Size)
	(let ((Out 0)) (map (lambda(K) 
		(set! Out (modulo (+ Out (char->integer K)) Size))) String) Out))

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

(define (ht-make Size) (make-vector Size #f))
(define (ht-set! HT Key Val) 
	(let*((Hkey (hasher Key (length HT))) (Pair (list Key Val)) (Tval (HT Hkey)))
		(vector-set! HT Hkey (if Val (if Tval (cons Pair Tval) (list Pair))
			(filter (lambda(K) (not (string=? (car K) Key))) Tval))))) ; delete
(define (ht-get HT Key) (let*((Hkey (hasher Key (length HT))) (Pairs (HT Hkey))) 
	(if Pairs (cadr (assoc Key Pairs)) #f)))
