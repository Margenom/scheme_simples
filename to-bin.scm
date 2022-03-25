(define (float-to-bin F L)
  (let* ((M (* F 2)) (B (truncate M)) (T (- M B)))
    (display (if (zero? B) 0 1))
    (unless (or (zero? L) (zero? F)) (float-to-bin T (- L 1)))))

(define (digit-to-bin D L)
	(let* ((Q (/ D 2)) (B (modulo Q 2)) (T (- Q B)))
		(unless (or (zero? L) (= Q B)) (digit-to-bin T (- L 1)))
		(display (if (zero? B) 0 1))))

(define (to-bin N L) (digit-to-bin (truncate N) L) (display ".") (float-to-bin (- N (truncate N)) L))
