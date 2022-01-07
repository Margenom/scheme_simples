(import ncurses srfi-1 (rename (chicken random) (pseudo-random-integer random)))

(define STICK_LEN 10)
(define TIMEOUT 40)

(define (main)
	(initscr)
	(cbreak)
	(noecho)
	(timeout TIMEOUT)
	(clear)

	(let*((pt '(10 10)) (der '(1 1)) (rc1 10) (rc2 10) (chet '(0 0))
				(putver (lambda(bp st) (let line((m bp) (len STICK_LEN))
					(mvaddstr (car m) (cadr m) st)
					(unless (= 1 len) (line (map + m '(1 0)) (- len 1)))))))
		(let loop((c #\p))
				(putver (list rc1 3) "  ")
				(putver (list rc2 (-(COLS) 5)) "  ")
				(let ((inver (lambda(k) (min (max k 1) (- (LINES) 1 STICK_LEN)))))
					(case c ;((#\h) (set! rc1 (inver (- rc1 1))))
									;((#\l) (set! rc1 (inver (+ rc1 1))))
									((#\j) (set! rc2 (inver (- rc2 1))))
									((#\k) (set! rc2 (inver (+ rc2 1)))))
					(if (> (truncate (/ (COLS) 2)) (cadr pt))
						(set! rc1 (inver (- rc1 
							(let*((pnt (- (car pt) (truncate (/ STICK_LEN 2)))) (cl (- rc1 pnt)))
								(if (zero? cl) 0 (if (> cl 0) 1 -1))))))))
				(putver (list rc1 3) "<>")
				(putver (list rc2 (-(COLS) 5)) "<>")

				(mvaddch (car pt) (cadr pt) #\ )
				(let*((npt (map + der pt)) 
							(inrang (lambda(p rng)
								(map (lambda(k lim) (< (car lim) k (cadr lim))) p rng)))
							(land (lambda(lst) (fold (lambda(k v) (and k v)) #t lst)))
							(rang (inrang npt (list (list 2 (- (LINES) 2)) (list 2 (- (COLS) 2)))))
							(stic1 (inrang npt (list (list rc1 (+ rc1 STICK_LEN)) '(3 5))))
							(stic2 (inrang npt (list (list rc2 (+ rc2 STICK_LEN)) 
								(list (-(COLS) 6) (-(COLS) 4))))))
					(if (or (land stic1) (land stic2)) (set! rang '(#t #f)))
					(unless (land rang) 
						(set! der (map * (if (car rang) '(1 -1) '(-1 1)) der))))
				(set! pt (map + der pt))
				(mvaddch (car pt) (cadr pt) #\@)
				(refresh)
			(unless (eq? c #\q) (loop(getch)))))
	(endwin))

(main)
