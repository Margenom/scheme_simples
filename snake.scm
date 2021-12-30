(import ncurses srfi-1 (rename (chicken random) (pseudo-random-integer random)))

(define (putpoint cord ch)
	(mvaddch (car cord) (if (list? cord) (cadr cord) (cdr cord)) ch)
	(move (car cord) (if (list? cord) (cadr cord) (cdr cord))))

(define (main)
	(define tail '((10 10)))
	(define step '(0 0))
	(define lim 1)
	(define end #t)
	(define nice '(10 20))

	(initscr)
	(cbreak)
	(noecho)
	(clear)
	(timeout 80)

	(let loop((c (getch))(h 24)(w 79))

		 (set! step (case c
			 ((#\j) '(+1 0))
			 ((#\k) '(-1 0))
			 ((#\h) '(0 -1))
			 ((#\l) '(0 +1))
			 ((#\q) (set! end #f) step)
			 ((#\t) (set! lim (+ lim 1)) step)
			 (else step)))

	 (let ((t (cons (map (lambda(a b l) (min l (max 1 (+ a b))))
		 					(car tail) step (list h w)) tail)))
	 		(set! tail (if (> (length t) (+ 1 lim)) (take t (+ lim 1)) t)))

	 (if (equal? (car tail) nice) (begin
		 (set! nice (map + '(2 2) (map random (list (- h 4) (- w 4)))))
		 (set! lim (+ lim 1))))

	 (if (and (not (equal? step '(0 0))) (member (car tail) (cdr tail)))
		 (set! end #f))

	 (let drw((p (reverse (cdr (take tail lim)))))
		 (unless (null? p) (let()
			 (putpoint (car p) #\#)
			 (drw (cdr p)))))

	 (putpoint (list-ref tail (-(length tail)1)) #\ )
	 (putpoint (car tail) #\@)
	 (putpoint nice #\%)

	 (refresh)

	 (if end (loop(getch) (-(LINES)2) (-(COLS)2))))

	(set! end (string-append "Game Ower! " "Your scope: " (number->string lim)))

	(timeout 1000)
	(mvaddstr 10 10 end)
	(refresh)
	(getch)
	(endwin)
	(print end))

(main)
