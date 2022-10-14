; Copyright (C) 2022 Daniil Shvachkin
; Released under the terms of the 3-clause BSD license 
(import ncurses srfi-1 (rename (chicken random) (pseudo-random-integer random)))

(define GAME_PLACE '(4 4))

(define (main)
	(define (mk_pises cont)
		(let ((temp_ps 
					(let* ((randch (lambda() 
									(integer->char (+ (char->integer #\A)
										(random (- (char->integer #\Z) (char->integer #\A)))))))
								(randstr (lambda() (list->string
									(let loop((len 2) (t '())) 
										(if (zero? len) t 
											(loop (- len 1) (cons (randch) t))))))))
						(let mkstrlst((len cont)(t '()))
							(if (zero? len) t
								(mkstrlst (- len 1) (cons 
										(let next((str (randstr))) 
											(if (member str t) (next (randstr)) str)) t))))))
				(recomp (lambda(k)
					(let loop((l k) (f '()))
						(if (null? l) f
							(let ((lst (call-with-values 
											(lambda() (split-at l (random (length l)))) cons)))
							(loop (append (car lst) (cddr lst))
								(cons (cadr lst) f))))))))
		(append (recomp temp_ps) (recomp temp_ps))))

	(initscr)
	(cbreak)
	(noecho)
	(clear)

	(define (size) (list (LINES) (COLS)))
	(define win 
		(let ((gp (map * '(3 4) GAME_PLACE)))
			(apply newwin (append gp
				(map (lambda(s k) (truncate (/ (- s k) 2))) (size) gp)))))
	(define pises (mk_pises (/ (apply * GAME_PLACE) 2)))
	(define (pget i j) (list-ref pises (+ (* i (car GAME_PLACE))j)))
	(define (shplase win i j str)
		(mvwaddstr win (+ (* i 3) 1) (+ (* j 4) 1) str))


	(wtimeout win 800)

	(let ((format 
					(lambda(f)
						(do ((i 0 (+ i 1))) ((= i (car GAME_PLACE)))
							(do ((j 0 (+ j 1))) ((= j (cadr GAME_PLACE)))
								(f i j))))))

	(format (lambda(i j)(shplase win i j (pget i j))))

	(wrefresh win)
	(wgetch win)

	(format (lambda(i j)(shplase win i j "##"))))

	(let ((toint (lambda(k) (- (char->integer k) (char->integer #\0))))
				(end #t)
				(temp_pises '())
				(showed_pises '())
				(cur_line #f))
		(let loop((c #\p))

			(if (= (length temp_pises) 2) 
				(if (let ((instr (map (lambda(k) 
								(list-ref pises (+ (* (car k) (car GAME_PLACE))(cadr k)))) temp_pises)))
							(string=? (car instr) (cadr instr)))
					(begin (set! showed_pises (append temp_pises showed_pises)) 
						(set! temp_pises '()))
					(let hide() (unless (null? temp_pises) (begin
						(unless (member (car temp_pises) showed_pises)
							(shplase win (caar temp_pises) (cadar temp_pises) "##"))
						(set! temp_pises (cdr temp_pises))
						(hide))))))
		
			(case c ((#\q) (set! end #f))
				((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) 
					(if cur_line 
						(let ((i cur_line) (j (toint c)))
							(if (and (< -1 i (car GAME_PLACE)) (< -1 j (cadr GAME_PLACE)) 
										(not (member (list i j) temp_pises))) (begin
								(set! temp_pises (cons (list i j) temp_pises)) 
								(shplase win i j (pget i j)) (set! cur_line #f))
								(set! cur_line #f)))
						(set! cur_line (toint c)))) )

			(wrefresh win)
		(if end (loop(wgetch win)))))
	(endwin))

(main)
