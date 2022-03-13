(import srfi-1 utf8)

(define alpha '( 
	("0" 1 1 1 1 1)
	("1" 0 1 1 1 1)
	("2" 0 0 1 1 1)
	("3" 0 0 0 1 1)
	("4" 0 0 0 0 1)
	("5" 0 0 0 0 0)
	("6" 1 0 0 0 0)
	("7" 1 1 0 0 0)
	("8" 1 1 1 0 0)
	("9" 1 1 1 1 0)
	("a" 0 1)
	("b" 1 0 0 0)
	("c" 1 0 1 0)
	("d" 1 0 0)
	("e" 0)
	("f" 0 0 1 0)
	("g" 1 1 0)
	("h" 0 0 0 0)
	("i" 0 0)
	("j" 0 1 1 1)
	("k" 1 0 1)
	("l" 0 1 0 0)
	("m" 1 1)
	("n" 1 0)
	("o" 1 1 1)
	("p" 0 1 1 0)
	("q" 1 1 0 1)
	("r" 0 1 0)
	("s" 0 0 0)
	("t" 1)
	("u" 0 0 1)
	("v" 0 0 0 1)
	("w" 0 1 1)
	("x" 1 0 0 1)
	("y" 1 0 1 1)
	("z" 1 1 0 0)
	("а" 0 1)
	("б" 1 0 0 0)
	("в" 0 1 1)
	("г" 1 1 0)
	("д" 1 0 0)
	("е" 0)
	("ж" 0 0 0 1)
	("з" 1 1 0 0)
	("и" 0 0)
	("й" 0 1 1 1)
	("к" 1 0 1)
	("л" 0 1 0 0)
	("м" 1 1)
	("н" 1 0)
	("о" 1 1 1)
	("п" 0 1 1 0)
	("р" 0 1 0)
	("с" 0 0 0)
	("т" 1)
	("у" 0 0 1)
	("ф" 0 0 1 0)
	("х" 0 0 0 0)
	("ц" 1 0 1 0)
	("ч" 1 1 1 0)
	("ш" 1 1 1 1)
	("щ" 1 1 0 1)
	("ъ" 1 1 0 1 1)
	("ы" 1 0 1 1)
	("ь" 1 0 0 1)
	("э" 0 0 1 0 0)
	("ю" 0 0 1 1)
	("я" 0 1 0 1)
	("." 0 1 0 1 1 0)
	("," 1 1 0 0 1 1)
	("?" 0 0 1 1 0 0)
	("'" 0 1 1 1 0 1)
	("!" 1 0 1 0 1 1)
	("/" 1 0 0 1 0)
	("(" 1 0 1 1 0)
	(")" 1 0 1 1 1 0)
	("&" 0 1 0 0 0)
	(":" 1 1 1 0 0 0)
	(";" 1 0 1 0 0 1)
	("=" 1 0 0 0 1)
	("+" 0 1 0 1 0)
	("-" 1 0 0 0 1 0)
	("_" 0 0 1 1 1 0)
	("\"" 0 1 0 0 0 1)
	("$" 0 0 0 1 1 0 0)
	("@" 0 1 1 0 0 1)
	("S" 1 0 1 0 1) ;Stop
	("N" 0 1 0 1 0) ;New
	("W" 0 1 0 0 0) ;Wait
	("E" 0 0 0 0 0 0 0 0) ;Error
	("T" 0 0 0 1 1 0) ;Termination
	("I" 1 0 1))) ;Init

(define (code str)
	(map (lambda(k) (let ((l (assoc (->string k) alpha)))(if l (cdr l) '()))) (string->list str)))

(define LEN_DOT 1)
(define LEN_TIRE (* LEN_DOT 3))
(define LEN_ILIT (* LEN_DOT -1))
(define LEN_LIT (* LEN_TIRE -1))
(define LEN_MWORD (* LEN_DOT -7))

(define (to_str cd) 
	(fold (lambda(k v) (string-append v
		(make-string (abs k) (if (negative? k) #\0 #\1))))
	(fold (lambda(k v) (append v (list LEN_MWORD) 
		(cdr 
		(fold (lambda(x g) (append g (list LEN_ILIT 
			(if (zero? x) LEN_DOT LEN_TIRE)))) '() x))))
		'() )))