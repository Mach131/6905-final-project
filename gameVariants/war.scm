;;;;    first pass at war game

;;; Building deck

(define suits
  '(hearts clubs diamonds spades))
(define ranks
  '(A 2 3 4 5 6 7 8 9 10 J Q K))

(define war-comp
  (c:component-instantiator 'war '(rank suit)))

(define (create-draw-deck)
  (let ((deck (c:make-collection)))
    (let lp-r ((ranks ranks))
      (if (null? ranks)
	  deck
	  (let lp-s ((suits suits))
	    (if (pair? suits)
		(let ((rank (car ranks))
		      (suit (car suits)))
		  ((c:card-instantiator
		    (list (war-comp rank suit)))
		   deck)
		  (lp-s (cdr suits)))
		(lp-r (cdr ranks))))))))
	

;;; Game interface

; player-deck-types
(define player-deck-types
  (list 'deck 'played 'captures))
(define game-decks
  (list (list 'main (create-draw-deck))))
(define (start-proc)
  (c:game-deal! the-game 'main 'deck 26)
  (newline)
  (war-prompt))

(define the-game
  (c:make-game 'War
	       2 2 player-deck-types
	       game-decks start-proc))


;;; Main loop

(define (war-prompt)
  (for-each print-player-stats (c:game-players the-game))
  (write-line "Call do-round to do the next round"))

(define (do-round)
  (let ((p1 (car (c:game-players the-game)))
	(p2 (cadr (c:game-players the-game))))
    (let ((p1-play (play-cards p1 1))
	  (p2-play (play-cards p2 1)))
      (write-line (string-append (string (c:player-name p1)) " played "
				 (card->string p1-play)))
      (write-line (string-append (string (c:player-name p2)) " played "
				 (card->string p2-play)))
      (let ((p1-rank (check-rank p1-play))
	    (p2-rank (check-rank p2-play)))
	(cond ((> p1-rank p2-rank)
	       (write-line (string-append (string (c:player-name p1)) " wins!"))
	       (capture-cards p1)
	       (war-prompt))
	      ((> p2-rank p1-rank)
	       (write-line (string-append (string (c:player-name p2)) " wins!"))
	       (capture-cards p2)
	       (war-prompt))
	      (else
	       (write-line "War!")
	       (play-cards p1 3)
	       (play-cards p2 3)
	       (do-round)))))))

;;; Helper functions

(define (check-rank card)
  (let ((rank (c:get-field-value card 'war 'rank)))
    (case rank
      ((J) 11)
      ((Q) 12)
      ((K) 13)
      ((A) 14)
      (else rank))))

(define (play-cards player n)
  (let ((deck (c:get-player-deck player 'deck))
	(played (c:get-player-deck player 'played)))
    (c:move-first-cards! deck played n)
    (car (c:get-first-cards played 1))))

(define (capture-cards player)
  (let ((p1 (car (c:game-players the-game)))
	(p2 (cadr (c:game-players the-game))))
    (let ((p1-plays (c:get-player-deck p1 'played))
	  (p2-plays (c:get-player-deck p2 'played))
	  (caps (c:get-player-deck player 'captures)))
      (c:move-cards! p1-plays caps (c:get-all-cards p1-plays))
      (c:move-cards! p2-plays caps (c:get-all-cards p2-plays)))))

;;; Display functions

(define (card->string card)
  (let ((rank (c:get-field-value card 'war 'rank))
	(suit (c:get-field-value card 'war 'suit)))
    (let ((print-rank
	   (case rank
	     ((A) "Ace")
	     ((J) "Jack")
	     ((Q) "Queen")
	     ((K) "King")
	     (else (string rank)))))
      (string-append print-rank " of " (string suit)))))

(define (print-player-stats player)
  (let ((name (string (c:player-name player)))
	(deck (c:get-player-deck player 'deck))
	(caps (c:get-player-deck player 'captures)))
    (let ((deck-size (length (c:get-all-cards deck)))
	  (caps-size (length (c:get-all-cards caps))))
      (write-line (string-append name ": "
				 (string caps-size) " captured, "
				 (string deck-size) " in deck")))))
