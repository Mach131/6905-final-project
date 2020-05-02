(load "./players.scm")
(load "./utils.scm")

(define-record-type c:game
    (c:%make-game name
		  min-players max-players players
		  player-deck-types
		  game-decks start-proc)
    c:game?
  (name c:%game-name)
  (min-players c:%game-min-players)
  (max-players c:%game-max-players)
  (players c:%game-players c:%set-game-players!)
  (player-deck-types c:%player-deck-types)
  (game-decks c:%game-decks c:%set-game-decks!)
  (start-proc c:%game-start-proc))

(define (c:print-game game)
  (write (list 'game:
	       (symbol->string (c:%game-name game))
	       (map c:print-player (c:%game-players game))
	       (map (lambda (x)
		      (c:print-collection (cadr x)))
		    (c:%game-decks game))))
  (newline))

(define (c:make-game name players player-decks game-decks play end-condition)
  (c:%make-game name players player-decks game-decks play end-condition))

(define (c:make-game name
		     min-players max-players player-deck-types
		     game-decks start-proc)
  (c:%make-game name
		min-players max-players '()
		player-deck-types
		game-decks start-proc))
		     

(define (c:get-game-deck game deck-name)
  (guarantee c:game? game)
  (guarantee symbol? deck-name)
  (cadar (filter
	  (lambda (x) (eq? (car x) deck-name))
	  (c:%game-decks game))))

(define (c:game-players game)
  (guarantee c:game? game)
  (c:%game-players game))

(define (c:add-player! game player-name)
  (guarantee c:game? game)
  (let ((players (c:game-players game))
	(max-players (c:%game-max-players game))
	(deck-types (c:player-deck-types the-game)))
    (if (or (not max-players)
	    (< (length players) max-players))
	(let ((new-player (c:make-player player-name deck-types)))
	  (c:%set-game-players!
	   game
	   (append players (list new-player))))
	(write-line "Maximum number of players already reached"))))

(define (c:player-deck-types game)
  (guarantee c:game? game)
  (c:%player-deck-types game))

(define (c:game-play game)
  (guarantee c:game? game)
  (let ((players (c:game-players game))
	(min-players (c:%game-min-players game)))
    (if (or (not min-players)
	    (>= (length players) min-players))
	(c:%game-start-proc game)
	(begin
	  (write-line "Not enough players added")
	  #f))))



;;; Utility functions

(define (c:game-deal game game-deck-type player-deck-type number-of-cards)
  (guarantee c:game? game)
  (guarantee symbol? game-deck-type)
  (guarantee symbol? player-deck-type)
  (guarantee exact-nonnegative-integer? number-of-cards)
  (let ((game-deck (c:get-game-deck game game-deck-type)))
    (c:shuffle-cards! game-deck)
    (for-each (lambda (player)
		(c:give-to-player player
				  player-deck-type
				  game-deck
				  (c:get-first-cards game-deck number-of-cards)))
	      (c:game-players game))))

