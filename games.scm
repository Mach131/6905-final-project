(load "./players.scm")

(define-record-type c:game
    (c:%make-game name players player-deck-types game-decks play end-condition)
    c:game?
	(name c:%game-name)
    (players c:%game-players c:%add-game-players)
	(player-deck-types c:%player-deck-types)
    (game-decks c:%game-decks c:%set-game-decks)
	(play c:%play-game)
    (end-condition c:%game-end-condition))

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

(define (c:get-game-deck game deck-name)
	(guarantee game? game)
	(guarantee symbol? deck-name)
	(cadar (filter
		(lambda (x) (eq? (car x) deck-name))
		(c:%game-decks game))))

(define (c:game-players game)
	(guarantee game? game)
	(c:%game-players game))

(define (c:add-player game player)
	(guarantee game? game)
	(guarantee player? player)
	(c:%add-game-players
		game
		(append! (c:game-players game) (list player))))

(define (c:player-deck-types game)
	(guarantee game? game)
	(c:%player-deck-types game))

(define (c:game-play game)
	(guarantee game? game)
	(c:%play-game game))

(define (c:game-deal game number-of-cards)
	(guarantee game? game)
	(guarantee exact-nonnegative-integer? number-of-cards)
	(c:shuffle-cards! (c:get-game-deck game 'draw))
	(map (lambda (x) (c:add-to-hand x (c:get-game-deck game 'draw)
					(c:get-first-cards (c:get-game-deck game 'draw) number-of-cards)))
			(c:game-players game)))
