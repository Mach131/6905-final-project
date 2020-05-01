(load "./collections.scm")

(define-record-type c:players
    (c:%make-player name decks score)
    c:player?
    (name c:%player-name)
    (decks c:%player-decks c:%player-set-decks)
	(score c:%player-score c:%player-set-score))

(define (c:print-player player)
	(write 'player:)
	(write (c:%player-name player))
			(newline)
			(map (lambda (x)
					(write (car x))
					(c:print-collection (cadr x))
					(newline))
				(c:%player-decks player))
				(newline))

(define (c:make-player name)
	(guarantee symbol? name)
	(c:%make-player name (list (list 'hand (c:make-collection))) 0))

(define (c:make-player-with name deck-types)
	(guarantee symbol? name)
	(guarantee list? deck-types)
	(c:%make-player name (map (lambda (x) (list x (c:make-collection))) deck-types) 0))

(define (c:player-name player)
	(guarantee c:player? player)
	(c:%player-name player))

(define (c:increment-score player score)
	(guarantee c:player? player)
	(guarantee exact-nonnegative-integer? score)
	(c:%player-set-score
		player
		(+ (c:%player-score) score)))

(define (c:add-deck player name deck)
	(guarantee c:player? player)
	(guarantee symbol? name)
	(guarantee c:collection? deck)
	(c:%player-set-decks
		player
		(append! (c:%player-decks) (list name deck))))

;; Returns a players deck with the given name
(define (c:get-player-deck player deck-name)
	(guarantee c:player? player)
	(guarantee symbol? deck-name)
	(cadar (filter
		(lambda (x) (eq? (car x) deck-name))
		(c:%player-decks player))))

;; Adds cards to players hands
(define (c:add-to-hand player source cards)
	(guarantee c:player? player)
	(guarantee c:collection? source)
	(guarantee list? cards)
	(c:move-cards! source (c:get-player-deck player 'hand) cards))
