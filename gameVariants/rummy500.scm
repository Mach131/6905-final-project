;;;Rummy500
(define rummy-game)
(define suits (list 'clubs 'hearts 'diamonds 'spades))
(define suit-values (list 1 2 3 4))

(define card-nums (list 'ace 'two 'three 'four 'five 'six 'seven 'eight 'nine 'ten 'jack 'queen 'king))
(define card-values (list 15 5 5 5 5 5 5 5 5 10 10 10 10))

 ;;Returns a 52 card deck (no jokers) where cards are ordered as above
 (define (create-draw-deck)
	(let ((col (c:make-collection)))
	(let lp ((suit-index 0))
				(let lp2 ((num-index 0))
					((c:card-instantiator (list
						((c:component-instantiator 'num (list (list-ref card-nums num-index))) (list-ref card-values num-index))
						((c:component-instantiator 'suit (list (list-ref suits suit-index))) (list-ref suit-values suit-index)))) col)

						(if (< num-index (- (length card-nums) 1))
									(lp2 (+ num-index 1))))
		(if (< suit-index (- (length suits) 1))
				(lp (+ suit-index 1))))
	col))

;; Creates decks for rummy500 game
(define (create-decks)
	(list (list'draw (create-draw-deck)) (list 'discard (c:make-collection))))

(define first-player 0)
(define current-player first-player)

(define player-deck-types (list 'hand 'played))
(define (play game)
	(set! rummy-game game)
	(c:game-deal game 7)
	(c:move-first-cards! (c:get-game-deck rummy-game 'draw) (c:get-game-deck rummy-game 'discard) 1)
	(newline)
	(player-turn))


(define condition 1)

(define (get-current-player)
	(list-ref (c:game-players rummy-game) current-player))

(define (player-turn)
	(write (c:player-name (get-current-player)))
	(display " it is your turn")
	(newline)
	(display "Call see-my-hand, see-my-played-cards, see-all-played-cards, see-discarded, draw-discarded, or draw-new to continue")
	(newline))

;; Displays current players hand
(define (see-my-hand)
	(c:print-collection (c:get-player-deck (get-current-player) 'hand))
	(newline))

;; Displays current players played cards
(define (see-my-played-cards)
	(c:print-collection (c:get-player-deck (get-current-player) 'played))
	(newline))

(define (see-players-played player)
	(c:print-collection (c:get-player-deck player 'played))
	(newline))

(define (see-all-played-cards)
	(map see-players-played (c:game-players rummy-game)))

(define (see-discarded)
	(let ((store (cards->collection (collection->cards (c:get-game-deck rummy-game 'discard)))))
	(c:reverse-cards! store)
	(c:print-collection store)))

;; Draw the desired number of cards from discard the last one must be played
(define (draw-discarded number)
	(c:move-last-cards!
		(c:get-game-deck rummy-game 'discard)
		(c:get-player-deck (get-current-player) 'hand)
		number)
	(display "Call discard, or play-set to continue")
	(newline))

;; Draw a new card from the deck
(define (draw-new)
	(c:move-first-cards!
		(c:get-game-deck rummy-game 'draw)
		(c:get-player-deck (get-current-player) 'hand)
		1)
	(display "Call discard, or play-set to continue")
	(newline))

;; Allows the player to play a set of cards at the given positions in their hand
(define (play-set . cards)
	(let ((my-cards
			(map
				(lambda (x)
					(list-ref
						(c:get-all-cards (c:get-player-deck (get-current-player) 'hand)) x))
				cards)))
		(c:move-cards!
			(c:get-player-deck (get-current-player) 'hand)
			(c:get-player-deck (get-current-player) 'played) my-cards))
		(if (= (length (c:get-all-cards (c:get-player-deck (get-current-player) 'hand))) 0)
			(display "Congratulations you have won!")
			(display "Call play-set, or discard to continue")))

;; Discards the card at position card-num in the current players hand and initiates the next
;; players turn
(define (discard card-num)
	(c:move-cards!
		(c:get-player-deck (get-current-player) 'hand)
		(c:get-game-deck rummy-game 'discard)
		(list (list-ref (c:get-all-cards (c:get-player-deck (get-current-player) 'hand)) card-num)))
	(if (< current-player (- (length (c:game-players rummy-game)) 1))
			(set! current-player (+ current-player 1))
			(set! current-player 0))
	(player-turn))
