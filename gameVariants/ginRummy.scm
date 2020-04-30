;;;Gin Rummy 
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

(define (create-decks)
	(list (list'draw (create-draw-deck)) (list 'discard (c:make-collection))))

(define player-deck-types (list 'hand 'played))
(define (play game)
	(display "in game")
	(c:game-deal game 7))


(define condition 1)
