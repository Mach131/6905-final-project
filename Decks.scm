(load "cards.scm")

;; Collections
(define-record-type c:collection
	(c:make-collection cards)
	c:collection?
	(cards c:%collection-cards c:%collection-add-cards!))


;; Printing for collection
(define (c:%print-collection collection)
	(write (list 'collection:
					(map c:print-card
						(c:%collection-cards collection))))
						(newline))

;; Collection Instantiator
(define (c:collection-instantiator card-list)
	(guarantee c:card-list? card-list)
	(c:make-collection card-list))

;;determines if x is a collection of cards
(define (c:collection? x) #t)
		; (and (list? x)
		; 			(eq? (car x) 'collection)
		; 			(every c:card? (collection->cards x))))

;;makes a new empty collection
(define (make-collection)
		(c:collection-instantiator (list)))

;;returns a list of only the cards of the collection
(define (get-all-cards collection)
	(guarantee c:collection? collection)
	(cdr collection))

;;convert from cards to collection
(define (cards->collection cards)
	(c:collection-instantiator cards))

;;convert from collection to cards
(define (collection->cards collection)
	(c:%collection-cards collection))

;;returns collection with cards that have been added
(define (c:add-cards! collection cards)
(c:%collection-add-cards!
 collection
 (append! (collection->cards collection) cards)))

;;returns a collection containing cards in collection but not in cards
;;TODO need to update for duplicates
(define (c:remove-cards! collection cards)
	(c:%collection-add-cards!
 	collection
		(filter
			(lambda (x)
				(not (contains-cards?
					(cards->collection cards)
					(list x))))
			(collection->cards collection))))

;;returns the first n cards of the collection
(define (get-first-cards collection n)
	(guarantee c:collection? collection)
	(guarantee number? n)
	(assert (> n -1))
	(let ((new-collection (make-collection)))
		(let lp ((x 0))
			(c:add-cards! new-collection (list (list-ref (collection->cards collection) x)))
			(if (= x (- n 1))
					new-collection
					(lp (+ x 1))))))

;;returns the last n cards of the collection
(define (get-last-cards collection n)
	(let ((new-collection collection))
	(c:remove-cards! new-collection (get-first-cards collection (- (length collection) (+ n 1))))))

;;returns true if the cards are all found in the given collection
;;TODO need to update for duplicates
(define (contains-cards? collection cards)
	(let lp ((x 0))
		(if (= (length (filter (lambda (y) (eq? (list-ref cards x) y)) (collection->cards collection))) 0)
				#f
				(if (< x (- (length cards) 1))
					(lp (+ x 1))
					#t))))

;; Shuffles the cards in the collection
(define (shuffle-cards! collection)
	(let ((new-collection collection))
	(let ((shuffled (make-collection))
				(cards (collection->cards collection)))
			(let lp ((x (length cards)))
				(let ((card (get-card-at new-collection (random x))))
				(c:add-cards! shuffled (list card))
				(c:remove-cards! new-collection (list card)))
				(if (= x 1)
				(c:%collection-add-cards!
				 collection
				 (collection->cards shuffled))
				 (lp (- x 1))
			)))))

;;returns the card from collection at the given position
(define (get-card-at collection position)
	(assert (< position (length (collection->cards collection))))
	(list-ref (collection->cards collection) position))


;; Removes cards from source and adds them to the end of destination
(define (move-cards! source destination cards)
	(guarantee collection? source)
	(guarantee collection? destination)
	(guarantee list? cards)
	(c:remove-cards! source cards)
	(c:add-cards! destination cards))

(define (move-first-cards! source destination n)
	(guarantee collection? source)
	(guarantee collection? destination)
	(assert (> n -1))
	(let ((cards (get-first-cards source n)))
	(move-cards! source destination cards)))

(define (move-last-cards! source destination n)
	(guarantee collection? source)
	(guarantee collection? destination)
	(assert (> n -1))
	(let ((cards (get-last-cards source n)))
	(move-cards! source destination cards)))

;;For testing

(define suits (list 'clubs 'hearts 'diamonds 'spades))
(define suit-values (list 1 2 3 4))

(define card-nums (list 'ace 'two 'three 'four 'five 'six 'seven 'eight 'nine 'ten 'jack 'queen 'king))
(define card-values (list 1 2 3 4 5 6 7 8 9 10 11 12 13))

 ;;Returns a 52 card deck (no jokers) where cards are ordered as above
 (define (create-standard-deck)
	(let ((col (make-collection)))
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

(define deck (create-standard-deck))

(define empty-deck (make-collection))


(c:%print-collection empty-deck)

(collection->cards deck)

(c:add-cards! empty-deck (collection->cards deck))

(collection->cards empty-deck)

(contains-cards? deck (collection->cards empty-deck))

(c:remove-cards! empty-deck (collection->cards deck))

(collection->cards empty-deck)

(shuffle-cards! deck)

(c:%print-collection deck)

()
