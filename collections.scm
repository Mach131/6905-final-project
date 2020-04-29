;;;;                Card Game Project - Collection Representation

#|
Collections are containers for cards that have some concept of
an order for them. The order does not necessarily need to be
used to access the collection's cards, but it allows collections
to be easily used for things like decks, discard piles, or
fields of "in-play" cards.
|#

(load "./cards.scm")


;;;     Collection Representation
#|  Collections mostly just need to keep track of a list
of the cards they currently contain.
|#


(define-record-type c:collection
    (c:%make-collection cards)
    c:collection?
  (cards c:%collection-cards c:%collection-set-cards))


;; Printing function for debugging
(define (c:print-collection coll)
(write (list 'collection:
				(map c:print-card
					(c:%collection-cards coll))))
					(newline))


(define (c:card-list? object)
	(and (list? object)
				(every c:card? object)))

;;;     Collection Instantiator
(define (c:collection-instantiator card-list)
	(guarantee c:card-list? card-list)
	(c:%make-collection card-list))

(define (c:make-collection)
  (c:collection-instantiator (list)))


;;;			Conversions

;;convert from cards to collection
(define (cards->collection cards)
	(c:collection-instantiator cards))

;;convert from collection to cards
(define (collection->cards collection)
	(c:%collection-cards collection))


;;;     Card Access
#|  Functions to facilitate inspection of the cards in a deck.
|#



;;  Gets a list of the collection's cards (in order).
(define (c:get-all-cards coll)
  (collection->cards coll))

;;  Gets a list of the first n cards in the collection.
(define (c:get-first-cards coll n)
(guarantee c:collection? coll)
(guarantee number? n)
(assert (> n -1))
(let ((new-coll (c:make-collection)))
	(let lp ((x 0))
		(c:%add-cards! new-coll (list (list-ref (collection->cards coll) x)))
		(if (= x (- n 1))
				new-coll
				(lp (+ x 1))))))

;;  Gets a list of the last n cards in the collection.
(define (c:get-last-cards coll n)
	(let ((new-coll coll))
	(c:%remove-cards! new-coll (get-first-cards coll (- (length coll) (+ n 1))))))


;;  Gets a list of n randomly selected cards in the collection.
(define (c:get-random-cards coll n)
  'todo)

;;  Checks if the collection contains all of the given cards,
;;    requiring that duplicates be matched to separate cards.
(define (c:contains-cards? coll cards)
(let lp ((x 0))
	(if (= (length (filter (lambda (y) (eq? (list-ref cards x) y)) (collection->cards coll))) 0)
			#f
			(if (< x (- (length cards) 1))
				(lp (+ x 1))
				#t))))

;;;     Card Transfer
#|  Functions to facilitate movement of cards between or within
collections.
|#

;; Helper functions (and interface for cards.scm)
(define (c:%add-cards! collection cards)
	(guarantee c:collection? collection)
	(c:%collection-set-cards
	 collection
 	(append! (collection->cards collection) cards)))

(define (c:%remove-cards! collection cards)
	(c:%collection-set-cards
	collection
		(filter
			(lambda (x)
				(not (c:contains-cards?
					(cards->collection cards)
					(list x))))
			(collection->cards collection))))

;;  Reverses the order of all cards in a collection.
(define (c:reverse-cards! coll)
  'todo)

;;  Randomizes the order of all cards in a collection.
(define (c:shuffle-cards! coll)
(if (not (= 0 (length (collection->cards coll))))
(let ((new-collection coll))
(let ((shuffled (c:make-collection))
			(cards (collection->cards coll)))
		(let lp ((x (length cards)))
			(let ((card (get-card-at new-collection (random x))))
			(c:%add-cards! shuffled (list card))
			(c:%remove-cards! new-collection (list card)))
			(if (= x 1)
			(c:%collection-set-cards
			 coll
			 (collection->cards shuffled))
			 (lp (- x 1))
		))))))

;;returns the card from collection at the given position
(define (get-card-at collection position)
	(assert (< position (length (collection->cards collection))))
	(list-ref (collection->cards collection) position))


;;  Removes the given cards from the source collection and adds
;;    them to the beginning of the destination. Uses the first
;;    copy of each card found in the source. Returns the number
;;    of cards successfully moved.
(define (c:move-cards! source-coll dest-coll cards)
	(guarantee c:collection? source-coll)
	(guarantee c:collection? dest-coll)
	(guarantee list? cards)
	(c:%remove-cards! source-coll cards)
	(c:%add-cards! dest-coll cards))

;;  Removes the first n cards from the source collection and adds
;;    them to the beginning of the destination. Returns the number
;;    of cards that were successfully moved.
(define (c:move-first-cards! source-coll dest-coll cards)
	(guarantee c:collection? source-coll)
	(guarantee c:collection? dest-coll)
	(assert (> n -1))
	(let ((cards (get-first-cards source-coll n)))
	(move-cards! source dest-coll cards)))

;;  Removes the last n cards from the source collection and adds
;;    them to the beginning of the destination. Returns the number
;;    of cards that were successfully moved.
(define (c:move-last-cards! source-coll dest-coll cards)
	(guarantee c:collection? source-coll)
	(guarantee c:collection? dest-coll)
	(assert (> n -1))
	(let ((cards (get-last-cards source-coll n)))
	(move-cards! source dest-coll cards)))

#|Testing

(define suits (list 'clubs 'hearts 'diamonds 'spades))
(define suit-values (list 1 2 3 4))

(define card-nums (list 'ace 'two 'three 'four 'five 'six 'seven 'eight 'nine 'ten 'jack 'queen 'king))
(define card-values (list 1 2 3 4 5 6 7 8 9 10 11 12 13))

 ;;Returns a 52 card deck (no jokers) where cards are ordered as above
 (define (create-standard-deck)
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

(define deck (create-standard-deck))

(define empty-deck (c:make-collection))


(c:print-collection deck)

(c:shuffle-cards! deck)

(c:print-collection deck)

(c:move-cards! deck empty-deck (collection->cards deck))

(c:%add-cards! empty-deck (collection->cards deck))

(collection->cards empty-deck)

(c:contains-cards? deck (collection->cards empty-deck))

(c:%remove-cards! empty-deck (collection->cards deck))

(collection->cards empty-deck)
 |#
