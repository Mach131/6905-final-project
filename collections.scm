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
  (cards c:%collection-cards c:%collection-set-cards!))


;; Printing function for debugging
(define (c:print-collection coll)
  (write-line 'start-collection:)
  (for-each c:print-card
	    (c:%collection-cards coll))
  (write-line 'end-collection))


;;;     Collection Instantiator
#|  Makes a new empty collection.
|#

(define (c:make-collection #!optional card-list)
  (let ((use-cards
	 (if (default-object? card-list)
	     '()
	     card-list)))
    (guarantee c:card-list? use-cards)
    (c:%make-collection use-cards)))


;;;     Card Access
#|  Functions to facilitate inspection of the cards in a deck.
|#

;;  Gets a list of the collection's cards (in order).
(define (c:get-all-cards coll)
  (guarantee c:collection? coll)
  (c:%collection-cards coll))

;;  Gets a list of the first n cards in the collection.
(define (c:get-first-cards coll n)
  (guarantee c:collection? coll)
  (guarantee exact-nonnegative-integer? n)
  (let ((cards (c:get-all-cards coll)))
    (let ((c (min n (length cards))))
      (list-head cards c))))

;;  Gets a list of the last n cards in the collection.
(define (c:get-last-cards coll n)
  (guarantee c:collection? coll)
  (guarantee exact-nonnegative-integer? n)
  (let ((cards (c:get-all-cards coll)))
    (let ((c (max (- (length cards) n) 0)))
      (list-tail cards c))))

;;  Gets a list of n randomly selected cards in the collection.
(define (c:get-random-cards coll n)
  (guarantee c:collection? coll)
  (guarantee exact-nonnegative-integer? n)
  (let ((cards (c:get-all-cards coll)))
    (let lp ((n n) (c (length cards)) (root #t))
      (if (or (= n 0)
	      (= c 0))
	  '()
	  (let* ((new-idx (random c))
		 (rest-idxs 
		  (map (lambda (idx)         ; avoiding duplicates
			 (if (>= idx new-idx)
			     (+ idx 1)
			     idx))
		       (lp (- n 1) (- c 1) #f)))
		 (combined-idxs (cons new-idx rest-idxs)))
	    (if (not root)
		combined-idxs
		(map (lambda (idx) (list-ref cards idx))
		     combined-idxs)))))))

;;  Checks if the collection contains all of the given cards,
;;    requiring that duplicates be matched to separate cards.
(define (c:contains-cards? coll cards)
  (guarantee c:collection? coll)
  (guarantee c:card-list? cards)
  (let lp ((coll-cards (c:get-all-cards coll))
	   (check-cards cards))
    (if (null? check-cards)
	#t
	(let ((check-card (car check-cards)))
	  (if (memq check-card coll-cards)
	      (lp (delq check-card coll-cards)
		  (cdr check-cards))
	      #f)))))

#|  I realized that the above should be all that's necessary
assuming that we're only comparing cards by reference; if
comparing by value is necessary, something like this might
be used instead:

(define (c:contains-cards? coll cards)
  (guarantee c:collection? coll)
  (guarantee c:card-list? cards)
  (let lp ((coll-cards (c:get-all-cards coll))
	   (check-cards cards))
    (if (null? check-cards)
	#t
	(let ((check-card (car check-cards)))
	  (let ((card-counter
		 (lambda (c)
		   (length (filter
			    (lambda (card)
			      (eq? check-card card))
			    c)))))
	    (if (>= (card-counter coll-cards)
		    (card-counter check-cards))
		(lp (delq check-card coll-cards)
		    (delq check-card check-cards))
		#f))))))
|#

;;;     Card Transfer
#|  Functions to facilitate movement of cards between or within
collections.
|#

;; Helper functions (and interface for cards.scm)
(define (c:%add-cards! coll cards)
  (guarantee c:collection? coll)
  (guarantee c:card-list? cards)
  (let ((old-cards (c:%collection-cards coll)))
    (c:%collection-set-cards! coll
			      (append cards old-cards))))

(define (c:%remove-cards! coll cards)
  (guarantee c:collection? coll)
  (guarantee c:card-list? cards)
  (let ((old-cards (c:%collection-cards coll)))
    (c:%collection-set-cards! coll
			      (remove (lambda (c)
					(memq c cards))
				      old-cards))))


;;  Reverses the order of all cards in a collection.
(define (c:reverse-cards! coll)
  (guarantee c:collection? coll)
  (let ((cards (c:%collection-cards coll)))
    (c:%collection-set-cards! coll (reverse cards))))

;;  Randomizes the order of all cards in a collection.
(define (c:shuffle-cards! coll)
  (guarantee c:collection? coll)
  (let ((cards (c:%collection-cards coll)))
    (let ((shuffled (c:get-random-cards coll (length cards))))
      (c:%collection-set-cards! coll shuffled))))

;;  Removes the given cards from the source collection and adds
;;    them to the beginning of the destination. Uses the first
;;    copy of each card found in the source. Returns the number
;;    of cards successfully moved.
(define (c:move-cards! source-coll dest-coll cards)
  (guarantee c:collection? source-coll)
  (guarantee c:collection? dest-coll)
  (guarantee c:card-list? cards)
  (let* ((old-source-cards (c:get-all-cards source-coll))
	 (movable-cards
	  (filter (lambda (c) (memq c old-source-cards))
		  cards)))
    (c:%remove-cards! source-coll movable-cards)
    (c:%add-cards! dest-coll movable-cards)
    (let ((new-source-cards (c:get-all-cards source-coll)))
      (- (length old-source-cards)
	 (length new-source-cards)))))

;;  Removes the first n cards from the source collection and adds
;;    them to the beginning of the destination. Returns the number
;;    of cards that were successfully moved.
(define (c:move-first-cards! source-coll dest-coll n)
  (guarantee c:collection? source-coll)
  (guarantee c:collection? dest-coll)
  (guarantee exact-nonnegative-integer? n)
  (let ((cards (c:get-first-cards source-coll n)))
    (c:move-cards! source-coll dest-coll cards)))

;;  Removes the last n cards from the source collection and adds
;;    them to the beginning of the destination. Returns the number
;;    of cards that were successfully moved.
(define (c:move-last-cards! source-coll dest-coll n)
  (guarantee c:collection? source-coll)
  (guarantee c:collection? dest-coll)
  (guarantee exact-nonnegative-integer? n)
  (let ((cards (c:get-last-cards source-coll n)))
    (c:move-cards! source-coll dest-coll cards)))

#|        Test Cases

;;  Variables for card making

(define comp-basic
  (c:component-instantiator 'basic
			    '(rank)))

;;  Making/Adding to Collections

(define deck (c:make-collection))
(c:print-collection deck)
    ; -> [empty list of cards]
(for-each
 (lambda (r)
   ((c:card-instantiator (list (comp-basic r)))
    deck))
 (iota 5))
(c:print-collection deck)
    ; -> [5 cards, numbered 0 through 4]


;;  Card Accessors

(for-each c:print-card
	  (c:get-all-cards deck))
    ; -> [similar result to print-collection]
(for-each c:print-card
	  (c:get-first-cards deck 2))
    ; -> [4 and 3 cards]
(for-each c:print-card
	  (c:get-last-cards deck 3))
    ; -> [2, 1, and 0 cards]
(for-each c:print-card
	  (c:get-random-cards deck 3))
(for-each c:print-card
	  (c:get-random-cards deck 3))
    ; -> [likely different combinations of three cards]

(define dupe-pile (c:make-collection))
(define dupe-maker
  (c:card-instantiator (list (comp-basic 'd))))
(for-each
 (lambda (x) (dupe-maker dupe-pile))
 (iota 3))
(c:print-collection dupe-pile)
    ; -> [three copies of a "d" card]

(c:contains-cards? deck
		   (c:get-all-cards deck))
    ; -> #t
(c:contains-cards? deck
		   (c:get-all-cards dupe-pile))
    ; -> #f


;;  Card Transfer

(c:reverse-cards! deck)
(c:print-collection deck)
    ; -> [cards in ascending number order]
(c:shuffle-cards! deck)
(c:print-collection deck)
    ; -> [cards in random order]

(c:move-cards! deck dupe-pile
	       (c:get-random-cards deck 2))
    ; -> 2
(c:print-collection deck)
    ; -> [missing two random cards]
(c:print-collection dupe-pile)
    ; -> [has the two cards at the front]

(c:move-first-cards! deck dupe-pile 4)
    ; -> 3
(c:print-collection deck)
    ; -> [empty deck]
(c:print-collection dupe-pile)
    ; -> [has the other three cards at the front]

(c:move-last-cards! dupe-pile deck 3)
    ; -> 3
(c:print-collection deck)
    ; -> [has the three duplicates]
(c:print-collection dupe-pile)
    ; -> [still has the other five cards]

|#
