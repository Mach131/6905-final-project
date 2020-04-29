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
  (cards c:%collection-cards))


;; Printing function for debugging
(define (c:print-collection coll)
  (for-each c:print-card (c:%collection-cards coll)))


;;;     Collection Instantiator
#|  Makes a new empty collection.
|#

(define (c:make-collection)
  'todo)


;;;     Card Access
#|  Functions to facilitate inspection of the cards in a deck.
|#

;;  Gets a list of the collection's cards (in order).
(define (c:get-all-cards coll)
  'todo)

;;  Gets a list of the first n cards in the collection.
(define (c:get-first-cards coll n)
  'todo)

;;  Gets a list of the last n cards in the collection.
(define (c:get-last-cards coll n)
  'todo)

;;  Gets a list of n randomly selected cards in the collection.
(define (c:get-random-cards coll n)
  'todo)

;;  Checks if the collection contains all of the given cards,
;;    requiring that duplicates be matched to separate cards.
(define (c:contains-cards? coll cards)
  'todo)

;;;     Card Transfer
#|  Functions to facilitate movement of cards between or within
collections.
|#

;; Helper functions (and interface for cards.scm)
(define (c:%add-cards! collection cards)
  'todo)

(define (c:%remove-cards! collection cards)
  'todo)

;;  Reverses the order of all cards in a collection.
(define (c:reverse-cards! coll)
  'todo)

;;  Randomizes the order of all cards in a collection.
(define (c:shuffle-cards! coll)
  'todo)

;;  Removes the given cards from the source collection and adds
;;    them to the beginning of the destination. Uses the first
;;    copy of each card found in the source. Returns the number
;;    of cards successfully moved.
(define (c:move-cards! source-coll dest-coll cards)
  'todo)

;;  Removes the first n cards from the source collection and adds
;;    them to the beginning of the destination. Returns the number
;;    of cards that were successfully moved.
(define (c:move-first-cards! source-coll dest-coll cards)
  'todo)

;;  Removes the last n cards from the source collection and adds
;;    them to the beginning of the destination. Returns the number
;;    of cards that were successfully moved.
(define (c:move-last-cards! source-coll dest-coll cards)
  'todo)