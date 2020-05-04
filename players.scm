OAOA(load "./collections.scm")

(define-record-type c:players
    (c:%make-player name decks attributes)
    c:player?
  (name c:%player-name)
  (decks c:%player-decks c:%player-set-decks!)
  (attributes c:%player-attributes))

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

(define (c:make-player name deck-types #!optional attributes)
  (let ((attribute-list
	 (if (default-object? attributes)
	     '()
	     attributes)))
    (guarantee symbol? name)
    (guarantee list? deck-types)
    (guarantee alist? attribute-list)
    (let ((deck-list (map
		      (lambda (deck-type)
			(list deck-type (c:make-collection)))
		      deck-types)))
      (c:%make-player name
		      deck-list
		      attribute-list))))

(define (c:player-name player)
  (guarantee c:player? player)
  (c:%player-name player))

(define (c:player-get-attribute player attribute-name)
  (let ((attrib-pair (assq attribute-name (c:%player-attributes player))))
    (and attrib-pair
	 (cadr attrib-pair))))

(define (c:player-set-attribute! player attribute-name value)
  (let ((attrib-pair (assq attribute-name (c:%player-attributes player))))
    (and attrib-pair
	 (set-cdr! attrib-pair (list value)))))

(define (c:add-deck! player name deck)
  (guarantee c:player? player)
  (guarantee symbol? name)
  (guarantee c:collection? deck)
  (c:%player-set-decks!
   player
   (cons (list name deck) (c:%player-decks))))

;; Returns a players deck with the given name
(define (c:get-player-deck player deck-name)
  (guarantee c:player? player)
  (guarantee symbol? deck-name)
  (let ((deck-pair (assq deck-name (c:%player-decks player))))
    (and deck-pair
	 (cadr deck-pair))))

;; Adds cards to players decks
(define (c:give-to-player! player player-deck-type source cards)
  (guarantee c:player? player)
  (guarantee symbol? player-deck-type)
  (guarantee c:collection? source)
  (guarantee list? cards)
  (let ((player-deck (c:get-player-deck player player-deck-type)))
    (c:move-cards! source player-deck cards)))
