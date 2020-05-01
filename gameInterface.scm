;;;Interface
(load "./games.scm")
;; Global variables used once the game begins
(define the-game)
(define game-began #f)

;; Initializes the game
(define (play-game name)
  (let ((game-path
	 (string-append "./gameVariants/" (string name) ".scm")))
  (if (file-exists? game-path)
      (begin
	(load game-path)
	(set! the-game
	      (c:make-game name '() player-deck-types
			   (create-decks) play condition))
	(write-line "Please add players"))
      (write-line "I don't know that game yet"))))

;; Adds player to game
(define (add-player name)
  (if game-began
    "The game has already begun. Try adding players when this game is over."
  (c:add-player the-game (c:make-player-with name (c:player-deck-types the-game)))))

;; Starts game players are no longer able to be added
(define (begin-game)
  (set! game-began #t)
  ((c:game-play the-game) the-game))

;; For debugging lists all the players and their hands
(define (see-players)
  (map (lambda (x) (c:print-player x)) (c:game-players the-game)))

#|  Tests
(play-game 'rummy500)
(add-player 'elizabeth)
(add-player 'dan)
(begin-game)
(discard 1)
  ;;-> You must draw a card to continue.
|#