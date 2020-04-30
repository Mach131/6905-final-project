;;;Interface
(load "./games.scm")
;; Global variables used once the game begins
(define the-game)
(define game-began #f)

;; Initializes the game
(define (play-game name)
  (cond ((eq? name 'rummy500)
    (load "./gameVariants/rummy500.scm")
    (set! the-game (c:make-game name (list) player-deck-types (create-decks) play condition))
    "Please add players")
    (else (write "I don't know that game yet"))))

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
