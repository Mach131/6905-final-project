;;;Interface
(load "./games.scm")
(load "./utils.scm")
;; Global variables used once the game begins
(define the-game #!default)
(define game-began #f)

;; Initializes the game
(define (reset-interface)
  (set! the-game #!default)
  (set! game-began #f))

(define (play-game name)
  (reset-interface)
  (let ((game-path
	 (string-append "./gameVariants/" (string name) ".scm")))
  (if (file-exists? game-path)
      (begin
	(load game-path)
	(if (default-object? the-game)
	    (write-line "Error: this variant hasn't defined a 'the-game' object.")
	    (write-line "Game loaded; call add-player to add players.")))
      (write-line "I don't know that game yet"))))

;; Adds player to game
(define (add-player name)
  (if game-began
    "The game has already begun. Try adding players when this game is over."
  (c:add-player! the-game name)))

;; Starts game players are no longer able to be added
(define (begin-game)
  (let ((game-start-proc (c:game-play the-game)))
    (if game-start-proc
	(begin
	  (set! game-began #t)
	  (game-start-proc))
	'not-started)))

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