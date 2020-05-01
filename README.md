# 6905-final-project
Card game simulator for 6.905 Spring 2020.

To play a game start scheme and run (load "gameInterface.scm").

To select which game to play call (play-game 'game-name). Use
(add-player 'player-name) to add players; once everyone has been
added, call (begin-game) and follow the prompts accordingly.

Current game options are: 'rummy500, 'war
