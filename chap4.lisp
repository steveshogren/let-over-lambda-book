'(football-game
  (game-started-at
   #.(get-internal-real-time))
  (coin-flip
   #.(if (zerop (random 2)) 'heads 'tails)))

`(football-game
  (game-started-at
   ,(get-internal-real-time))
  (coin-flip
   ,(if (zerop (random 2)) 'heads 'tails)))

(let ((s 'hello))
  `(,s world))

(let ((s '(b c d)))
  `(a . ,s))

