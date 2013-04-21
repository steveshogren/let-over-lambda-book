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

(let ((s '(b c d)))
  `(a ,@s e))
(defvar to-splice '(b c d))
`(a ,.to-splice e)
to-splice
(defun dangerious-use-of-bq ()
  `(a ,.'(b c d) e))
;;(dangerious-use-of-bq)

(defun safer-use-of-bq ()
  `(a
    ,. (mapcar #'identity '(b c d))
    e))

(let (*print-pretty*)
  (print
   `'(football-game
      (game-started-at
       ,(get-internal-real-time))
      (coin-flip
       ,(if (zerop (random 2))
            'heads
          'tails)))))
(let ((s 'c))
  ('a s))

(let ((let '`(let ((let ',let))
               ,let)))
  `(let ((let ',let)) ,let))
(equal * +)



