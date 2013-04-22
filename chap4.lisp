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

(defun |#>-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((curr (read-char stream)
               (read-char stream)))
        ((char= #\newline curr))
      (push curr chars))
    (let* ((pattern (nreverse chars))
           (pointer pattern)
           (output))
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((null pointer))
        (push curr output)
        (setf pointer
              (if (char= (car pointer) curr)
                  (cdr pointer)
                pattern))
        (coerce
         (nreverse
          (nthcdr (length pattern) output))
         'string)))))
(set-dispatch-macro-character
 #\# #\> #'|#>-reader|)
"#>ENDthis \ adf > #>END"

(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream)
                   (read-char stream)))
            ((char= ch curr))
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (- n 1))))))

(segment-reader t #\/ 3)

#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:scan
       ,(car ,g!args)
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))

#+cl-ppcre
(defun |#~-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((mode-char (read-char stream)))
    (cond
     ((char= mode-char #\m)
      (match-mode-ppcre-lambda-form
       (segment-reader stream
                       (read-char stream)
                       1)))
     ((char= mode-char #\s)
      (subst-mode-ppcre-lambda-form
       (segment-reader stream
                       (read-char stream)
                       2)))
     (t (error "Unknown #~~ mode character")))))

#+cl-ppcre
(set-dispatch-macro-character #\# #\~ #'|#~-reader|)

(funcall #~s/abc/def/ "123abc")

(defvar not-shared '((1) (1)))
(eq (car not-shared) (cadr not-shared)) ;; nil

(defvar shared '(#1=(1) #1#))
(eq (car shared) (cadr shared)) ;; T

(defvar lister (list
    #1= (list 0)
    #1#
    #1#))

(let ((*print-circle* t))
  (print lister)
  t)

(let ((*print-circle* t))
  (print '#1= (hello . #1#))
  nil)


