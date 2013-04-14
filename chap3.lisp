(defun sleep-units% (value unit)
  (sleep
   (* value
      (case unit
        ((s) 1)
        ((m) 60)
        ((h) 3600)
        ((d) 86400)
        ((ms) 1/1000)
        ((us) 1/1000000)))))
(sleep-units% 500 'ms)  

(defmacro sleep-units (value unit)
  `(sleep
    (* ,value
       ,(case unit
          ((s) 1)
          ((m) 60)
          ((h) 3600)
          ((d) 86400)
          ((ms) 1/1000)
          ((us) 1/1000000)))))

(sleep-units 500 ms)

(defmacro unit-of-time (value unit)
  `(* ,value
      ,(case unit
         ((s) 1)
         ((m) 60)
         ((h) 3600)
         ((d) 86400)
         ((ms) 1/1000)
         ((us) 1/1000000))))
(macroexpand '(unit-of-time 4 d))

(load "chap1.lisp")
(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
                ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defun nlet-fact (n)
  (nlet fact ((n n))
        (if (zerop n)
            1
          (* n (fact (- n 1))))))

(macroexpand-1 '(nlet fact ((n n))
                    (if (zerop n)
                        1
                      (* n (fact (- n 1))))))

(defmacro x-injector ()
  'x)

(defmacro nif! (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond
        ((plusp ,g) ,pos)
        ((zerop ,g) ,zero)
        (t ,neg)))))

(defun is-symbol-p (symbl needle)
  (and (symbolp symbl)
       (> (length (symbol-name symbl)) 2)
       (string= (symbol-name symbl)
                needle
                :start1 0
                :end1 2)))


(defun g!-symbol-p (s) (is-symbol-p s "G!"))
(defun o!-symbol-p (s) (is-symbol-p s "O!"))

(* (remove-duplicates (flatten '(test (this (test this)))))
   *)

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))

(defmacro/g! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result) ,pos)
           ((zerop ,g!result) ,zero)
           (t ,neg))))

(defun mkstr (&rest args)
       (with-output-to-string (s)
          (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(*
 (nif -5 "positive" "zero" "negative")
  (macroexpand '(nif -5 "positive" "zero" "negative")) *)

(defun o!-symbol-to-g!-symbol (s)
  (cond
   ((o!-symbol-p s)
    (symb "G!" (subseq (symbol-name s) 2)))
   ;; do nothing
   (t s)))

(o!-symbol-to-g!-symbol 'osurst)

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))



