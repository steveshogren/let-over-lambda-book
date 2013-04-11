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
(unit-of-time 4 d)

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

(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond
        ((plusp ,g) ,pos)
        ((zerop ,g) ,zero)
        (t ,neg)))))

(nif -5 "positive" "zero" "negative")

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

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
         ,body))))

