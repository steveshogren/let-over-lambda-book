(load "chap3.lisp")

(defmacro! defunits% (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x)) ,(cadr x)))
                      (group units 2))))))

(macroexpand '(defunits% time s
     m 60
     h 3600
     d 86400
     ms 1/1000
     us 1/1000000))

(unit-of-time 1 s) ;; 1

(defun defunits-chaining% (u units)
  (let ((spec (find u units :key #'car)))
    (if (null spec)
        (errror "unknown unit ~a" u)
      (let ((chain (cadr spec)))
        (if (listp chain)
            (* (car chain)
               (defunits-chaining%
                 (cadr chain)
                 units))
          chain)))))
(defmacro! defunits%% (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x))
                          ,(defunits-chaining%
                             (car x)
                             (cons `(,base-unit 1)
                                   (group units 2)))))
                      (group units 2))))))

(defunits%% time s
  m 60
  h (60 m)
  d (24 h)
  ms (1/1000 s)
  us (1/1000000 ms))
(unit-of-time 50 h)

(defun defunits-chaining (u units prev)
  (if (member u prev)
      (error "~{ ~a~~ depends on~}"
             (cons u prev)))
  (let ((spec (find u units :key #'car)))
    (if (null spec)
        (error "unknown unit ~a" u)
      (let ((chain (cadr spec)))
        (if (listp chain)
            (* (car chain)
               (defunits-chaining
                 (cadr chain)
                 units
                 (cons u prev)))
          chain)))))
(defmacro! defunits (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity)
     (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x))
                          ,(defunits-chaining
                             (car x)
                             (cons
                              `(,base-unit 1)
                              (group units 2))
                             nil)))
                      (group units 2))))))

(defunits time s
  m 60
  h (60 m)
  d (24 h)
  ms (1/1000 s)
  us (1/1000000 ms)) ;; works
(unit-of-time 50 h)
(defunits time s
  m (1/60 h)
  h (60 m))

(defunits distance m
  km 1000
  cm 1/100
  mm (1/10 cm)
  nm (1/1000 mm)

  yard 9144/10000
  foot (1/3 yard)
  inch (1/12 foot)
  mile (1760 yard)
  furlong (1/8 mile)

  fathom (2 yard)
  nautical-mile 1852
  cable (1/10 nautical-mile)
  old-brit-nautical-mile (6080/3 yard)
  old-brit-cable (1/10 old-brit-nautical-mile)
  old-brit-fathom (1/100 old-brit-cable))

(/ (unit-of-distance 1 fathom)
   (unit-of-distance 1 old-brit-fathom))
(coerce (unit-of-distance 1/76 old-brit-fathom)
        'float)

(defun tree-leaves% (tree result)
  (if tree
      (if (listp tree)
          (cons
           (tree-leaves% (car tree)
                         result)
           (tree-leaves% (cdr tree)
                         result))
        result)))

(tree-leaves% '(2 (nil t (a . b)))
              'leaf)

(defun predicate-splitter (orderp splitp)
  (lambda (a b)
    (let ((s (funcall splitp a)))
      (if (eq s (funcall splitp b))
          (funcall orderp a b)
        s))))
(sort '(5 1 2 4 3 8 9 6 7)
      (predicate-splitter #'< #'evenp))

(defun tree-leaves%% (tree test result)
  (if tree
      (if (listp tree)
          (cons
           (tree-leaves%% (car tree) test result)
           (tree-leaves%% (cdr tree) test result))
        (if (funcall test tree)
            (funcall result tree)
          tree))))

(tree-leaves%%
 '(1 2 (3 4 (5 6)))
 (lambda (x)
   (declare (ignorable x))
   (and (numberp x) (evenp x)))
 (lambda (x)
   (declare (ignorable x))
   'even-number))

(defmacro tree-leaves (tree test result)
  `(tree-leaves%%
    ,tree
    (lambda (x)
      (declare (ignorable x))
      ,test)
    (lambda (x)
      (declare (ignorable x))
      ,result)))

(tree-leaves
 '(1 2 (3 4 (5 6)))
 (and (numberp x) (evenp x))
 'even-number)

(defmacro! nlet-tail (n letargs &rest body)
  (let ((gs (loop for i in letargs
                  collect (gensym))))
    `(macrolet
         ((,n ,gs
              `(progn
                 (psetq
                  ,@(apply #'nconc
                           (mapcar
                            #'list
                            ',(mapcar #'car letargs)
                            (list ,@gs))))
                 (go ,',g!n))))
       (block ,g!b
         (let ,letargs
           (tagbody
            ,g!n (return-from
                     ,g!b (progn ,@body))))))))
(defun nlet-tail-fact (n)
  (nlet-tail fact ((n n) (acc 1))
             (if (zerop n)
                 acc
               (fact (- n 1) (* acc n)))))

(nlet-tail-fact 500)

(find 'a
      '(((a b) (c d)) ((c d) (b a)))
      :key #'cadadr)
(defmacro cxr% (x tree)
  (if (null x)
      tree
    `(,(cond
        ((eq 'a (cadr x)) 'car)
        ((eq 'd (cadr x)) 'cdr)
        (t (error "non a/d symbol")))
      ,(if (= 1 (car x))
           `(cxr% ,(cddr x) ,tree)
         `(cxr% ,(cons (- (car x) 1) (cdr x))
                ,tree)))))
(defun eleventh (x)
  (cxr% (1 a 10 d) x))

(macroexpand
  '(cxr% (1 a 10 d) x))

(eleventh '(1 2 3 4 5 6 7 8 9 10 11 12 13))

(defvar cxr-inline-thresh 10)
(defmacro! cxr (x tree)
  (if (null x)
      tree
    (let ((op (cond
               ((eq 'a (cadr x)) 'car)
               ((eq 'd (cadr x)) 'cdr)
               (t (error "non a/d sumbol")))))
      (if (and (integerp (car x))
               (<= 1 (car x) cxr-inline-thresh))
          (if (= 1 (car x))
              `(,op (cxr ,(cddr x) ,tree))
            `(,op (cxr ,(cons (- (car x) 1) (cdr x))
                       ,tree)))
        `(nlet-tail
          ,g!name ((,g!count ,(car x))
                   (,g!val (cxr ,(cddr x) ,tree)))
          (if (>= 0 ,g!count)
              ,g!val
            ;; will be a tail:
            (,g!name (- ,g!count 1)
                     (,op ,g!val))))))))
(defun nthcdr% (n list)
  (cxr (n d) list))

(defun nth% (n list)
  (cxr (1 a n d) list))

(macroexpand
 '(cxr (n d) list))

(defmacro def-english-list-accessors (start end)
  (if (not (<= 1 start end))
      (error "bad start end range"))
  `(progn
     ,@(loop for i from start to end collect
             `(defun
                ,(symb
                  (map 'string
                       (lambda (c)
                         (if (alpha-char-p c)
                             (char-upcase c)
                           #\-))
                       (format nil "~:r" i)))
                (arg)
                (cxr (1 a ,(- i 1) d) arg)))))
(macroexpand
 '(def-english-list-accessors 11 20))

(defun cxr-calculator (n)
  (loop for i from 1 to n
        sum (expt 2 i)))

(cxr-calculator 4)

(loop for i from 1 to 16
      collect (cxr-calculator i))

(defun cxr-symbol-p (s)
  (if (symbolp s)
      (let ((chars (coerce
                    (symbol-name s)
                    'list)))
        (and
         (< 6 (length chars))
         (char= #\C (car chars))
         (char= #\R (car (last chars)))
         (null (remove-if
                (lambda (c)
                  (or (char= c #\A)
                      (char= c #\D)))
                (cdr (butlast chars))))))))


(defun cxr-symbol-to-cxr-list (s)
  (labels ((collect (l)
                    (if l
                        (list*
                         1
                         (if (char= (car l) #\A)
                             'A
                           'D)
                         (collect (cdr l))))))
    (collect
     (cdr   ; chop off C
      (butlast ;chop off R
       (coerce
        (symbol-name s)
        'list))))))

(cxr-symbol-to-cxr-list 'caddadr)

(defmacro with-all-cxrs (&rest forms)
  `(labels
       (,@(mapcar
           (lambda (s)
             `(,s (l)
                  (cxr ,(cxr-symbol-to-cxr-list s)
                       l)))
           (remove-duplicates
            (remove-if-not
             #'cxr-symbol-p
             (flatten forms)))))
     ,@forms))

(with-all-cxrs #'cadadadadadadr)

(let ((count 0))
  (lambda (msg)
    (case msg
      ((:inc)
       (incf count))
      ((:dec)
       (decf count)))))

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
          (lambda (d)
            `(,(if (eq t (car d))
                   t
                 (list (car d)))
              (apply (lambda ,@(cdr d))
                     ,(if (eq t (car d))
                          g!args
                        `(cdr ,g!args)))))
          ds))))

(setf (symbol-function 'count-test)
      (let ((count 0))
        (dlambda
         (:reset () (setf count 0))
         (:inc (n) (incf count n))
         (:dec (n) (decf count n))
         (:bound (lo hi)
                 (setf count
                       (min hi
                            (max lo
                                 count)))))))


(count-test :reset)
(count-test :inc 100)
(count-test :bound -10 10)

(setf (symbol-function 'dlambda-test)
      (dlambda
       (:something-special ()
                           (format t "SPECIAL~%"))
       (t (&rest args)
          (format t "DEFAULT: ~a~%" args))))

(dlambda-test 1 2 3)


