(load "chap3.lisp")
(load "chap1.lisp")

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

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(alambda (n)
    (if (> n 0)
        (cons
         n
         (self (- n 1)))))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
             collect (symb 'a i))
     ,(funcall
       (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
 #\# #\` #'|#`-reader|)

(mapcar (lambda (a)
          (list a ''empty))
        `(var-a var-b var-c))

(mapcar (lambda (a)
          `(,a 'empty))
        `(var-a var-b var-c))

(mapcar #`(,a1 'empty)
        `(var-a var-b var-c))

(let ((vars '(var-a var-b var-c)))
  (mapcar #2`(,a1 ',a2)
          vars
          (loop for v in vars
                collect (gensym
                         (symbol-name v)))))


(#3`(((,a1)) ,@a2 (,a3))
   (gensym)
   '(a b c)
   'hello)

(#3`(((,@a2)) ,a3 (,a1 ,a1))
   (gensym)
   '(a b c)
   'hello)

(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))

(let ((z (alet% ((sum) (mul) (expt))
          (funcall this :reset)
          (dlambda
           (:reset ()
                   (psetq sum 0
                          mul 1
                          expt 2))
           (t (n)
              (psetq sum (+ sum n)
                     mul (* mul n)
                     expt (expt expt n))
              (list sum mul expt))))))
  (loop for i from 1 to 5 collect (funcall z 2)))

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(setf (symbol-function 'alet-test) 
(alet ((acc 0))
      (alambda (n)
          (if (eq n 'invert)
              (setq this
                    (lambda (n)
                      (if (eq n 'invert)
                          (setq this #'self)
                        (decf acc n))))
            (incf acc n)))))
(alet-test 3)
(alet-test 'invert)
(macroexpand '(alet ((acc 0))
         (alambda (n)
             (if (eq n 'invert)
                 (setq this
                       (lambda (n)
                         (if (eq n 'invert)
                             (setq this #'self)
                           (decf acc n))))
               (incf acc n)))))
(defmacro alet-fsm (&rest states)
  `(macrolet ((state (s)
                     `(setq this #',s)))
     (labels (,@states) #',(caar states))))
(alet ((acc 0))
      (alet-fsm
       (going-up (n)
                 (if (eq n 'invert)
                     (state going-down)
                   (incf acc n)))
       (going-down (n)
                   (if (eq n 'invert)
                       (state going-up)
                     (decf acc n)))))

(defmacro! ichain-before (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             ,@body
             (apply ,g!indir-env
                    ,g!temp-args)))))

(alet ((acc 0))
      (ichain-before
       (format t "changing from ~a~%" acc))
      (lambda (n)
        (incf acc n)))

(setf (symbol-function 'ichain-test) 
      (alet ((acc 0))
            (ichain-before
             (format t "A~%"))
            (ichain-before
             (format t "B~%"))
            (ichain-before
             (format t "C~%"))
            (lambda (n)
              (incf acc n))))
(ichain-test 2)

(let ((tta (alet ((acc 0))
         (lambda (n)
           (ichain-before
            (format t "hello world~%"))
           (incf acc n)))))
      (loop for i from 1 to 4
            do
            (format t "~:r invocation: ~%" i)
            (funcall tta i)))

(defmacro! ichain-after (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             (prog1
                 (apply ,g!indir-env
                        ,g!temp-args)
               ,@body)))))
(let ((x (alet ((acc 0))
         (ichain-before
          (format t "changing from ~a~%" acc))
         (ichain-after
          (format t "changed to ~a~%" acc))
         (lambda (n)
           (incf acc n)))))
  (funcall x 7))

(defmacro! ichain-intercept% (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             (block intercept
               (prog1
                   (apply ,g!indir-env
                          ,g!temp-args)
                 ,@body))))))
(let ((x (alet ((acc 0))
         (ichain-intercept%
          (when (< acc 0)
            (format t "acc went negative ~%")
            (setq acc 0)
            (return-from intercept acc)))
         (lambda (n)
           (incf acc n)))))
  (funcall x -9))
(defmacro! ichain-intercept (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             (block ,g!intercept
               (macrolet ((intercept (v)
                                     `(return-from
                                          ,',g!intercept
                                        ,v)))
                 (prog1
                     (apply ,g!indir-env
                            ,g!temp-args)
                   ,@body)))))))
(let ((x (alet ((acc 0))
               (ichain-intercept
                (when (< acc 0)
                  (format t "acc went neg~%")
                  (setq acc 0)
                  (intercept acc)))
               (lambda (n)
                 (incf acc n)))))
  (funcall x -8))

(defmacro alet-hotpatch% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest args)
       (if (eq (car args) ':hotpatch)
           (setq this (cadr args))
         (apply this args)))))

(setf (symbol-function 'hotpatch-test)
         (alet-hotpatch% ((acc 0))
                         (lambda (n)
                           (incf acc n))))
(hotpatch-test 3)

(hotpatch-test
 :hotpatch
 (let ((acc 0))
   (lambda (n)
     (incf acc (* 2 n)))))

(defmacro! let-hotpatch (letargs &rest body)
  `(let ((,g!this) ,@letargs)
     (setq ,g!this ,@(last body))
     ,@(butlast body)
     (dlambda
      (:hotpatch (closure)
                 (setq ,g!this closure))
      (t (&rest args)
         (apply ,g!this args)))))

(defun let-binding-transform (bs)
  (if bs
      (cons
       (cond  ((symbolp (car bs))
               (list (car bs)))
              ((consp (car bs))
               (car bs))
              (t
               (error "Bad let bindings")))
       (let-binding-transform (cdr bs)))))
(let-binding-transform
 '(a (b) (c nil)))

(defmacro sublet (bindings% &rest body)
  (let ((bindings (let-binding-transform
                   bindings%)))
    (setq bindings
          (mapcar
           (lambda (x)
             (cons (gensym (symbol-name (car x))) x))
           bindings))
    `(let (,@(mapcar #'list
                     (mapcar #'car bindings)
                     (mapcar #'caddr bindings)))
       ,@(tree-leaves
          body
          #1= (member x bindings :key #'cadr)
          (caar #1#)))))
(macroexpand
 '(sublet ((a 0))
          (list a)))
(macroexpand
 '(sublet ((a 0))
          (list 'a)))

(defmacro sublet* (bindings &rest body)
  `(sublet ,bindings
           ,@(mapcar #'macroexpand-1 body)))

(macroexpand '(sublet* ((a 0))
                       (list a)))
(defmacro injector-for-a ()
  'a)

(macroexpand '(sublet* ((a 0))
                       (list a)))
(defmacro injector-for-a ()
  'a)

(macroexpand-1
 '(sublet* ((a 0))
           (injector-for-a)))

