(load "chap3.lisp")
(load "chap1.lisp")

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
             collect (symb 'a i))
     ,(funcall
       (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
 #\# #\` #'|#`-reader|)

(defvar forth-registers
  '(pstack rstack pc
           dict compiling dtable))
(defstruct forth-word
  name prev immediate thread)

(defun forth-lookup (w last)
  (if last
      (if (eql (forth-word-name last) w)
          last
        (forth-lookup
         w (forth-word-prev last)))))

(defmacro forth-inner-interpreter ()
  `(loop
    do (cond
        ((functionp (car pc))
         (funcall (car pc)))
        ((consp (car pc))
         (push (cdr pc) rstack)
         (setf pc (car pc)))
        ((null pc)
         (setf pc (pop rstack)))
        (t
         (push (car pc) pstack)
         (setf pc (cdr pc))))
    until (and (null pc) (null rstack))))

;; Prim-form: (name immediate . forms)
(defvar forth-prim-forms nil)

(defmacro def-forth-naked-prim (&rest code)
  `(push ',code forth-prim-forms))

(defmacro def-forth-prim (&rest code)
  `(def-forth-naked-prim
     ,@code
     (setf pc (cdr pc))))

(def-forth-prim nop nil)

(def-forth-prim * nil
  (push (* (pop pstack) (pop pstack))
        pstack))

(def-forth-prim drop nil
  (pop pstack))

(def-forth-prim dup nil
  (push (car pstack) pstack))

(def-forth-prim swap nil
  (rotatef (car pstack) (cadr pstack)))

(def-forth-prim print nil
  (print (pop pstack)))

(def-forth-prim >r nil
  (push (pop pstack) rstack))

(def-forth-prim r> nil
  (push (pop rstack) pstack))

(defmacro new-forth ()
  `(let ,forth-registers
     (forth-install-prims)
     (lambda (v)
       (let ((word (forth-lookup v dict)))
         (if word
             (forth-handle-found)
             (forth-handle-not-found))))))

(defmacro! go-forth (o!forth &rest words)
  `(dolist (w ',words)
     (funcall ,g!forth w)))

(defvar forth-stdlib nil)

(defmacro forth-stdlib-add (&rest all)
  `(setf forth-stdlib
         (nconc forth-stdlib
                ',all)))


(defmacro new-forth ()
  `(alet ,forth-registers
         (setq dtable (make-hash-table))
         (forth-install-prims)
         (dolist (v forth-stdlib)
                    (funcall this v))
         (plambda (v) ,forth-registers
             (let ((word (forth-lookup v dict)))
               (if word
                   (forth-handle-found)
                 (forth-handle-not-found))))))

(defvar my-forth (new-forth))
(go-forth my-forth
          1 2.0 "three" 'four '(f i v e))

(defmacro forth-install-prims ()
  `(progn
     ,@(mapcar
        #`(let ((thread (lambda ()
                          ,@(cddr a1))))
            (setf dict
                  (make-forth-word
                   :name ',(car a1)
                   :prev dict
                   :immediate ,(cadr a1)
                   :thread thread))
            (setf (gethash thread dtable)
                  ',(cddr a1)))
        forth-prim-forms)))

(def-forth-prim [ t ; <- t means immediate
  (setf compiling nil))

(def-forth-prim ] nil ; <- not immediate
  (setf compiling t))

(defmacro forth-compile-in (v)
  `(setf (forth-word-thread dict)
         (nconc (forth-word-thread dict)
                (list ,v))))

(defmacro forth-handle-found ()
  `(if (and compiling
            (not (forth-word-immediate word)))
       (forth-compile-in (forth-word-thread word))
     (progn
       (setf pc (list (forth-word-thread word)))
       (forth-inner-interpreter))))

(defmacro forth-handle-not-found ()
  `(cond
    ((and (consp v) (eq (car v) 'quote))
     (if compiling
         (forth-compile-in (cadr v))
       (push (cadr v) pstack)))
    ((and (consp v) (eq (car v) 'postpone))
     (let ((word (forth-lookup (cadr v) dict)))
       (if (not word)
           (error "Postpone failed: ~a" (cadr v)))
       (forth-compile-in (forth-word-thread word))))
    ((symbolp v)
     (error "Word ~a not found" v))
    (t
     (if compiling
         (forth-compile-in v)
       (push v pstack)))))

(def-forth-prim create nil
  (setf dict (make-forth-word :prev dict)))

(def-forth-prim name nil
  (setf (forth-word-name dict) (pop pstack)))

(def-forth-prim immediate nil
  (setf (forth-word-immediate dict) t))

(go-forth my-forth
          ] dup * [)

(forth-stdlib-add
 create
 ] create ] [
 '{ name)

(forth-stdlib-add
 { (postpone [) [
 '} name immediate)

(setq my-forth (new-forth))

(go-forth my-forth
          { dup * } 'square name)

(go-forth my-forth
          5 square print)

(go-forth my-forth
          { square square } 'quartic name)

(go-forth my-forth
          1/2 quartic print)
(go-forth my-forth
          { 3 } 'three nae
          three three * print)
(go-forth my-forth
          { 4.0 } '4 name
          4 4 * print)

(def-forth-prim @ nil
  (push (car (pop pstack))
        pstack))
(def-forth-prim ! nil
  (let ((location (pop pstack)))
    (setf (car location) (pop pstack))))

(defmacro forth-unary-word-definer (&rest words)
  `(progn
     ,@(mapcar
        #`(def-forth-prim ,a1 nil
            (push (,a1 (pop pstack))
                  pstack))
        words)))

(defmacro! forth-binary-word-definer (&rest words)
  `(progn
     ,@(mapcar
        #`(def-forth-prim ,a1 nil
            (let ((,g!top (pop pstack)))
              (push (,a1 (pop pstack)
                         ,g!top)
                    pstack)))
        words)))

(forth-unary-word-definer
 not car cdr cadr caddr cadddr
 oddp evenp)
(forth-binary-word-definer
 eq equal + - / = < > <= >=
 max min and or)
