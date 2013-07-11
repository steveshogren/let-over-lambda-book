(load "chap3.lisp")
(load "chap1.lisp")

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


