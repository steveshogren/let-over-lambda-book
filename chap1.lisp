(defun mkstr (&rest args)
  (with-output-to-string (s)
                         (do-list (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                      (rec rest (cons
                                 (subseq source 0 n)
                                 acc))
                    (nreverse
                     (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec
                          (car x)
                          (rec (cdr x) acc))))))
    (rec x nil)))

(defun fact (x)
  (if (= x 0)
      1
    (* x (fact (- x 1)))))
;;(fact 4) 
(defun choose (n r)
  (/ (fact n)
     (fact (- n r))
     (fact r)))

(defun register-allocated-fixnum ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((acc 0))
    (loop for i from 1 to 100 do
          (incf (the fixnum acc)
                (the fixnum i)))
    acc))

(let ((direction 'up))
  (defun toggle-counter-direction ()
    (setq direction
          (if (eq direction 'up)
              'down
            'up)))
  (defun counter-class ()
    (let ((counter 0))
      (lambda ()
        (if (eq direction 'up)
            (incf counter)
          (decf counter))))))

