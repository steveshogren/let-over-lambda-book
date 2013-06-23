;; makes a reader macro to expand a #f into a larger statement
(set-dispatch-macro-character #\# #\f
                              (lambda (stream sub-char numarg)
                                (declare (ignore stream sub-char))
                                (setq numarg (or numarg 3))
                                (unless (<= numarg 3)
                                  (error "bad value for #f: ~a" numarg))
                                `(declare (optimize (speed ,numarg)
                                                    (safety ,(- 3 numarg))))))

'#f  ;; (declare (optimize (speed 3) (safety 0)))
'#0f  ;; (declare (optimize (speed 0) (safety 3)))
'#1f  ;; (declare (optimize (speed 1) (safety 2)))

(defmacro fast-progn (&rest body)
  `(locally #f ,@body))
(defmacro safe-progn (&rest body)
  `(locally #0f ,@body))
(macroexpand '(fast-progn (+ 1 2)))
(macroexpand '(fast-progn
               (declare (type fixnum a))
               (the fixnum (+ a 1))))

