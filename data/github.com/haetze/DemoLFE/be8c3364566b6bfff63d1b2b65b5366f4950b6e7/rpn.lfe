(defmodule rpn
  (export-macro define-function push)
  (export
   (create-stack 0)
   (get-n 1)
   (ps 0)
   (add 1)
   (*stack* 1)))

(defun create-stack ()
  (register 'stack (spawn 'rpn '*stack* '(()))))

(defun *stack* (data)
  (receive
    ((tuple 'add x)
     (*stack* (cons x data)))
    ((tuple 'get pid)
     (cond ((=:= (length data) 0)
	    (! pid 'nil)
	    (*stack* data))
	   ((/= (length data) 0)
	    (let (((cons d ds) data))
	      (! pid d)
	      (*stack* ds)))))))


(defun get-n
  ((0)
   ())
  ((n)
  (! 'stack (tuple 'get (self)))
  (receive
    ('nil
     'nil)
    (x
     (let ((y (get-n (- n 1))))
       (cond ((=:= y 'nil)
	      (! 'stack (tuple 'add x))
	      ())
	     ((/= y 'nil)
	      (cons x y))))))))


(defun add
  ((())
   'ok)
  (((cons x xs))
   (! 'stack (tuple 'add x))
   (add xs)))

(defmacro push l
  `(rpn:add ,l))

(defmacro define-function (n name f)
  `(defun ,name ()
     (let ((x (rpn:get-n ,n)))
       (if (=:= x ())
	 'not-enough-elements-on-the-stack
	 (rpn:add (list (apply ,f x)))))))


(defun ps ()
  (let ((n (get-n 1)))
    (cond ((=:= n 'nil)
	 'ok)
	  ((/= n 'nil)
	  (io:format "~p " n)
	  (ps)
	  (add n)))))
	