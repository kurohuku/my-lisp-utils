(defpackage my-function
  (:use :cl)
  (:nicknames mf)
  (:export
   mkstr
   symb
   make-keyword
   mklist
   iota
   enum
   group
   flatten
   compose
   chain))

(in-package :mf)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
(defun make-keyword (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;;SRFI-1 iota
(defun iota (n &optional (base 0))
  (loop :repeat n
     :for i from base :collect i))

;;enumerate
(defun enum (from to)
  (loop :for i from from to to :collect i))

(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (T (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;;split list to 'n' elements list
(defun group (lst n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (lst acc)
	     (let ((rest (nthcdr n lst)))
	       (if (consp rest)
		   (rec rest (cons (subseq lst 0 n) acc))
		   (nreverse (cons lst acc))))))
    (if lst (rec lst nil) nil)))

;;関数合成
;;(compose a b) => (a (b args))
(defun compose (&rest fns)
  (if fns
      (let ((fn (car (last fns)))
	    (fn-rest (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fn-rest
		    :from-end t
		    :initial-value (apply fn args))))
      #'identity))
;;(chain a b) => (b (a args))
(defun chain (&rest fns)
  (let ((fns (reverse fns)))
    (apply #'compose fns)))
    



