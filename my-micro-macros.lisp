(defpackage my-micro-macros
  (:use :cl)
  (:nicknames mmm)
  (:export
   ;; control flow macros
   while until
   ;; biding macros
   with-assoc-values with-ht-values
   with-default-values with-default-values*
   ;; anaphoric macros
   aif awhen aunless awhile
   ;; form translation macros
   -> ->>))

(in-package mmm)

;;;; control flow macros

(defmacro while (test &body body)
  `(loop
      :until ,test
      :do ,@body))

(defmacro until (test &body body)
  `(loop
      :while ,test
      :do ,@body))


;;;; binding macros

(defmacro with-assoc-values (binds alist &body body)
  (let ((al (gensym "alist")))
    (flet ((expand-bind (bind)
	     `(,(first bind)
		(cdr (assoc ,(second bind) ,al)))))
      `(let ((,al ,alist))
	 (let ,(mapcar #'expand-bind binds)
	   ,@body)))))

(defmacro with-ht-values (binds hash-table &body body)
  (let ((ht (gensym "alist")))
    (flet ((expand-bind (bind)
	     `(,(first bind)
		(gethash ,(second bind) ,ht))))
      `(let ((,ht ,hash-table))
	 (let ,(mapcar #'expand-bind binds)
	   ,@body)))))

(defmacro with-default-values (binds &body body)
  "letとほぼ同じ動作をしますが、varシンボルがすでにnil以外の値に
束縛されている場合、その値を利用します。"
  (labels ((expand-bind (bind)
	     `(,(first bind)
		;; 'boundp' ignores any lexical environment.
		(restart-case
		    (handler-bind ((unbound-variable
				    #'(lambda (c)
					(declare (ignore c))
					(invoke-restart 'my-restart))))
		      (if ,(first bind) ,(first bind) ,(second bind)))
		  (my-restart () ,(second bind))))))
    `(let ,(mapcar #'expand-bind binds)
       ,@body)))

(defmacro with-default-values* (binds &body body)
  "with-default-valuesとほぼ同じですが、こちらはletではなく
let*に展開されます。"
  (labels ((expand-bind (bind)
	     `(,(first bind)
		;; 'boundp' ignores any lexical environment.
		(restart-case
		    (handler-bind ((unbound-variable
				    #'(lambda (c)
					(declare (ignore c))
					(invoke-restart 'my-restart))))
		      (if ,(first bind) ,(first bind) ,(second bind)))
		  (my-restart () ,(second bind))))))
    `(let* ,(mapcar #'expand-bind binds)
       ,@body)))

;;;; anaphoric macros

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &rest body)
  `(let ((it ,test))
     (when it ,@body)))

(defmacro aunless (test &rest body)
  `(let ((it ,test))
     (unless it
       ,@body)))

(defmacro awhile (test &rest body)
  `(loop
      :for it = ,test then ,test
      :until it
      :do ,@body))

;;;; form translation macros

(defmacro -> (exp &rest rest)
  (if rest
      (let ((fst (car rest))
            (rest (cdr rest)))
        (typecase fst
          (symbol `(-> (,fst ,exp) ,@rest))
          (atom `(-> (,fst ,exp) ,@rest))
          (list `(-> (,(car fst) ,exp ,@(cdr fst)) ,@rest))))
      exp))

(defmacro ->> (exp &rest rest)
  (if rest
      (let ((fst (car rest))
            (rest (cdr rest)))
        (typecase fst
          (symbol `(->> (,fst ,exp) ,@rest))
          (atom `(->> (,fst ,exp) ,@rest))
          (list `(->> (,(car fst) ,@(cdr fst) ,exp) ,@rest))))
      exp))
