
(defpackage my-reader-macros
  (:use :cl :mf)
  (:nicknames mrm)
  (:export
   enable-string-reader
   disable-string-reader
   enable-short-lambda-reader
   disable-short-lambda-reader))

(in-package :mrm)

;;;"\n"を改行に変える
(defun enable-string-reader ()
  (set-macro-character
   #\"
   (lambda (stream ch)
     (declare (ignore ch))
     (with-output-to-string (s)
       (loop
	  :for i = (read-char stream)
	  :until (char= i #\")
	  :do
	  (if (char= i #\\)
		(let ((next (read-char stream)))
		  (case next
		    ((#\n)
		     (write-char #\newline s))
		    (T
		     (write-char next s))))
	      (write-char i s)))))))

(defun disable-string-reader ()
  (set-macro-character #\" #'sb-impl::read-string))

(defun dollar-symbol-p (sym)
  (and (symbolp sym) (char= #\$ (char (symbol-name sym) 0))))

(defun dollar-symbol-index (sym)
  (and (dollar-symbol-p sym)
       (parse-integer
	(symbol-name sym)
	:start 1 :junk-allowed t)))

(defun short-lambda-reader (stream ch1 ch2)
  (declare (ignore ch1 ch2))
  (let* ((body  (read-delimited-list #\} stream t))
	 (dollars (remove-if-not #'dollar-symbol-p (mf:flatten body)))
	 (rest-p (find "$R" dollars :test #'string= :key #'symbol-name))
	 (largest
	  (apply #'max (or (remove nil (mapcar #'dollar-symbol-index dollars))
			   '(0)))))
    (let ((args (loop :for i from 1 to largest
		   :collect (mf:symb "$" i))))
      `(lambda ,(if rest-p
		    `(,@args &rest ,rest-p)
		    `(,@args))
	 ,@body))))

(defun enable-short-lambda-reader ()
  (set-macro-character #\} (get-macro-character #\)))
  (set-dispatch-macro-character #\# #\{ #'short-lambda-reader))

(defmacro disable-short-lambda-reader ()
  (set-macro-character #\} nil)
  (set-dispatch-macro-character #\# #\{ nil))