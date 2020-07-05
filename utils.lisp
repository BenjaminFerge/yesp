(in-package #:yesp)

(defun alist->plist (alist)
  (loop for pair in alist with result finally (return result) do (setf (getf result (intern (symbol-name (car pair)) "KEYWORD")) (cdr pair))))

(defun event-stream-path (event-stream)
  (merge-pathnames (make-pathname
		    :directory `(:relative ,(symbol-name (name event-stream)))
		    :name (format nil "~a" (id event-stream))
		    :type "lisp")
		   *db-dir*))

(defmethod print-object ((object hash-table) stream)
  (format stream "#<HASH-TABLE :TEST ~a :COUNT ~a {~x}~%  ~{~{(~a : ~a)~}~^ ~}>"
          (hash-table-test object)
	  (hash-table-count object)
	  #+sbcl
	  (sb-kernel:get-lisp-obj-address object)
	  #+clisp
	  (system::address-of object)
	  #-(or sbcl clisp)
	  'not-implemented
	  (loop for key being the hash-keys of object
	     using (hash-value value)
	     collect (list key value))))
