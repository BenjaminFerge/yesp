;;;; yesp.lisp

(in-package #:yesp)

(defparameter *db* (make-hash-table))
(defparameter *db-dir*
  (make-pathname
   :directory `(,@(pathname-directory (user-homedir-pathname))
		  ".yesp.d" "streams")))

(defun make-event (action payload version)
  (list :action action :payload payload :version version))

(defun make-event-stream (name events)
  (list :name name :events events))

(defun push-event (event stream)
  (push event (getf stream :events)))

(defun event-stream-path (name)
  (make-pathname
   :defaults *db-dir*
   :name name
   :type "lisp"))

(defun save-event-stream (event-stream)
  (with-open-file (out (event-stream-path (getf event-stream :name))
		       :direction :output
		       :if-exists :append
		       :if-does-not-exist :create)
    (with-standard-io-syntax
      (dolist (item (getf event-stream :events)) (prin1 item out)))))

(defun event-stream-files ()
  (list-directory *db-dir*))

(defun read-event-stream-file (path)
  (with-open-file (in path)
    (loop for form = (read in nil)
       while form
       collect form)))

(defun load-event-stream (event-stream)
  (setf (gethash (getf event-stream :name) *db*) event-stream))

(defun event-stream-from-file (path)
  (make-event-stream
   (read-from-string (pathname-name path))
   (read-event-stream-file path)))

(defun load-event-stream-from-file (path)
  (load-event-stream (event-stream-from-file path)))

(defun load-db ()
  (walk-directory *db-dir* #'load-event-stream-from-file))

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
