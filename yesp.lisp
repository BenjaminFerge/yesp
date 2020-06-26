;;;; yesp.lisp

(in-package #:yesp)

(defparameter *db* (make-hash-table))
(defparameter *db-dir*
  (make-pathname
   :directory `(,@(pathname-directory (user-homedir-pathname))
		  ".yesp.d" "streams")))

(defstruct event
  action
  payload
  version)

(defclass event-stream ()
  ((name
    :initarg :name
    :accessor name)
   (events
    :initarg :events
    :initform nil
    :accessor events)))

(defmethod version ((stream event-stream))
  (let ((last-event (car (last (events stream)))))
    (when last-event
      (event-version last-event))))

(defmethod valid-p ((e event) (s event-stream))
  (= (version s) (1- (event-version e))))

(defmethod push-event ((e event) (s event-stream))
  (unless (valid-p e s)
    (error (format nil "Event version mismatch!~%Expected: ~a, got: ~a" (1+ (version s)) (event-version e))))
  (setf (slot-value s 'events) (nreverse (push e (slot-value s 'events)))))

(defun event-stream-path (name)
  (make-pathname
   :defaults *db-dir*
   :name name
   :type "lisp"))

(defun save-event-stream (event-stream)
  (with-open-file (out (event-stream-path (name event-stream))
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (with-standard-io-syntax
      (dolist (item (events event-stream)) (prin1 item out)))))

(defun event-stream-files ()
  (list-directory *db-dir*))

(defun read-event-stream-file (path)
  (with-open-file (in path)
    (loop for form = (read in nil)
       while form
       collect form)))

(defun load-event-stream (event-stream)
  (setf (gethash (name event-stream) *db*) event-stream))

(defun event-stream-from-file (path)
  (make-instance 'event-stream
   :name (read-from-string (pathname-name path))
   :events (read-event-stream-file path)))

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

(defvar *xml-server*)

(defun start-rpc-server (port)
  (setq *xml-server* (s-xml-rpc:start-xml-rpc-server :port port)))

(defun stop-rpc-server ()
  (stop-server *xml-server*))

(defun s-xml-rpc-exports::|lisp.GCD| (m n)
  (gcd m n))

(defun s-xml-rpc-exports::|lisp.getTime| ()
  (multiple-value-list (get-decoded-time)))

