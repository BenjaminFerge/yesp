;;;; yesp.lisp

(in-package #:yesp)

(defclass event ()
  ((action
    :initarg :action
    :accessor action)
   (payload
    :initarg :payload
    :accessor payload
    :initform nil)
   (version
    :initarg :version
    :accessor version
    :initform 0)))
#||
(defmethod print-object ((obj event) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((action action)
		     (payload payload)
		     (version version))
	obj
      (format stream "~a (~a): ~a" action version payload))))
||#
(defclass event-stream ()
  ((name
    :initarg :name
    :accessor name)
   (events
    :initarg :events
    :initform nil
    :accessor events)
   (version
    :initarg :version
    :accessor version
    :initform 0)))
#||
(defmethod print-object ((obj event-stream) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name name)
		     (events events)
		     (version version))
	obj
      (format stream "~a (~a): ~%~{  ~a~%~}" name version events))))

(defparameter *db-path* "~/yesp.lisp")
(defparameter *db* (make-hash-table))
||#

(defparameter *db* (make-hash-table))
(defparameter *db-path* "~/yesp.lisp")

;; TODO: Check version
(defmethod push-event ((s event-stream) (e event))
  (push e (gethash (name s) *db*)))

;; cl-store: https://common-lisp.net/project/cl-store/
(defun save-db ()
  (with-open-file (out *db-path*
		       :direction :output
		       :if-exists :append
		       :if-does-not-exist :create)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db ()
  (with-open-file (in *db-path*)
    (with-standard-io-syntax
      (setf *db* (read in)))))
