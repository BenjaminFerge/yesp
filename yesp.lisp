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

(defmethod print-object ((obj event) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((action action)
		     (payload payload)
		     (version version))
	obj
      (format stream "~a (~a): ~a" action version payload))))

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

(defmethod print-object ((obj event-stream) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name name)
		     (events events)
		     (version version))
	obj
      (format stream "~a (~a): ~%~{  ~a~%~}" name version events))))
