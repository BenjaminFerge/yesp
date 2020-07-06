(in-package #:yesp)

(defclass projection ()
  ((name
    :initarg :name
    :accessor name)
   (body
    :initarg :body
    :accessor body)
   (stream-id
    :initarg :stream-id
    :initform nil
    :accessor stream-id)))

(defmethod call ((p projection) &key :stream-name)
  (let ((streams (gethash stream-name *db*)))
    streams))

