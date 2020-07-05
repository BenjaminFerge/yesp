;;;; yesp.lisp

(in-package #:yesp)

(defstruct event
  id
  action
  payload
  version)

(defclass event-stream ()
  ((id
    :initarg :id
    :initform (make-v4-uuid)
    :accessor id)
   (name
    :initarg :name
    :accessor name)
   (events
    :initarg :events
    :initform nil
    :accessor events)))

(defmethod version ((stream event-stream))
  (let ((last-event (car (last (events stream)))))
    (if last-event
	(event-version last-event)
	0)))

(defmethod valid-p ((e event) (s event-stream))
  (or
   (and (= (list-length (events s)) 0) (= (event-version e) 1))
   (= (version s) (1- (event-version e)))))

(defmethod create-event ((s event-stream) &key action payload version)
  (let ((e (make-event :id (make-v4-uuid) :action action :payload payload :version version)))
    (restart-case (push-event e s)
      (accept-next-version () (create-event s :action action :payload payload :version (1+ (version s)))))))

(define-condition event-version-mismatch (error)
  ((got :initarg :got :reader got)
   (expected :initarg :expected :reader expected))
  (:report (lambda (condition stream)
	     (format stream "Event version mismatch!~%Expected: ~a, got: ~a"
		     (expected condition)
		     (got condition)))))

(defmethod push-event ((e event) (s event-stream))
  (unless (valid-p e s)
    (error 'event-version-mismatch :expected (1+ (version s)) :got (event-version e)))
  (setf (slot-value s 'events) (nreverse (push e (slot-value s 'events))))
  (save-event-stream s))

(defun count-event-streams ()
  (loop for k being the hash-keys in *db* using (hash-value v) collecting k into names collecting (length v) into stream-count finally (return (pairlis names stream-count))))

(defun reload-db ()
  (setf *db* (make-hash-table))
  (load-db))

(defun find-event-stream-by-str-id (id)
  (handler-case (make-uuid-from-string id)
    (error (c)
      (declare (ignore c))
      (return-from find-event-stream-by-str-id)))
  (loop named outer for values being the hash-values of *db* do
       (loop for evs in values do
     	    (when (eq (intern id) (id evs))
	      (return-from outer evs)))))
