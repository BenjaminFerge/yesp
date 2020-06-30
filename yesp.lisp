;;;; yesp.lisp

(in-package #:yesp)

(defparameter *db* (make-hash-table))
(defparameter *db-dir*
  (make-pathname
   :directory `(,@(pathname-directory (user-homedir-pathname))
		  ".yesp.d" "streams")))

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
    (push-event e s)))

(defmethod push-event ((e event) (s event-stream))
  (unless (valid-p e s)
    (error (format nil "Event version mismatch!~%Expected: ~a, got: ~a" (1+ (version s)) (event-version e))))
  (setf (slot-value s 'events) (nreverse (push e (slot-value s 'events)))))

(defun event-stream-path (event-stream)
  (merge-pathnames (make-pathname
		    :directory `(:relative ,(symbol-name (name event-stream)))
		    :name (format nil "~a" (id event-stream))
		    :type "lisp")
		   *db-dir*))

(defun save-event-stream (event-stream)
  (with-open-file (out (event-stream-path event-stream)
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
  (push event-stream (gethash (name event-stream) *db*)))

(defun event-stream-from-file (path)
  (make-instance 'event-stream
		 :id (read-from-string (pathname-name path))
		 :name (intern (car (last (pathname-directory path))))
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

(defvar *xml-server* nil)

(defun start-rpc-server (port)
  (stop-rpc-server)
  (setq *xml-server* (s-xml-rpc:start-xml-rpc-server :port port)))

(defun stop-rpc-server ()
  (stop-server *xml-server*))

(defun s-xml-rpc-exports::|yesp.getDatabase| ()
       "123")

(defun s-xml-rpc-exports::|lisp.getTime| ()
       (multiple-value-list (get-decoded-time)))

(defun event-stream-to-xml (event-stream)
  (make-xml-element :name :stream :attributes `((:id . ,(symbol-name (id event-stream))) (:name . ,(symbol-name (name event-stream))) (:version . ,(write-to-string (version event-stream)))) :children (mapcar #'event-to-xml (events event-stream))))

(defun event-stream-print-xml (event-stream)
  (print-xml (event-stream-to-xml event-stream) :pretty t :input-type :xml-struct))

(defun event-to-xml (e)
  (make-xml-element :name :event :attributes `((:id . ,(symbol-name (event-id e))) (:action . ,(symbol-name (event-action e))) (:version . ,(write-to-string (event-version e))) (:payload . ,(format nil "~a" (event-payload e))))))

;; TODO: Nest streams inside DOMs per stream name
(defun db->xml ()
  (make-xml-element :name :data :attributes `((:count . ,(write-to-string (hash-table-count *db*)))) :children (let (result) (maphash #'(lambda (k v) (setf result (mapcar #'event-stream-to-xml v))) *db*) result)))
