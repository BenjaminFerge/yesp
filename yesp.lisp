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

(defun event-stream-path (event-stream)
  (merge-pathnames (make-pathname
		    :directory `(:relative ,(symbol-name (name event-stream)))
		    :name (format nil "~a" (id event-stream))
		    :type "lisp")
		   *db-dir*))

(defun save-event-stream (event-stream)
  (let ((path (event-stream-path event-stream)))
    (ensure-directories-exist path)
    (with-open-file (out path
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (with-standard-io-syntax
	(dolist (item (events event-stream)) (prin1 item out))))))

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
  (when *xml-server* (stop-rpc-server))
  (handler-case (setq *xml-server* (s-xml-rpc:start-xml-rpc-server :port port))
    (error (c)
      (format t "We caught a condition.~&")
      (values 0 c))))

(defun stop-rpc-server ()
  (stop-server *xml-server*))

(defun count-event-streams ()
  (loop for k being the hash-keys in *db* using (hash-value v) collecting k into names collecting (length v) into stream-count finally (return (pairlis names stream-count))))

(defun alist->plist (alist)
  (loop for pair in alist with result finally (return result) do (setf (getf result (intern (symbol-name (car pair)) "KEYWORD")) (cdr pair))))

(defun reload-db ()
  (setf *db* (make-hash-table))
  (load-db))

(defun s-xml-rpc-exports::|yesp.reload| ()
       (reload-db))

(defun s-xml-rpc-exports::|eventStreams.count| ()
       (reload-db)
       (alist->plist (count-event-streams)))

(defun xml-missing-args (&rest args)
  (format nil "Missing arguments: ~{~a~^, ~}" args))

(defun s-xml-rpc-exports::|eventStreams.getByName| (&optional (name nil name-supplied-p))
       (if name-supplied-p
	   (progn
	     (reload-db)
	     (mapcar #'event-stream->xml-rpc-struct (gethash (intern name) *db*)))
	   (xml-missing-args :name)))

(defun s-xml-rpc-exports::|eventStreams.getById| (&optional (id nil id-supplied-p))
       (if id-supplied-p
	   (progn
	     (reload-db)
	     (event-stream->xml-rpc-struct (find-event-stream-by-id id)))
	   (xml-missing-args :id)))

(defun s-xml-rpc-exports::|eventStreams.create| (&optional (name nil name-supplied-p))
       (if name-supplied-p
	   (event-stream->xml-rpc-struct
	    (let ((event-stream
		   (make-instance 'event-stream
				  :name (intern (string-upcase name)))))
	      (save-event-stream event-stream)
	      event-stream))
	   (xml-missing-args :name)))

(defun s-xml-rpc-exports::|eventStreams.pushEvent| (&optional (stream-id nil stream-id-supplied-p) (action nil action-supplied-p) (version nil version-supplied-p) (payload nil payload-supplied-p))
       (unless (and stream-id-supplied-p action-supplied-p version-supplied-p payload-supplied-p)
	 (return-from s-xml-rpc-exports::|eventStreams.pushEvent| (xml-missing-args :stream-id :action :version :payload)))
       (reload-db)
       (let ((evs (find-event-stream-by-str-id stream-id)))
	 (if evs
	     (handler-case 
		 (progn (create-event evs
				      :action (intern (string-upcase action))
				      :version version
				      :payload payload)
			'ok)
	       (event-version-mismatch (c)
		 (format nil "~a" c))
	       (error (c)
		 (declare (ignore c))
		 'unknown-error))
	   (format nil "Event stream not found with id: ~a" stream-id))))

(defun s-xml-rpc-exports::|lisp.getTime| ()
       (multiple-value-list (get-decoded-time)))

(defun find-event-stream-by-str-id (id)
  (handler-case (make-uuid-from-string id)
    (error (c)
      (declare (ignore c))
      (return-from find-event-stream-by-str-id)))
  (loop named outer for values being the hash-values of *db* do
       (loop for evs in values do
     	    (when (eq (intern id) (id evs))
	      (return-from outer evs)))))

(defun event-stream-to-xml (event-stream)
  (make-xml-element :name :stream :attributes `((:id . ,(symbol-name (id event-stream))) (:name . ,(symbol-name (name event-stream))) (:version . ,(write-to-string (version event-stream)))) :children (mapcar #'event-to-xml (events event-stream))))

(defun event-stream-print-xml (event-stream)
  (print-xml (event-stream-to-xml event-stream) :pretty t :input-type :xml-struct))

(defun event-to-xml (e)
  (make-xml-element :name :event :attributes `((:id . ,(symbol-name (event-id e))) (:action . ,(symbol-name (event-action e))) (:version . ,(write-to-string (event-version e))) (:payload . ,(format nil "~a" (event-payload e))))))

(defun event->xml-rpc-struct (event)
  (xml-rpc-struct
   :type 'event
   :id (format nil "~a" (event-id event))
   :action (event-action event)
   :payload (event-payload event)
   :version (event-version event)))

(defun event-stream->xml-rpc-struct (event-stream)
  (xml-rpc-struct
   :type 'event-stream
   :id (format nil "~a" (id event-stream))
   :name (name event-stream)
   :version (version event-stream)
   :events (mapcar #'event->xml-rpc-struct (events event-stream))))

(defun db->xml ()
  (make-xml-element
   :name :data
   :attributes `((:count . ,(write-to-string
			     (hash-table-count *db*))))
   :children (let (result)
	       (maphash
		#'(lambda (k v)
		    (push
		     (make-xml-element
		      :name :aggregate
		      :attributes `((:type . ,(symbol-name k)))
		      :children (mapcar #'event-stream-to-xml v))
		     result))
		*db*)
	       result)))
