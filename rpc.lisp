(in-package #:yesp)

(defvar *xml-server* nil)

(defun start-rpc-server (port)
  (when *xml-server* (stop-rpc-server))
  (handler-case (setq *xml-server* (s-xml-rpc:start-xml-rpc-server :port port))
    (error (c)
      (format t "We caught a condition.~&")
      (values 0 c))))

(defun stop-rpc-server ()
  (stop-server *xml-server*))


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
