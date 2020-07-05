(in-package #:yesp)

(defparameter *db* (make-hash-table))
(defparameter *db-dir*
  (make-pathname
   :directory `(,@(pathname-directory (user-homedir-pathname))
		  ".yesp.d" "streams")))

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
