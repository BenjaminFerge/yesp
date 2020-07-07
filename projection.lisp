(in-package #:yesp)

(defclass projector ()
  ((name
    :col-type (:varchar 255)
    :initarg :name
    :accessor name)
   (desc
    :col-type (:varchar 255)
    :initarg :desc
    :accessor desc)
   (body
    :col-type (or (:varchar 255) :null)
    :initarg :body
    :accessor body)
   (stream-type
    :col-type (:varchar 64)
    :initarg :stream-type
    :initform nil
    :accessor stream-type))
  (:metaclass mito:dao-table-class))

(defclass projection ()
  ((projector-id
    :col-type :integer)
   (stream-id
    :col-type (:varchar 64))
   (state
    :col-type (or (:varchar 255) :null))
   (version
    :col-type :integer))
  (:metaclass mito:dao-table-class))

(defmethod call ((p projector) &key stream-name)
  (let ((streams (gethash stream-name *db*)))
    streams))

(defparameter *sqlite-path*
  (make-pathname
   :directory `(,@(pathname-directory (user-homedir-pathname))
		  ".yesp.d")
   :name "db"
   :type "sqlite"))

(mito:connect-toplevel :sqlite3 :database-name *sqlite-path*)

(mito:ensure-table-exists 'projector)
(mito:ensure-table-exists 'projection)
