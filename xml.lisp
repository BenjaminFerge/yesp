(defun event-stream-to-xml (event-stream)
  (make-xml-element :name :stream :attributes `((:id . ,(symbol-name (id event-stream))) (:name . ,(symbol-name (name event-stream))) (:version . ,(write-to-string (version event-stream)))) :children (mapcar #'event-to-xml (events event-stream))))

(defun event-stream-print-xml (event-stream)
  (print-xml (event-stream-to-xml event-stream) :pretty t :input-type :xml-struct))

(defun event-to-xml (e)
  (make-xml-element :name :event :attributes `((:id . ,(symbol-name (event-id e))) (:action . ,(symbol-name (event-action e))) (:version . ,(write-to-string (event-version e))) (:payload . ,(format nil "~a" (event-payload e))))))

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
