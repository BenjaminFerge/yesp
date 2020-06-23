;;;; yesp.asd

(asdf:defsystem #:yesp
  :description "Event stream database and processor"
  :author "Benjámin J. Ferge <benjamin.ferge@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:pcl-pathnames
	       #:s-xml-rpc)
  :components ((:file "package")
               (:file "yesp")))
