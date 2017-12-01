;;;; liql.asd

(asdf:defsystem #:liql
  :description "Describe liql here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:gadgets
               #:cl-hash-util
               #:sql-stuff
               #:alexandria
               #:cl-ascii-table
               #:split-sequence)
  :serial t
  :components ((:file "package")
               (:file "liql")))

