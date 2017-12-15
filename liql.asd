;;;; liql.asd

(asdf:defsystem #:liql
  :description "Liql is a DSL for generating SQL queries."
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:gadgets
               #:cl-hash-util
               #:sql-stuff
               #:alexandria
               #:cl-ascii-table
               #:split-sequence)
  :serial t
  :components ((:file "package")
               (:file "liql")))

