;;;; package.lisp

(defpackage #:liql
  (:use #:cl #:gadgets #:alexandria)
  (:export
   #:summarize
   #:liql
   #:*handle-empty*
   #:grab-one
   #:grab-column
   #:grab-records
   #:grab-record
   #:grab-plist
   #:grab-hash-table
   #:grab-alist
   #:grab-query))

