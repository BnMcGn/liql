;;;; liql.lisp

(in-package #:liql)

;;; "liql" goes here. Hacks and glory await!

;;; Liql is Linear Query Language.
;;;
;;; A hopefully simpler, more natural alternative to SQL.
;;;




(defparameter
    *liql-finisher* nil
  "Is there a processor waiting below the LIQL? If not, we need to display nicely.")

(defparameter
    *last-database* nil
  "Bookmark of the last referenced database.")

(defparameter
    *last-table* nil
  "Bookmark of the last referenced table.")

(defparameter
    *last-column* nil
  "Bookmark of the last referenced column.")







(defmacro liql (&rest specifiers)
  (parse-liql specifiers))


(defmacro summarize (liql-expression)
  `(let ((*liql-finisher* #'summarize-core))
     ,@liql-expression))

(defun summarize-core (&key database table column query)
  (if query
      (summarize-query query table column)
      (summarize-database database)))
