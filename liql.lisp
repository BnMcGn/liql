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

(defparameter *fetch-column-only* nil)

(define-condition ambiguous-symbol (error)
  ((options :initarg :options :reader options)
   (text :initarg :text :reader text)))

(define-condition record-not-found (error)
  ())

(defun splittable (symbol)
  (let ((res (split-sequence:split-sequence #\. (mkstr symbol))))
    (when (< 1 (length res))
      (sql-stuff:colm (intern (car res) 'keyword) (intern (second res) 'keyword)))))

(defun get-table-or-column-object (item)
  (if (or (stringp item) (symbolp item))
      (or (splittable item)
          (when-let ((table (first-match
                             (lambda (x) (string-equal-caseless x item))
                             (clsql-sys:database-list-tables (get-current-database)))))
            (sql-stuff:tabl (symb table)))
          (let ((cols
                 (collecting
                     (dolist (tname (clsql-sys:database-list-tables (get-current-database)))
                       (dolist (cname (sql-stuff:get-table-columns tname))
                         (when (string-equal-caseless cname item)
                           (collect (cons tname cname))))))))
            (if (< 1 (length cols))
                (error 'ambiguous-symbol :text "Multiple matching columns found"
                       :options cols)
                (sql-stuff:colm (caar cols) (cdar cols))))
          (error "No matching database item found"))
      item))

(defun column-p (t-or-c)
  (eq 'clsql-sys:sql-ident-table (type-of t-or-c)))

(defun ensure-column (t-or-c)
  "Returns an ident attribute that contains a table and a column. Given just a table, will fill with the table's pkey."
  (if (eq 'clsql-sys:sql-ident-table (type-of t-or-c))
      (multiple-value-bind (pkey sig) (sql-stuff:get-table-pkey t-or-c)
        (unless sig
          (error "Couldn't get PKey column for table specifier"))
        (sql-stuff:colm (sql-stuff:table-symbol t-or-c) (intern pkey 'keyword)))
      t-or-c))

(defun ensure-table (t-or-c)
  (if (eq 'clsql-sys:sql-ident-table (type-of t-or-c))
      t-or-c
      (sql-stuff:tabl (sql-stuff:table-symbol t-or-c))))

(defun same-table-p (spec1 spec2)
  (string-equal-caseless (sql-stuff:table-symbol spec1) (sql-stuff:table-symbol spec2)))

(defun get-current-database ()
  "Currently, this function returns a clsql database object."
  (or *last-database*
      clsql:*default-database*))

(defun %%normalize-liql (specifiers &optional results)
  "Normalize-liql is the preprocessor for liql. It should return a plist."
  (let ((sp (car specifiers)))
    (cond
      ((null sp) (nreverse results))
      ((keywordp sp)
       (multiple-value-bind (kw clause remainder)
           (%%normalize-liql-keyword sp (cdr specifiers))
         (%%normalize-liql remainder (list* clause kw results))))
      ((quoted-symbol-p sp)
       (%%normalize-liql (cdr specifiers)
                         (list* sp :designator results)))
      ((or (quoted-list-p sp) (numberp sp) (stringp sp) (symbolp sp))
       (%%normalize-liql (cdr specifiers)
                         (list* sp :input results)))
      ((listp sp)
       (%%normalize-liql (cdr specifiers)
                         (cons (%%normalize-liql-function (car sp) (cdr sp)) results)))
      (t (error "Don't handle that yet!")))))

(defun %flag-selects-in-normalized-liql (liql)
  (collecting
      ;;Step through the liql plist, converting :designator keys to :select if the
      ;;previous key was also a designator.
      (do-window ((prek prev k v) liql :size 4 :step 2 :start-padding (list nil nil))
        (declare (ignore prev))
        (if (and (eq k :designator) (eq prek :designator))
            (progn (collect :select) (collect v))
            (progn (collect k) (collect v))))))

(defun %%normalize-liql-keyword (kw remainder) (error "Not implemented"))
(defun %%normalize-liql-function (name data) (error "Not implemented"))

(defun %%parse-liql (specifiers)
  `(funcall (or *liql-finisher* (symbol-function 'summarize-core))
            ,(if specifiers
                 `(%build-liql-query
                   (%flag-selects-in-normalized-liql
                    (list ,@(%%normalize-liql specifiers))))
                 nil)))

(defun %add-table-to-query-chain (table colspec query input &key final)
  (and query input (error "Query and input parameters should not both be set."))
  (let ((where
         (cond
           (query (clsql:sql-in query))
           (input (sql-stuff:in-or-equal (ensure-column table) input))
           (t nil)))
        (columns (if (and final (not *fetch-column-only*))
                     (sql-stuff:colm '*)
                     (ensure-column (or colspec table)))))
    (apply #'clsql:sql-query
           `(,columns :from ,(ensure-table table) ,@(when where (list :where where))))))

(defun %build-liql-query (normalized-liql)
  (labels ((proc (liql database table/col query input)
             (if liql
                 (case (car liql)
                   (:input
                    (when (or table/col query input)
                      (error "Input should be first in chain!")) ;Might have meaning later?
                    (proc (cddr liql) database nil nil (second liql)))
                   (:designator
                    (if table/col
                        (if (same-table-p table/col (second liql))
                            (proc (cddr liql) database nil
                                  (%add-table-to-query-chain
                                   table/col (get-table-or-column-object (second liql))
                                   query input) nil)
                            (proc (cddr liql) database (second liql)
                                  (%add-table-to-query-chain table/col nil query input) nil))
                        (let ((desg (get-table-or-column-object (second liql))))
                          (proc (cddr liql) database desg nil input))))
                   (otherwise (error "Not implemented2")))
                 ;; Process endstate:
                 (if table/col
                     (%add-table-to-query-chain table/col nil query input :final t)
                     (or query
                         (error "Not implemented4"))))))
    (proc normalized-liql (get-current-database) nil nil nil)))

(defmacro liql (&rest specifiers)
  (%%parse-liql specifiers))

(defmacro summarize (liql-expression)
  `(let ((*liql-finisher* #'summarize-core))
     ,@liql-expression))

(defun summarize-core (query)
  (if query
      (summarize-query query)
      (summarize-database (get-current-database))))

(defun summarize-query (query)
  (multiple-value-bind (results cols) (clsql:query query)
    (let ((display (ascii-table:make-table cols))
          (count (length results)))
      (if (>= 10 count)
          (dolist (row results)
            (ascii-table:add-row display row))
          (progn
            (loop for i from 1 to 3
               for row in results
               do (ascii-table:add-row display row))
            (ascii-table:add-row
             display
             (mapcar (lambda (x) (declare (ignore x)) "...") cols))
            (dolist (row (last results 3))
              (ascii-table:add-row display row))))
      (format t "Results (~a rows):" count)
      (terpri)
      (ascii-table:display display))))

;;FIXME: Could also show a list of available DBs.
(defun summarize-database (database)
  (let ((clsql:*default-database* database))
    (format t "Database ~a" (clsql:database-name database))
    (format t "~&Tables:")
    (dolist (tablname (clsql:list-tables))
      (format t "~&   ~a" tablname))))

;;FIXME: mechanism for listing all dbs not available yet
(defun summarize-all ())

(defparameter *handle-empty-series* nil)
(defparameter *handle-empty* :error)

(eval-always
  (defmacro define-finisher (name funcname series &optional (columnar *fetch-column-only*))
    `(defmacro ,name (&body kwds-and-body)
       (let ((fail ,(if series '*handle-empty-series* '*handle-empty*))
             (fname ',funcname)
             (clum ,columnar)
             (body kwds-and-body))
         (when (eq :fail (car kwds-and-body))
           (setf fail (second kwds-and-body))
           (setf body (cddr kwds-and-body)))
         `(let
              ((*fetch-column-only* ,clum)
               (*liql-finisher*
                (lambda (query)
                  (multiple-value-bind (results cols) (clsql:query query)
                    (if results
                        (,fname results cols)
                        (if (eq :error ,fail)
                            (error 'record-not-found)
                            ,fail))))))
            ,@body)))))

(defun %grab-one (results cols)
  (values (caar results) (car cols)))

(define-finisher grab-one %grab-one nil)

(defun %grab-column (results cols)
  (values (mapcar #'car results) (car cols)))

(define-finisher grab-column %grab-column t t)

(defun %grab-records (results cols)
  (values results cols))

(define-finisher grab-records %grab-records t)

(defun %grab-record (results cols)
  (values (car results) cols))

(define-finisher grab-record %grab-record nil)

(defun %grab-plist (results cols)
  (hu:alist->plist (car (sql-stuff:assocify-results (list (car results)) cols))))

(define-finisher grab-plist %grab-plist nil)

(defun %grab-hash-table (results cols)
  (hu:alist->hash (car (sql-stuff:assocify-results (list (car results)) cols))))

(define-finisher grab-hash-table %grab-hash-table nil)

(defun %grab-alist (results cols)
  (car (sql-stuff:assocify-results (list (car results)) cols)))

(define-finisher grab-alist %grab-alist nil)
