;; tinmop: an humble mastodon client
;; Copyright (C) 2020  cage

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.
;; If not, see [[http://www.gnu.org/licenses/][http://www.gnu.org/licenses/]].
;; derived from:

;; niccolo': a chemicals inventory
;; Copyright (C) 2016  Universita' degli Studi di Palermo

;; This  program is  free  software: you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published  by  the  Free  Software Foundation,  version  3  of  the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :db-utils)

(define-constant +db-invalid-id-number+ 0         :test #'=)

(define-constant +characters-trouble-name+ '(#\-) :test #'equalp)

(define-constant +separator-re+             "\\." :test #'equalp)

(define-constant +separator+                  "." :test #'equalp)

(define-constant +column-wildcard+            "*" :test #'equalp)

(define-constant +directive-no-journaling+      "PRAGMA journal_mode = MEMORY" :test #'string=)

(define-constant +directive-no-sync-os+         "PRAGMA synchronous = OFF"     :test #'string=)

(define-constant +directive-foreign-keys+       "PRAGMA foreign_keys = ON"     :test #'string=)

(define-constant +directive-foreign-keys-off+   "PRAGMA foreign_keys = OFF"    :test #'string=)

(define-constant +sqlite3-db-scheme-table+      :sqlite_master                 :test #'eq)

(define-constant +sqlite3-db-scheme-table-type+ "table"                        :test #'string=)

(define-constant +sqlite3-db-scheme-type+       :type                          :test #'eq)

(define-constant +sqlite3-db-scheme-table-name+ :tbl_name                      :test #'eq)

(defmacro with-disabled-foreign (&body body)
  `(unwind-protect
        (progn
          (query-low-level +directive-foreign-keys-off+)
          ,@body)
     (query-low-level +directive-foreign-keys+)))

(defparameter *connection* nil)

(defmacro with-db-transaction (&body body)
  `(sqlite:with-transaction *connection*
     ,@body))

(defun connectedp ()
  "Non nil if the connection to db is alive"
  *connection*)

(defun close-db ()
  "Close the connection to database"
  (when (connectedp)
    (sqlite:disconnect *connection*)))

(defgeneric quote-symbol (s))

(defmethod quote-symbol ((s string))
  "Quote `s' to be usable as column name in database (e.g. \"a-b\" -> \\\"a-b\\\")"
  (if (scan +separator-re+ s)
      (let* ((splitted (split +separator-re+ s))
             (res (flatten (loop for i in splitted collect
                                (if (string= i +column-wildcard+)
                                    i
                                    (format nil "\"~a\"" i))))))
        (join-with-strings res +separator+))
      (if (null (every  #'(lambda (a) (null (find a s)))
                        +characters-trouble-name+))
          (format nil "\"~(~a~)\"" s)
          (format nil "~(~a~)" s))))

(defmethod quote-symbol ((s symbol))
  (quote-symbol (symbol-name s)))

(defun prepare-query (sql)
  "Compile a query in a format suitable to be executed"
  #+debug-mode (misc:dbg "compiling ~a~%" sql)
  (sqlite:prepare-statement *connection* sql))

(defun execute-query (prepared-sql &optional (parameters nil))
  "Execute the prepared query with parameter `parameters'"
  (let* ((columns-name   (mapcar (lambda (a) (make-keyword (string-upcase a)))
                                 (sqlite:statement-column-names prepared-sql))))
    (loop
         for param in parameters
         for i from 1 do
         (sqlite:bind-parameter prepared-sql i param))
    (let ((res (loop while (sqlite:step-statement prepared-sql) collect
                    (loop
                       for i from 0
                       for column-name  in columns-name
                       append
                         (list column-name (sqlite:statement-column-value prepared-sql i))))))
      (sqlite:finalize-statement prepared-sql)
      res)))

(defun fetch-all (executed-query)
  "Fetch all rows from an executed query"
  executed-query)

(defun fetch (executed-query)
  "Fetch a single row from an executed query"
  (first executed-query))

(defun query-low-level (sql &optional (parameters nil))
  "prepare and Execute a text in SQL format"
  #+debug-mode (misc:dbg  "sql ~a parameters ~a~%" sql parameters)
  (execute-query (prepare-query sql) parameters))

(defun query (q)
  "Execute a sxql query (i.e. sql in s-expression format)"
  (multiple-value-bind (sql params)
      (sxql:yield q)
    (query-low-level sql params)))

(defun query->sql (q)
  "Convert sxql to SQL code"
  (sxql:yield q))

(defmacro do-rows ((row res) table &body body)
  "Iterate each row af a list of lists"
  `(let ((,res ,table))
     (loop for ,row from 0 below (length ,res) do ,@body)
     ,res))

(defun prepare-for-sql-like (s)
  "Prepare s as an argument for LIKE SQL clause"
  (if (not (text-utils:string-empty-p s))
      (format nil "%~a%" s)
      "%"))

(defmacro object-exists-in-db-p (table clause)
  `(fetch (query (select :*
                   (from ,table)
                   (where ,clause)))))

(defmacro object-count-in-db (table clause)
  `(second (fetch (query (select ((:count :*))
                           (from ,table)
                           (where ,clause))))))

(defgeneric db-nil-p (a)
  (:documentation "Non nil if the column can be considered a null value in lisp
example:

:nil -> T
\"false\" -> T
0         -> T
\"0\"     -> T
\"no\"    -> T
\"null\"  -> T
"))

(defmethod db-nil-p ((a null))
  t)

(defmethod db-nil-p ((a symbol))
  (eq a :nil))

(defmethod db-nil-p ((a string))
  (or (string-empty-p a)
      (string-equal a "false")
      (string-equal a "null")
      (string-equal a "nil")
      (string-equal a "no")
      (string-equal a "0")))

(defmethod db-nil-p ((a number))
  (num:epsilon= a 0.0))

(defun db-not-nil-p (a)
  (not (db-nil-p a)))

(defun db-getf (row indicator &optional (default nil))
  "Try  to  find a  value  in  a `row'  (modeled  as  a plist),  return
`default'  if  indicator has  a  value  of nil  in  row  and signal  a
`conditions:column-not-found'  if  `indicator'   does  not  exists  in
`row'."
  (let ((res (getf row indicator :not-found)))
    (cond
      ((eq res :not-found)
       (error 'conditions:column-not-found :column indicator :row row))
      ((db-nil-p res)
       default)
      (t
       res))))

(defmacro if-db-nil-else (expr else)
  `(if (not (db-nil-p ,expr))
       ,expr
       ,else))

(defun count-all (table)
  (getf (first (fetch-all (query (select ((:as (:count :*) :ct))
                                   (from table)))))
        :ct))

(defun db-path ()
  (uiop:unix-namestring (concatenate 'string
                                     (res:home-datadir)
                                     "/"
                                     +db-file+)))

(defun init-connection ()
  "Initialize a db connection (and create db file if does not exists)"
  (when (not (fs:file-exists-p (db-path)))
    (fs:create-file (db-path)))
  (setf *connection* (sqlite:connect (db-path))))

(defmacro with-ready-database ((&key (connect t)) &body body)
  "Ensure  a  valid connection  to  db  exists,  if `connect'  is  non
nil (default T), start a new connection"
  `(let ((sxql:*sql-symbol-conversion* #'db-utils:quote-symbol))
     (when ,connect
       (init-connection)
       (query-low-level +directive-no-journaling+)
       (query-low-level +directive-no-sync-os+)
       (query-low-level +directive-foreign-keys+))
     (db:maybe-build-all-tables)
     (progn ,@body)))

(defun local-time-obj-now ()
  (local-time:now))

; db -> application
(defun encode-datetime-string (d &optional (fallback nil))
  "Encode a datetime string from db"
  (handler-case
      (local-time:parse-timestring d)
     (error () fallback)))

;; application -> db
(defgeneric decode-datetime-string (object)
  (:documentation "Decode object from application to a datetime format
  suitable for database."))

(defmethod decode-datetime-string ((object (eql nil)))
  "")

(defmethod decode-datetime-string ((object local-time:timestamp))
  (local-time:format-rfc3339-timestring nil object))

(defmethod decode-datetime-string ((object string))
  (decode-datetime-string (encode-datetime-string object)))

(defmethod decode-datetime-string ((object number))
  (decode-datetime-string (universal-to-timestamp object)))

(defgeneric decode-date-string (object)
  (:documentation  "Decode object  from application  to a  date format
  suitable for database."))

(defmethod decode-date-string ((object (eql nil)))
  "")

(defmethod decode-date-string ((object local-time:timestamp))
  (local-time:format-timestring nil object :format '(:year "-" (:month 2) "-"
                                                     (:day 2))))

(defmethod decode-date-string ((object string))
  (decode-date-string (encode-datetime-string object)))

(defmethod decode-date-string ((object number))
  (decode-date-string (universal-to-timestamp object)))

(defgeneric decode-time-string (object))

(defmethod decode-time-string ((object local-time:timestamp))
  (local-time:format-timestring nil object :format '((:hour 2) ":" (:min 2))))

(defmethod decode-time-string ((object string))
  (decode-time-string (encode-datetime-string object)))

(defun encoded-datetime-year (decoded)
  (misc:extract-year-from-timestamp (encode-datetime-string decoded)))

(defmacro make-insert (table-name names values)
  "Generate an sxql insert statement

example

(make-insert :table-name
             (:col-a :col-b)
             (value-a value-b))
"
  (assert (= (length names) (length values)))
  `(insert-into ,table-name
     (set= ,@(loop
                for name in names
                for value in values append
                  (list name value)))))

(defmacro make-delete (table-name where-clause)
  "Generate an sxql delete statement

example

(make-delete :table-name
             (:col-a :col-b)
             (:and (:= col-a 1)
                   (:= col-b 2)))
"
  `(delete-from ,table-name
     (where ,where-clause)))

(defmacro make-update (table-name names values where-clause)
 "Generate an sxql update statement

example

(make-delete :table-name
             (:col-a :col-b)
             (1      2)
             (:and (:= col-a 1)
                   (:= col-b 2)))
"
  (assert (= (length names) (length values)))
  `(update ,table-name
     (set= ,@(loop
                for name in names
                for value in values append
                  (list name value)))
     (where ,where-clause)))

(defun get-max-id (table)
  (or (second (fetch (query (select (fields (:max :id)) (from table)))))

      0))

(defun get-min-id (table)
  (or (second (fetch (query (select (fields (:min :id)) (from table)))))

      0))

(defun decode-blob (blob)
  (and blob
       (base64:usb8-array-to-base64-string blob)))

(defun rows->tsv (rows)
  (with-output-to-string (stream)
    (labels ((%escape (s)
               (regex-replace-all "\"" s "\"\""))
             (%fmt (tpl &rest args)
             (apply #'format
                    stream
                    (strcat tpl (coerce '(#\return #\linefeed) 'string))
                    args))
             (%join (s)
               (join-with-strings s (string #\tab)))
             (%wrap (s)
               (wrap-with (%escape (to-s s)) "\""))
             (%filter-print (filter-fn row)
               (%join (mapcar #'%wrap
                              (remove-if-not filter-fn row))))
             (%filter-header (a)
               (and (symbolp a)
                    (not (eq :nil a))))
             (%filter-data (a)
               (cond
                 ((null a)
                  t)
                 ((and (symbolp a)
                       (not (eq :nil a)))
                   nil)
                 (t t))))
      (%fmt (%filter-print #'%filter-header (first-elt rows)))
      (loop for row in rows do
           (%fmt (%filter-print #'%filter-data row)
                 :stream stream)))))

(defun table-exists-p (table-name)
  (fetch (query (select :*
                  (from +sqlite3-db-scheme-table+)
                  (where (:and (:= +sqlite3-db-scheme-table-name+ (quote-symbol table-name))
                               (:= +sqlite3-db-scheme-type+ +sqlite3-db-scheme-table-type+)))))))

(defgeneric prepare-for-db (object &key &allow-other-keys)
  (:documentation "Prepare object to be inserted into database"))

(defmethod prepare-for-db (object &key (to-integer nil) &allow-other-keys)
  "Note that object is ignored for unspecialized method"
  (if to-integer
      1
      object))

(defmethod prepare-for-db ((object (eql t)) &key (to-integer nil) &allow-other-keys)
  "Note that object is ignored for unspecialized method"
  (declare (ignore object))
  (if to-integer
      1
      t))

(defmethod prepare-for-db ((object null)
                           &key
                             (to-integer nil)
                           &allow-other-keys)
  (declare (ignorable object))
  (if to-integer
      0
      ""))

(defmethod prepare-for-db ((object symbol) &key &allow-other-keys)
  (symbol-name object))

(defmethod prepare-for-db ((object string) &key &allow-other-keys)
  object)

(defmethod prepare-for-db ((object sequence) &key &allow-other-keys)
  (map 'list #'prepare-for-db object))

(defmethod prepare-for-db ((object local-time:timestamp) &key &allow-other-keys)
  (decode-datetime-string  object))

(defun last-inserted-rowid ()
  "Maximum value of a primary key of a table so far"
  (sqlite:last-insert-rowid *connection*))
