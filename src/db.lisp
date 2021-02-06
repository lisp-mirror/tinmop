;; tinmop: an humble gemini and pleroma client
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

;; derived from

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

(in-package :db)

(define-constant +make-id-autoincrement+        " id INTEGER PRIMARY KEY AUTOINCREMENT, "
  :test #'string=)

(define-constant +make-id+                      " id INTEGER PRIMARY KEY, "
  :test #'string=)

(define-constant +make-string-id+               " id TEXT PRIMARY KEY, "
  :test #'string=)

(define-constant +other-id+                     :other-id
  :test #'eq)

(define-constant +make-open+                    " ( "
  :test #'string=)

(define-constant +make-close+                   " )  "
  :test #'string=)

(define-constant +restrict+                     " RESTRICT "
  :test #'string=)

(define-constant +cascade+                      " CASCADE "
  :test #'string=)

(define-constant +col-sep+                      " , "
  :test #'string=)

(define-constant +db-true+                      1
  :test #'eq)

(define-constant +db-false+                     0
  :test #'eq)

(define-constant +table-cache+                  :cache
  :test #'eq)

(define-constant +table-status+                 :status
  :test #'eq)

(define-constant +table-crypto-data+            :crypto
  :test #'eq)

(define-constant +table-account+                :account
  :test #'eq)

(define-constant +table-poll-option+            :poll-option
  :test #'eq)

(define-constant +table-poll+                   :poll
  :test #'eq)

(define-constant +table-attachment+             :attachment
  :test #'eq)

(define-constant +table-input-history+          :input-history
  :test #'eq)

(define-constant +table-ignored-status+         :ignored-status
  :test #'eq)

(define-constant +table-skipped-status+         :skipped-status
  :test #'eq)

(define-constant +table-pagination-status+      :pagination-status
  :test #'eq)

(define-constant +table-followed-user+          :followed-user
  :test #'eq)

(define-constant +table-subscribed-tag+         :subscribed-tag
  :test #'eq)

(define-constant +table-tag-histogram+          :tag-histogram
  :test #'eq)

(define-constant +table-conversation+           :conversation
  :test #'eq)

(define-constant +table-chat+                   :chat
  :test #'eq)

(define-constant +table-chat-message+           :chat-message
  :test #'eq)

(define-constant +table-gemini-tofu-cert+       :gemini-tofu-cert
  :test #'eq)

(define-constant +table-gemini-subscription+    :gemini-subscription
  :test #'eq)

(define-constant +table-gemlog-entries+         :gemlog-entries
  :test #'eq)

(define-constant +federated-timeline+           "federated"
  :test #'string=)

(define-constant +local-timeline+               "local"
  :test #'string=)

(define-constant +home-timeline+                "home"
  :test #'string=)

(define-constant +direct-timeline+              "direct"
  :test #'string=)

(define-constant +default-status-folder+        "default"
  :test #'string=)

(define-constant +mentions-status-folder+       "mentions"
  :test #'string=)

(define-constant +default-tag-timeline+         +federated-timeline+
  :test #'string=)

(define-constant +default-converation-timeline+ +federated-timeline+
  :test #'string=)

(define-constant +hidden-recipient-prefix+      #\.
  :test #'char=)

(define-constant +default-reblogged-timeline+   ".reblogged"
  :test #'string=)

(define-constant +message-index-start+          1
  :test #'=)

(define-constant +tag-separator+                ","
  :test #'string=)

(defun default-timelines ()
  (list +home-timeline+
        +local-timeline+
        +federated-timeline+))

(defgeneric hidden-recipient-p (object))

(defmethod hidden-recipient-p ((object string))
  (char= +hidden-recipient-prefix+
         (first-elt object)))

(defun message-index->sequence-index (message-index)
  (- message-index +message-index-start+))

(defmacro gen-timeline-const->description (timeline-const-sym description)
  (let* ((const-name (symbol-name timeline-const-sym))
         (fn-prefix  (subseq const-name 1 (1- (length const-name))))
         (fn-name    (misc:format-fn-symbol t "~a->description" fn-prefix)))
    `(defun ,fn-name ()
       ,description)))

(gen-timeline-const->description +federated-timeline+ (_ "federated"))

(gen-timeline-const->description +local-timeline+     (_ "local"))

(gen-timeline-const->description +direct-timeline+    (_ "direct"))

(gen-timeline-const->description +home-timeline+      (_ "home"))

(defun timeline-type->description (key)
  (cond
    ((string= key +federated-timeline+)
     (federated-timeline->description))
    ((string= key +local-timeline+)
     (local-timeline->description))
    ((string= key +direct-timeline+)
     (direct-timeline->description))
    ((string= key +home-timeline+)
     (home-timeline->description))
    (t
     key)))

(defun create-table-index (table-name &optional (columns '(:id)))
  (labels ((%replace (s chars)
           (if (null chars)
               s
               (%replace (regex-replace-all (string (first chars)) s "_")
                         (rest chars)))))
    (let ((actual-table-name (if (symbolp table-name)
                                 (symbol-name table-name)
                                 (string-downcase table-name))))
      (query-low-level (format nil
                               "CREATE UNIQUE INDEX IF NOT EXISTS index_~a on ~a (~{~a~^, ~})"
                               (%replace actual-table-name +characters-trouble-name+)
                               (quote-symbol table-name)
                               (mapcar #'quote-symbol columns))))))

(defun delete-table (table-name)
  (query-low-level (format nil "DROP TABLE IF EXISTS ~a" (quote-symbol table-name))))

(defun delete-view (view-name)
  (query-low-level (format nil "DROP VIEW ~a" (quote-symbol view-name))))

(defun prepare-table (name &key
                             (autogenerated-id-p t)
                             (integer-id-p       nil)
                             (autoincrementp     nil))
  (let ((id-section (when autogenerated-id-p
                      (if (or autoincrementp
                              integer-id-p)
                          (if autoincrementp
                              +make-id-autoincrement+
                              +make-id+)
                          +make-string-id+))))
    (strcat "create table "
            (quote-symbol name)
            +make-open+
            id-section)))

(defun make-foreign (table column on-delete on-update &optional (add-comma nil))
  (format nil
          " REFERENCES ~a (~a) ON DELETE ~a ON UPDATE ~a ~:[ ~;,~]"
          (quote-symbol table) (quote-symbol column) on-delete on-update add-comma))

(defun make-cache ()
  (query-low-level (strcat (prepare-table +table-cache+
                                          :autogenerated-id-p t
                                          :autoincrementp     t)
                           "key              TEXT NOT NULL,"
                           "type             TEXT NOT NULL,"
                           ;; timestamp
                           " \"created-at\"  TEXT NOT NULL,"
                           ;; timestamp
                           " \"accessed-at\" TEXT NOT NULL,"
                           " UNIQUE(key) ON CONFLICT FAIL"
                           +make-close+)))

(defun make-attachment ()
  (query-low-level (strcat (prepare-table +table-attachment+ :autogenerated-id-p nil)
                           " id                    TEXT NOT NULL,"
                           ;; one of swconf:*allowed-attachment-type*
                           " type                  TEXT NOT NULL,"
                           " url                   TEXT NOT NULL,"
                           " \"preview-url\"       TEXT NOT NULL,"
                           " \"remote-url\"        TEXT,"
                           ;; url shortened
                           " \"text-url\"          TEXT,"
                           ;; metadata only for original attachment
                           " width                 TEXT,"
                           " height                TEXT,"
                           " \"frame-rate\"        TEXT,"
                           " duration              TEXT,"
                           " bitrate               TEXT,"
                           " description           TEXT,"
                           " blurhash              TEXT,"
                           " \"attached-to-id\"    TEXT"
                           +make-close+)))

(defun make-chat ()
  (query-low-level (strcat (prepare-table +table-chat+ :autogenerated-id-p nil)
                           "id                TEXT    NOT NULL,"
                           ;; timestamp
                           " \"updated-at\"   TEXT    NOT NULL,"
                           ;; timestamp
                           " \"created-at\" TEXT NOT NULL,"
                           " \"unread-count\" INTEGER DEFAULT 0,"
                           " label            TEXT    DEFAULT \"-\" ,"
                           ;; boolean
                           " ignoredp INTEGER DEFAULT 0,"

                           " \"account-id\"   TEXT    NOT NULL"
                           (make-foreign +table-account+ "id" +cascade+ +cascade+)
                           +make-close+)))

(defun make-chat-message ()
  (query-low-level (strcat (prepare-table +table-chat-message+ :autogenerated-id-p nil)
                           "id                TEXT    NOT NULL,"
                           ;; boolean
                           " unreadp INTEGER DEFAULT 1,"
                           " content          TEXT,"
                           " \"chat-id\"      TEXT, "
                           ;; timestamp
                           " \"updated-at\" TEXT,"
                           ;; timestamp
                           " \"created-at\" TEXT NOT NULL,"
                           " \"attachment-id\" TEXT,"
                           " \"account-id\"    TEXT    NOT NULL"
                           (make-foreign +table-account+ "id" +cascade+ +cascade+)
                           +make-close+)))

(defun make-conversation ()
  (query-low-level (strcat (prepare-table +table-conversation+)
                           " folder             TEXT, "
                           " \"root-status-id\" TEXT, "
                           ;; boolean
                           " ignoredp INTEGER DEFAULT 0,"
                           ;; timestamp
                           " \"created-at\" TEXT    NOT NULL,"
                           " UNIQUE(folder) ON CONFLICT FAIL"
                           +make-close+)))

(defun make-input-history ()
  (query-low-level (strcat (prepare-table +table-input-history+ :autoincrementp t)
                           " prompt                TEXT NOT NULL" +col-sep+
                           " input                 TEXT NOT NULL" +col-sep+
                           " \"date-added\"        TEXT NOT NULL"
                           +make-close+)))

(defun make-crypto-data ()
  "The data all base64 encoded"
  (query-low-level (strcat (prepare-table +table-crypto-data+ :autoincrementp t)
                           ;; the key
                           " key                   TEXT    NOT NULL"
                           +make-close+)))

(defun make-account ()
  (query-low-level (strcat (prepare-table +table-account+)
                           " username              TEXT    NOT NULL,"
                           ;; this is the actual user identification
                           " acct                  TEXT    NOT NULL,"
                           ;; profile homepage
                           " url                   TEXT    NOT NULL,"
                           " \"display-name\"      TEXT    NOT NULL,"
                           ;; bio
                           " note                  TEXT    NOT NULL,"
                           ;; url
                           " avatar                TEXT    NOT NULL,"
                           ;; the same as avatar if avatar is *not* an animated gif
                           " \"avatar-static\"     TEXT    NOT NULL,"
                           ;; url of banner profile
                           " header                TEXT    NOT NULL,"
                           ;; the same as header if header is *not* an animated gif
                           " \"header-static\"     TEXT    NOT NULL,"
                           ;; boolean
                           " discoverable          integer default 0,"
                           ;; boolean
                           " locked                integer default 0,"
                           ;; timestamp
                           " \"created-at\"        TEXT    NOT NULL,"
                           " \"statuses-count\"    INTEGER NOT NULL,"
                           " \"followers-count\"   INTEGER NOT NULL,"
                           " \"following-count\"   INTEGER NOT NULL,"
                           " \"moved-id\"          TEXT ,"
                           ;; boolean
                           " botp                  integer default 0,"
                           ;; local value
                           " \"encryption-key-id\" INTEGER "
                           (make-foreign +table-crypto-data+ "id" +cascade+ +cascade+) +col-sep+
                           ;; boolean
                           " ignoredp              INTEGER default 0, "
                           " UNIQUE(id) ON CONFLICT FAIL"
                           +make-close+)))

(defun make-followed-user ()
  (query-low-level (strcat (prepare-table +table-followed-user+ :integer-id-p t :autoincrementp t)
                           " \"user-id\" TEXT "
                           (make-foreign +table-account+ "id" +cascade+ +cascade+) +col-sep+
                           ;; timestamp
                           " \"created-at\"       TEXT    NOT NULL"
                           +make-close+)))

(defun make-poll-option ()
  (query-low-level (strcat (prepare-table +table-poll-option+
                                          :autogenerated-id-p t
                                          :autoincrementp     t)
                           " \"poll-id\" TEXT NOT NULL "
                           (make-foreign +table-poll+ "id" +cascade+ +cascade+) +col-sep+
                           " title TEXT, "
                           " \"votes-count\" INTEGER DEFAULT 0"
                           +make-close+)))

(defun make-poll ()
  (query-low-level (strcat (prepare-table +table-poll+ :autogenerated-id-p t)
                           " \"status-id\" TEXT NOT NULL "
                           ;(make-foreign +table-status+ "status-id" +cascade+ +cascade+)
                           +col-sep+
                           ;; date
                           " \"expire-date\"  TEXT    NOT NULL,"
                           ;; boolean
                           " expired          INTEGER DEFAULT 0 ,"
                           ;; boolean
                           " multiple         INTEGER DEFAULT 0 ,"
                           " \"voters-count\" INTEGER DEFAULT 0 ,"
                           " \"votes-count\"  INTEGER DEFAULT 0 ,"
                           ;; boolean
                           " \"voted\"        INTEGER DEFAULT 0 ,"
                           ;; comma separated values
                           " \"own-votes\"    TEXT "
                           +make-close+)))

(defun make-status ()
  (query-low-level (strcat (prepare-table +table-status+ :autogenerated-id-p nil)
                           " \"status-id\"        TEXT    NOT NULL, "
                           " \"account-id\"       TEXT    NOT NULL "
                           (make-foreign +table-account+ "id" +cascade+ +cascade+) +col-sep+
                           " uri                  TEXT    NOT NULL,"
                           ;; timestamp
                           " \"created-at\"       TEXT    NOT NULL,"
                           ;; actual message (In HTML)
                           " content              TEXT    NOT NULL,"
                           ;; output message to  display when redraft,
                           ;; probably useless to us
                           " text                 TEXT   ,"
                           ;; actual message to display in tinmop, used also for searching
                           " \"rendered-text\"    TEXT    ,"
                           ;; one of swconf:*allowed-status-visibility*
                           " \"visibility\"       TEXT   NOT NULL,"
                           ;; boolean,
                           " sensitive            integer default 0 ,"
                           ;; can value "" (empty string) but if not can be used as message subject?
                           " \"spoiler-text\"     TEXT    ,"
                           " \"reblogs-count\"    INTEGER default 0,"
                           " \"favourites-count\" INTEGER default 0,"
                           " \"replies-count\"    INTEGER default 0,"
                           ;; nullables
                           " url                          TEXT,"
                           " \"in-reply-to-id\"           TEXT,"
                           " \"in-reply-to-account-id\"   TEXT,"
                           ;; boosted
                           " \"reblog-id\"                INTEGER, "
                           ;; two letter iso code (it, en etc.)
                           " language                     TEXT,"
                           ;; user action on this status
                           ;; boolean
                           " favourited                   integer default 0,"
                           ;; boosted, boolean
                           " reblogged                    integer default 0,"
                           ;; boolean
                           " muted                        integer default 0,"
                           ;; boolean
                           " bookmarked                   integer default 0,"
                           ;; boolean
                           " pinned                       TEXT,"
                           ;; comma separated values
                           " tags                         TEXT, "
                           " application                  TEXT, "
                           ;; local value
                           ;; has been red? boolean
                           " redp                         INTEGER DEFAULT 0, "
                           ;; has the user marked this status for deletion? boolean
                           " deletedp                     INTEGER DEFAULT 0, "
                           ;; with timeline this status belong to
                           ;; must be one of:
                           ;; +federated-timeline+
                           ;; +local-timeline+
                           ;; +direct-timeline+
                           " timeline                     TEXT    NOT NULL, "
                           " folder TEXT NOT NULL DEFAULT \"" +default-status-folder+ "\"" +col-sep+
                           ;; used  in  thread  window  to address  the  message  in  a
                           ;; comfortable way for humans :)
                           " \"message-index\"            INTEGER DEFAULT 1, "
                           " UNIQUE (folder, timeline, \"status-id\") ON CONFLICT FAIL"
                           +make-close+)))

(defun make-subscribed-tag ()
  (query-low-level (strcat (prepare-table +table-subscribed-tag+)
                           " \"last-status-id-fetched\" TEXT, "
                           ;; boolean
                           " \"got-new-message-p\"  INTEGER DEFAULT 0, "
                           ;; timestamp
                           " \"created-at\"       TEXT    NOT NULL"
                           +make-close+)))

(defun make-tag-histogram ()
  (query-low-level (strcat (prepare-table +table-tag-histogram+
                                          :integer-id-p   t
                                          :autoincrementp t)
                           " \"tag\"   TEXT NOT NULL, "
                           " \"count\" INTEGER DEFAULT 0, "
                           ;; date
                           " \"day\"   TEXT    NOT NULL"
                           +make-close+)))

(defun make-ignored-status ()
  (query-low-level (strcat (prepare-table +table-ignored-status+ :autoincrementp t)
                           " \"status-id\" TEXT NOT NULL, "
                           " timeline      TEXT NOT NULL, "
                           " folder        TEXT NOT NULL, "
                           ;; timestamp
                           " \"created-at\"       TEXT    NOT NULL"
                           +make-close+)))

(defun make-skipped-status ()
  (query-low-level (strcat (prepare-table +table-skipped-status+ :autoincrementp t)
                           " \"status-id\" TEXT NOT NULL, "
                           " timeline      TEXT NOT NULL, "
                           " folder        TEXT NOT NULL, "
                           ;; timestamp
                           " \"created-at\"       TEXT    NOT NULL"
                           +make-close+)))

(defun make-pagination-status ()
  (query-low-level (strcat (prepare-table +table-pagination-status+ :autoincrementp t)
                           " \"status-id\" TEXT NOT NULL, "
                           " timeline      TEXT NOT NULL, "
                           " folder        TEXT NOT NULL  "
                           +make-close+)))

(defun make-tofu-certs ()
  (query-low-level (strcat (prepare-table +table-gemini-tofu-cert+ :autoincrementp t)
                           " host TEXT NOT NULL, "
                           " hash TEXT NOT NULL, "
                           ;; timestamp
                           " \"seen-at\" TEXT    NOT NULL,"
                           " UNIQUE(host) ON CONFLICT FAIL"
                           +make-close+)))

(defun make-gemini-subscription ()
  (query-low-level (strcat (prepare-table +table-gemini-subscription+
                                          :autoincrementp nil
                                          :autogenerated-id-p nil)
                           " url TEXT PRIMARY KEY, "
                           " title TEXT, "
                           " subtitle TEXT "
                           +make-close+)))

(defun make-gemlog-entries ()
  (query-low-level (strcat (prepare-table +table-gemlog-entries+
                                          :autoincrementp nil
                                          :autogenerated-id-p nil)
                           " url TEXT PRIMARY KEY, "
                           " \"gemlog-id\" TEXT NON NULL "
                           (make-foreign +table-gemini-subscription+
                                         :url
                                         +cascade+
                                         +cascade+
                                         t)
                           ;; timestamp
                           " date TEXT NOT NULL, "
                           " title TEXT, "
                           " snippet TEXT, "
                           ;; boolean
                           " seenp INTEGER DEFAULT 0, "
                           " deletedp INTEGER DEFAULT 0, "
                           " UNIQUE(url) ON CONFLICT FAIL"
                           +make-close+)))

(defun build-all-indices ()
  (create-table-index +table-status+              '(:folder :timeline :status-id))
  (create-table-index +table-account+             '(:id :acct))
  (create-table-index +table-followed-user+       '(:user-id))
  (create-table-index +table-subscribed-tag+      '(:id))
  (create-table-index +table-ignored-status+      '(:folder :timeline :status-id))
  (create-table-index +table-skipped-status+      '(:folder :timeline :status-id))
  (create-table-index +table-pagination-status+   '(:folder :timeline :status-id))
  (create-table-index +table-conversation+        '(:id))
  (create-table-index +table-cache+               '(:id :key))
  (create-table-index +table-gemini-tofu-cert+    '(:host))
  (create-table-index +table-gemini-subscription+ '(:url))
  (create-table-index +table-gemlog-entries+      '(:url)))

(defmacro gen-delete (suffix &rest names)
  `(progn
     ,@(loop for name in names collect
            `(,(misc:format-fn-symbol t "delete-~a" suffix) ,name))))

(defun delete-all-tables ()
  (gen-delete table
              +table-cache+
              +table-input-history+
              +table-status+
              +table-account+
              +table-followed-user+
              +table-subscribed-tag+
              +table-tag-histogram+
              +table-attachment+
              +table-conversation+
              +table-pagination-status+
              +table-ignored-status+
              +table-skipped-status+
              +table-poll-option+
              +table-poll+
              +table-chat-message+
              +table-chat+
              +table-gemini-tofu-cert+
              +table-gemini-subscription+
              +table-gemlog-entries+))

(defun build-views ())

(defun delete-all-views ())

(defun delete-database ()
   (with-disabled-foreign
     (delete-all-views)
     (delete-all-tables)))

(defun maybe-build-all-tables ()
  (when (= (fs:file-size (db-path))
           0)
    (make-cache)
    (make-input-history)
    (make-crypto-data)
    (make-account)
    (make-followed-user)
    (make-status)
    (make-ignored-status)
    (make-skipped-status)
    (make-attachment)
    (make-subscribed-tag)
    (make-tag-histogram)
    (make-conversation)
    (make-pagination-status)
    (make-poll-option)
    (make-poll)
    (make-chat-message)
    (make-chat)
    (make-tofu-certs)
    (make-gemini-subscription)
    (make-gemlog-entries)
    (build-all-indices)
    (fs:set-file-permissions (db-path) (logior fs:+s-irusr+ fs:+s-iwusr+))))

;; specific utils

(defun table->alist (table col)
  (let ((all (fetch-all (query (select (:id col)
                                 (from table)
                                 (order-by col))))))
    (loop for i in all collect
         (cons (getf i :id)
               (getf i col)))))

(defgeneric fetch-single (query)
  (:documentation "Fetch the first row from the results of exectuting `query'"))

(defmethod fetch-single (query)
  (fetch (query query)))

(defmethod fetch-single ((query string))
  (fetch (query-low-level query nil)))

(defgeneric fetch-all-rows (query)
  (:documentation "Fetch all rows from the results of exectuting `sql'"))

(defmethod fetch-all-rows (sql)
  (fetch-all (query sql)))

(defmethod fetch-all-rows ((sql string))
  (fetch-all (query-low-level sql nil)))

(defun fetch-from-id (table id)
  "Select a row from a `table' by column named `:id' with value `id'"
   (fetch-single (select :*
                   (from table)
                   (where (:= :id id)))))

(defun create-view (name select-query)
  (query-low-level (format nil "create view ~a as ~a"
                           (quote-symbol name)
                           (query->sql select-query))))

(defun delete-by-id (table id)
  "Delete a row from a `table' by column named `:id' with value `id'"
  (query (delete-from table (where (:= :id id)))))

(defun account-ignored-p (account-id)
  "Returns non nil if this account has been setted as ignored by the user"
  (let* ((db-account-row  (fetch-from-id :account account-id))
         (account-known-p db-account-row))
    (and account-known-p
         (db-getf db-account-row
                  :ignoredp
                  :default nil))))

(defun user-ignored-p (account-id)
  "Returns non nil if this account must be ignored"
  (or (db:account-ignored-p account-id)
      (when-let ((ignore-regexps (swconf:ignore-users-regexps))
                 (username (db:user-id->username account-id)))
        (loop for ignore-re in ignore-regexps do
             (when (cl-ppcre:scan ignore-re username)
               (return-from user-ignored-p t)))
        nil)))

(defun acct->user (acct)
  "Convert `acct' (acct is synonyym  for username in mastodon account)
to the corresponding row in table +table-account+"
  (fetch-single (select :*
                  (from :account)
                  (where (:= :acct acct)))))

(defun acct->id (acct)
  "Convert `acct' (acct is synonyym  for username in mastodon account)
to the corresponding id in table +table-account+"
  (db-getf (acct->user acct) :id))

(misc:defalias username->id #'acct->id)

(defun user-exists-p (username)
  (acct->user username))

(defun user-id->user (id)
  (fetch-from-id :account id))

(defun user-id->username (user-id)
  "username or acct are synonyms"
  (when-let ((user (user-id->user user-id)))
    (db-getf user :acct)))

(defun last-in-history (prompt)
  (let* ((query (select (:*
                         (:as (fields (:max :id)) :max))
                  (from :input-history)
                  (where (:= :prompt prompt)))))
    (fetch-single query)))

(defun insert-in-history (prompt input)
  "insert an history entry with `prompt` and `input'"
  (when (string-not-empty-p input)
    (let* ((last-inserted (last-in-history prompt)))
      (when (or (null last-inserted)
                (not (string= input (getf last-inserted :input))))
        (let* ((now           (prepare-for-db (local-time-obj-now)))
               (insert-query  (make-insert :input-history
                                           (:prompt :input :date-added)
                                           (prompt input   now))))
          (query insert-query))))))

(defun next-in-history (min-id prompt)
  "Return the history entry with prompt `prompt` and id that is greater
than `min-id'"
  (let* ((query (select (:id
                         (:as (fields (:min :id)) :min)
                         :prompt
                         :input)
                  (from :input-history)
                  (where (:and (:> :id min-id)
                               (:= :prompt prompt)))))
         (row    (fetch-single query)))
    (and (second row)
         (values (getf row :min)
                 (getf row :input)))))

(defun previous-in-history (max-id prompt)
  "Return the history entry with prompt `prompt` and id that is smaller
than `max-id'"
  (let* ((query (select (:id
                         (:as (fields (:max :id)) :max)
                         :prompt
                         :input)
                  (from :input-history)
                  (where (:and (:< :id max-id)
                               (:= :prompt prompt)))))
         (row   (fetch-single query)))
    (and (second row)
         (values (getf row :max)
                 (getf row :input)))))

(defun most-recent-history-id (prompt)
  "The most recent history entry with prompt `prompt'"
  (let* ((query  (select (fields (:max :id))
                   (from :input-history)
                   (where (:= :prompt prompt))))
         (row    (fetch-single query)))
    (or (second row)
        0)))

(defun threshold-time (days-in-the-past)
  "Returns a time object `days-in-the-past' days in the past"
  (local-time:adjust-timestamp (local-time-obj-now)
    (offset :day (- (abs days-in-the-past)))))

(defun purge-history ()
  "Remove expired entry in history.

An         entry          is         expired          if         older
than (swconf:config-purge-history-days-offset) days in the past"
  (let ((treshold (threshold-time (swconf:config-purge-history-days-offset))))
    (query (make-delete +table-input-history+
                        (:< :date-added (prepare-for-db treshold))))))

(defun history-prompt->values (prompt)
  (mapcar #'second
          (query (select :input
                   (from +table-input-history+)
                   (where (:= :prompt prompt))))))

(defmethod prepare-for-db ((object tooter:application) &key &allow-other-keys)
  (tooter:name object))

(defgeneric update-db (object &key &allow-other-keys)
  (:documentation "Save object in database"))

(defmethod update-db ((object sequence) &key &allow-other-keys)
  (map 'list #'update-db object))

(defmacro with-no-row-id ((table id) &body body)
  "Execute `body' only if in table `table' an object with ID `id' does not exists"
  `(when (null (fetch-from-id ,table ,id))
     ,@body))

(defmacro gen-insert-and-update-query ((insert-query-name
                                        update-query-name
                                        table
                                        keys
                                        values
                                        &key (where nil))
                                       &body body)
  `(let ((,insert-query-name (make-insert ,table ,keys ,values))
         (,update-query-name (make-update ,table ,keys ,values ,where)))
     ,@body))

(defgeneric metadata-width      (object)
  (:documentation "attachment metadata width"))

(defgeneric metadata-height     (object)
  (:documentation "attachment metadata height"))

(defgeneric metadata-frame-rate (object)
  (:documentation "attachment metadata frame rate"))

(defgeneric metadata-duration   (object)
  (:documentation "attachment metadata duration"))

(defgeneric metadata-bitrate    (object)
  (:documentation "attachment metadata bitrate"))

(defgeneric metadata-original    (object)
  (:documentation "attachment metadata original metadata"))

(defmethod metadata-width ((object tooter:image-metadata))
  (and object
       (tooter:width object)))

(defmethod metadata-height ((object tooter:image-metadata))
  (and object
       (tooter:height object)))

(defmethod metadata-width ((object tooter:video-metadata))
  (and object
       (tooter:width object)))

(defmethod metadata-height ((object tooter:video-metadata))
  (and object
       (tooter:height object)))

(defmethod metadata-frame-rate (object)
  (declare (ignore object))
  nil)

(defmethod metadata-duration (object)
  (declare (ignore object))
  nil)

(defmethod metadata-bitrate (object)
  (declare (ignore object))
  nil)

(defmethod metadata-width (object)
  (declare (ignore object))
  nil)

(defmethod metadata-height (object)
  (declare (ignore object))
  nil)

(defmethod metadata-frame-rate ((object tooter:video-metadata))
  (and object
       (tooter:frame-rate object)))

(defmethod metadata-duration ((object tooter:video-metadata))
  (and object
       (tooter:duration object)))

(defmethod metadata-duration ((object tooter:audio-metadata))
  (and object
       (tooter:duration object)))

(defmethod metadata-bitrate ((object tooter:video-metadata))
  (and object
       (tooter:bitrate object)))

(defmethod metadata-bitrate ((object tooter:audio-metadata))
  (and object
       (tooter:audio-bitrate object)))

(defmethod metadata-original (object)
  (and object
       (tooter:original object)))

(defun find-poll-option (poll-id title)
  (fetch-single (select :*
                  (from +table-poll-option+)
                  (where (:and (:= :title     title)
                               (:= :poll-id poll-id))))))

(defun poll-option-exists-p (poll-id title)
  (find-poll-option poll-id title))

(defun all-poll-options (poll-id)
  (fetch-all-rows (select :*
                    (from +table-poll-option+)
                    (where (:= :poll-id poll-id)))))

(defun find-poll (poll-id)
  (fetch-from-id +table-poll+ poll-id))

(defun find-poll-bound-to-status (status-id)
  (fetch-single (select :*
                  (from +table-poll+)
                  (where (:= :status-id status-id)))))

(defun poll-bound-to-status-exists-p (status-id)
  (find-poll-bound-to-status status-id))

(defmethod update-db ((object tooter:poll-option) &key (poll-id nil) &allow-other-keys)
  (assert poll-id)
  (with-accessors ((title       tooter:title)
                   (votes-count tooter:votes-count)) object
    (let ((insert-query (make-insert +table-poll-option+
                                     (:title :votes-count :poll-id)
                                     (title  votes-count  poll-id)))
          (update-query (make-update +table-poll-option+
                                     (:votes-count)
                                     (votes-count)
                                     (:and (:= :title     title)
                                           (:= :poll-id poll-id)))))
      (if (poll-option-exists-p poll-id title)
          (query update-query)
          (query insert-query)))))

(defmethod update-db ((object tooter:poll) &key (status-id nil) &allow-other-keys)
  (assert status-id)
  (with-accessors ((id           tooter:id)
                   (expires-at   tooter:expires-at)
                   (expired      tooter:expired)
                   (multiple     tooter:multiple)
                   (voters-count tooter:voters-count)
                   (votes-count  tooter:votes-count)
                   (voted        tooter:voted)
                   (own-votes    tooter:own-votes)
                   (options      tooter:options)) object
    (let* ((expire-date      (decode-datetime-string expires-at))
           (actual-expired   (prepare-for-db expired  :to-integer t))
           (actual-multiple  (prepare-for-db multiple :to-integer t))
           (actual-voted     (prepare-for-db voted :to-integer t))
           (actual-own-votes (join-with-strings (if own-votes
                                                    (mapcar #'to-s own-votes)
                                                    "")
                                                +tag-separator+))
           (insert-query (make-insert +table-poll+
                                      (:id
                                       :status-id
                                       :expire-date
                                       :expired
                                       :multiple
                                       :voters-count
                                       :votes-count
                                       :voted
                                       :own-votes)
                                      (id
                                       status-id
                                       expire-date
                                       actual-expired
                                       actual-multiple
                                       voters-count
                                       votes-count
                                       actual-voted
                                       actual-own-votes))))
      (when (not (poll-bound-to-status-exists-p status-id))
        (query insert-query))
      (loop for option in options do
           (update-db option :poll-id id)))))

(defmethod update-db ((object tooter:attachment) &key (attached-to-id nil) &allow-other-keys)
  (with-accessors ((id  tooter:id)
                   (kind        tooter:kind)
                   (url         tooter:url)
                   (preview-url tooter:preview-url)
                   (remote-url  tooter:remote-url)
                   (text-url    tooter:text-url)
                   (metadata    tooter:metadata)
                   (description tooter:description)
                   (blurhash    tooter:blurhash)) object
    (assert attached-to-id)
    (let* ((actual-attachment-type (prepare-for-db kind))
           (original-file-metadata (metadata-original metadata))
           (width                  (prepare-for-db (metadata-width      original-file-metadata)))
           (height                 (prepare-for-db (metadata-height     original-file-metadata)))
           (frame-rate             (prepare-for-db (metadata-frame-rate original-file-metadata)))
           (duration               (prepare-for-db (metadata-duration   original-file-metadata)))
           (bitrate                (prepare-for-db (metadata-bitrate    original-file-metadata)))
           (insert-query (make-insert +table-attachment+
                                      (:id
                                       :type
                                       :url
                                       :preview-url
                                       :remote-url
                                       :text-url
                                       :width
                                       :height
                                       :frame-rate
                                       :duration
                                       :bitrate
                                       :description
                                       :blurhash
                                       :attached-to-id)
                                      (id
                                       actual-attachment-type
                                       url
                                       preview-url
                                       remote-url
                                       text-url
                                       width
                                       height
                                       frame-rate
                                       duration
                                       bitrate
                                       description
                                       blurhash
                                       attached-to-id)))
           (attachment-exists-p (fetch-single (select :*
                                                 (from +table-attachment+)
                                                 (where (:and (:= :attached-to-id
                                                                  attached-to-id)
                                                              (:= :id
                                                                  id)))))))
      (when (not attachment-exists-p)
        (query insert-query)))))

(defmacro insert-or-update (table keys values)
  "Anaphoric `id'"
  (with-gensyms (insert-query update-query)
    `(gen-insert-and-update-query (,insert-query
                                  ,update-query
                                  ,table
                                  ,keys
                                  ,values
                                  :where (:= :id id))
       (if (fetch-from-id ,table id)
           (query ,update-query)
           (query ,insert-query)))))

(defmethod update-db ((object tooter:account) &key &allow-other-keys)
  (with-accessors ((id               tooter:id)
                   (username         tooter:username)
                   (account-name     tooter:account-name)
                   (url              tooter:url)
                   (display-name     tooter:display-name)
                   (note             tooter:note)
                   (avatar           tooter:avatar)
                   (avatar-static    tooter:avatar-static)
                   (header           tooter:header)
                   (header-static    tooter:header-static)
                   (locked           tooter:locked)
                   (discoverable     tooter:discoverable)
                   (created-at       tooter:created-at)
                   (followers-count  tooter:followers-count)
                   (following-count  tooter:following-count)
                   (statuses-count   tooter:statuses-count)
                   (moved            tooter:moved)
                   (bot              tooter:bot)) object
    (let ((actual-created-at   (decode-datetime-string created-at))
          (actual-botp         (prepare-for-db bot          :to-integer t))
          (actual-discoverable (prepare-for-db discoverable :to-integer t))
          (actual-locked       (prepare-for-db locked       :to-integer t))
          (actual-moved-id     (if moved
                                   (prepare-for-db (tooter:id moved))
                                   (prepare-for-db nil))))
      (complete:initialize-complete-username-cache)
      (insert-or-update +table-account+
                        (:id
                         :username
                         :acct
                         :url
                         :display-name
                         :note
                         :avatar
                         :avatar-static
                         :header
                         :header-static
                         :locked
                         :discoverable
                         :created-at
                         :followers-count
                         :following-count
                         :statuses-count
                         :moved-id
                         :botp)
                        (id
                         username
                         account-name
                         url
                         display-name
                         note
                         avatar
                         avatar-static
                         header
                         header-static
                         actual-locked
                         actual-discoverable
                         actual-created-at
                         followers-count
                         following-count
                         statuses-count
                         actual-moved-id
                         actual-botp)))))

(defmethod update-db ((object tooter:tag-history) &key (tag nil) &allow-other-keys)
  (assert (stringp tag))
  (with-accessors ((day       tooter:day)
                   (use-count tooter:use-count)) object
    (let* ((actual-day     (decode-date-string day))
           (entry-exists-p (query (select :*
                                    (from +table-tag-histogram+)
                                    (where (:and (:= :day  actual-day)
                                                 (:= :tag  tag))))))
           (updatable-p    (query (select :*
                                    (from +table-tag-histogram+)
                                    (where (:and (:= :day  actual-day)
                                                 (:= :tag   tag)
                                                 (:> :count use-count)))))))
      (cond
        ((not entry-exists-p)
         (query (make-insert +table-tag-histogram+
                             (:tag :day       :count)
                             (tag  actual-day use-count))))
        (updatable-p
         (query (make-update +table-tag-histogram+
                             (:count)
                             (use-count)
                             (:and (:= :day  actual-day)
                                   (:= :tag  tag)))))))))

(defmethod update-db ((object tooter:status)
                      &key
                        (timeline +local-timeline+)
                        (folder   +default-status-folder+)
                        (skip-ignored-p nil)
                        &allow-other-keys)
  (with-accessors ((id                     tooter:id)
                   (uri                    tooter:uri)
                   (created-at             tooter:created-at)
                   (content                tooter:content)
                   (visibility             tooter:visibility)
                   (sensitive              tooter:sensitive)
                   (spoiler-text           tooter:spoiler-text)
                   (reblogs-count          tooter:reblogs-count)
                   (favourites-count       tooter:favourites-count)
                   (url                    tooter:url)
                   (in-reply-to-id         tooter:in-reply-to-id)
                   (in-reply-to-account-id tooter:in-reply-to-account-id)
                   (language               tooter:language)
                   (favourited             tooter:favourited)
                   (reblogged              tooter:reblogged)
                   (parent                 tooter:parent)
                   (muted                  tooter:muted)
                   (pinned                 tooter:pinned)
                   (account                tooter:account)
                   (tags                   tooter:tags)
                   (application            tooter:application)
                   (media-attachments      tooter:media-attachments)
                   (poll                   tooter:poll)) object
    (update-db account)
    (let* ((account-id         (tooter:id account))
           (actual-created-at  (decode-datetime-string created-at))
           (actual-application (prepare-for-db application))
           (tag-names          (if tags
                                   (mapcar #'client:tag-name tags)
                                   ""))
           (actual-tags        (join-with-strings tag-names
                                                  +tag-separator+))
           (actual-language    (prepare-for-db language))
           ;; use string-downcase as a workaround because tooter return an upcased keyword
           (actual-visibility  (string-downcase (prepare-for-db visibility)))
           (actual-sensitive   (prepare-for-db sensitive  :to-integer t))
           (actual-favourited  (prepare-for-db favourited :to-integer t))
           (actual-pinned      (prepare-for-db pinned     :to-integer t))
           (actual-reblogged   (prepare-for-db reblogged  :to-integer t))
           (actual-muted       (prepare-for-db muted      :to-integer t))
           (rendered-text      (msg-utils:message-original->text-body content
                                                                      :try-decrypt nil))
           (reblog-id          (if parent
                                   (prepare-for-db (tooter:id parent))
                                   (prepare-for-db nil)))
           (account-ignored-p  (user-ignored-p account-id))
           (status-ignored-p   (status-ignored-p id folder timeline)))
      (when (not (and skip-ignored-p
                      (or status-ignored-p
                          account-ignored-p)))
        (let ((insert-query (make-insert +table-status+
                                         (:status-id
                                          :account-id
                                          :uri
                                          :created-at
                                          :content
                                          :rendered-text
                                          :visibility
                                          :sensitive
                                          :spoiler-text
                                          :reblogs-count
                                          :favourites-count
                                          :url
                                          :in-reply-to-id
                                          :in-reply-to-account-id
                                          :reblog-id
                                          :language
                                          :favourited
                                          :reblogged
                                          :muted
                                          :pinned
                                          :timeline
                                          :tags
                                          :application
                                          :folder)
                                         (id
                                          account-id
                                          uri
                                          actual-created-at
                                          content
                                          rendered-text
                                          actual-visibility
                                          actual-sensitive
                                          spoiler-text
                                          reblogs-count
                                          favourites-count
                                          url
                                          in-reply-to-id
                                          in-reply-to-account-id
                                          reblog-id
                                          actual-language
                                          actual-favourited
                                          actual-reblogged
                                          actual-muted
                                          actual-pinned
                                          timeline
                                          actual-tags
                                          actual-application
                                          folder))))
          (when (not (single-status-exists-p id timeline folder))
            (query insert-query)
            ;; attachments, tag history latest because of the
            ;; reference from this table to table status
            (map nil
                 (lambda (media-attachment)
                   (update-db media-attachment :attached-to-id id))
                 media-attachments)
            (loop
               for tag      in tags
               for tag-name in tag-names do
                 (let ((tag-history (or (tooter:history tag)
                                        (api-client:make-placeholder-tag-histogram))))
                   (update-db tag-history :tag tag-name)))
            (update-db parent
                       :skip-ignored-p skip-ignored-p
                       :timeline       +default-reblogged-timeline+)
            ;; now try to decrypt message if possible/needed
            (maybe-decrypt-update-status-text id timeline folder)
            (let ((db-status (find-status-id-folder-timeline id folder timeline)))
              (hooks:run-hook-compose 'hooks:*after-saving-message* db-status)))
          ;; add poll or update poll's votes
          (when poll
            (update-db poll :status-id id)))))))

(defun find-chat (chat-id)
  (fetch-single (select :*
                  (from +table-chat+)
                  (where (:= :id chat-id)))))

(defun chat-message-exists-p (chat-id message-id)
  (query (select :*
           (from +table-chat-message+)
           (where (:and (:= :chat-id    chat-id)
                        (:= :message-id message-id))))))

(defun mark-all-chat-messages-read (chat-id)
  (query (make-update +table-chat-message+
                      (:unreadp)
                      (+db-false+)
                      (:= :chat-id chat-id))))

(defun count-unread-chat-messages (chat-id)
  (second (fetch-single (select (fields (:count :id))
                          (from +table-chat-message+)
                          (where (:and (:= :chat-id chat-id)
                                       (:= :unreadp +db-true+)))))))

(defmethod update-db ((object api-pleroma:chat-message) &key &allow-other-keys)
  (with-accessors ((message-id api-pleroma:message-id)
                   (emojis     api-pleroma:emojis)
                   (updated-at api-pleroma:updated-at)
                   (created-at api-pleroma:created-at)
                   (content    api-pleroma:content)
                   (chat-id    api-pleroma:chat-id)
                   (attachment api-pleroma:attachment)
                   (account-id api-pleroma:account-id)) object
    (when (and (user-id->user account-id)
               (not (chat-message-exists-p chat-id message-id)))
      (update-db attachment :attached-to-id message-id)
      (let ((attachment-id     (and attachment
                                    (tooter:id attachment)))
            (actual-updated-at (decode-datetime-string updated-at))
            (actual-created-at (decode-datetime-string created-at)))
        (query (make-insert +table-chat-message+
                            (:id
                             :content
                             :chat-id
                             :attachment-id
                             :account-id
                             :updated-at
                             :created-at)
                            (message-id
                             content
                             chat-id
                             attachment-id
                             account-id
                             actual-updated-at
                             actual-created-at)))))))

(defun chat-change-label (chat-id label)
  (assert (stringp chat-id))
  (assert (stringp label))
  (assert (chat-exists-p chat-id))
  (query (make-update +table-chat+
                      (:label)
                      (label)
                      (:= :id chat-id))))

(defun chat-exists-p (chat-id)
  (query (select :*
           (from +table-chat+)
           (where (:= :id chat-id)))))

(defun all-chats ()
  "Return all chats ordered by most recent updated to last recent updated"
  (query (select :*
                 (from +table-chat+)
                 (order-by (:desc :updated-at)))))

(defun all-chat-messages (chat-id)
  "Return all messages belonging  to `chat-id' ordered by `id'
in ascending order"
  (query (select :*
           (from +table-chat-message+)
           (where (:= :chat-id chat-id))
           (order-by (:asc :id)))))

(defun all-chat-links (chat-id)
  "Return all links belonging  to `chat-id' ordered by message `id'
in ascending order"
  (let ((all (query (select ((:as :attachment.text-url :url))
                      (from :attachment)
                      (join :chat-message :on (:and (:= :chat-message.attachment-id
                                                        :attachment.id)
                                              (:not-null :chat-message.attachment-id)))
                      (where (:= :chat-message.chat-id chat-id))
                      (order-by (:asc :chat-message.id))))))
    (remove-duplicates (mapcar #'second all) :test #'string=)))

(defun last-chat-message-id (chat-id)
  (second (fetch-single (select ((:as (fields (:max :id)) :max-id))
                          (from +table-chat-message+)
                          (where (:= :chat-id chat-id))))))

(defmethod update-db ((object api-pleroma:chat) &key &allow-other-keys)
  (with-accessors ((chat-id    api-pleroma:chat-id)
                   (updated-at api-pleroma:updated-at)
                   (created-at api-pleroma:created-at)
                   (account    api-pleroma:account)) object
    (when (not (chat-exists-p chat-id))
      (let ((actual-updated-at (decode-datetime-string updated-at))
            (actual-created-at (decode-datetime-string created-at)))
        (update-db account)
        (query (make-insert +table-chat+
                            (:id
                             :account-id
                             :updated-at
                             :created-at)
                            (chat-id
                             (tooter:id account)
                             actual-updated-at
                             actual-created-at)))))))

(defun maybe-decrypt-update-status-text (status-id timeline folder)
  "Decrypt, if possible, status identified by `status-id', `timeline' and `folder'.

Update database with the decrypted text in column `rendered-text'"
  (when-let* ((status    (fetch-single (make-filtered-message-select nil
                                                                     timeline
                                                                     folder
                                                                     nil
                                                                     `(:= :status-id ,status-id))))
              (raw-text  (row-message-rendered-text status))
              (decrypted (msg-utils:message-original->text-body status
                                                                :notify-cant-decrypt t
                                                                :try-decrypt         t)))
    (query (make-update +table-status+
                        (:rendered-text)
                        (decrypted)
                        (:and (:= :status-id status-id)
                              (:= :folder    folder)
                              (:= :timeline  timeline))))))

(defun message-root (timeline folder status-id)
  "Return the root of the status identified by 'status-id'.
   If  'status-id'  does not  belong  to  a  reply return  the  status
   identitfied by 'status-id'.   If parent of status  so identified is
   not in the database return (values status :partial)"
  (labels ((get-status (id)
             (and id
                  (message-from-timeline-folder-id timeline folder id)))
           (climb-tree (parent child reply-id)
             (if (null parent)
                 (if reply-id
                     (values child :partial)
                     (values child nil))
                 (let* ((reply-id     (getf parent :in-reply-to-id))
                        (grand-parent (get-status reply-id)))
                   (climb-tree grand-parent parent reply-id)))))
    (climb-tree (get-status status-id) nil nil)))

(defun all-root-statuses (timeline-type &key (folder +default-status-folder+))
  (assert folder)
  (assert timeline-type)
  (let* ((query-no-reply   (select :*
                           (from :status)
                           (where (:and (:= :timeline timeline-type)
                                        (:= :folder   folder)
                                        (:is-null     :in-reply-to-id)))))
         (query-with-reply (select :*
                           (from :status)
                           (where (:and (:= :timeline timeline-type)
                                        (:= :folder   folder)
                                        (:not-null     :in-reply-to-id)))))
         (complete-tree    (fetch-all-rows query-no-reply))
         (orphan           (remove-if (lambda (row)
                                      (let ((id-reply  (row-message-reply-to-id row))
                                            (folder    (row-message-folder      row))
                                            (timeline  (row-message-timeline    row)))
                                        (message-from-timeline-folder-id timeline
                                                                         folder
                                                                         id-reply)))
                                    (fetch-all-rows query-with-reply))))
    (values (append complete-tree orphan)
            complete-tree
            orphan)))

(defun all-root-status-id (timeline-type
                             &key
                               (sort-fn nil)
                               (folder +default-status-folder+))
  (let ((ids (mapcar #'second (all-root-statuses timeline-type :folder folder))))
    (if sort-fn
        (sort ids sort-fn)
        ids)))

(defun message-from-timeline-folder-id (timeline folder status-id)
  "Returns   a   message   identified   by   `status-id',   `timeline'
and`folder'. A `message' is a plist that contains all the informations
of  a row  of table  +table-status+ and  at least  the columns  :acct,
:display-name and :locked from +table-account+"
  (fetch-single (make-filtered-message-select nil
                                              nil
                                              folder
                                              nil
                                              `(:and := :status.timeline  ,timeline)
                                              `(:and := :status.status-id ,status-id))))

(defun message-from-timeline-folder-message-index (timeline folder message-index)
  "Returns   a   message   identified   by   `status-id',   `timeline'
and `message-index'. A `message' is a plist that contains all the informations
of  a row  of table  +table-status+ and  at least  the columns  :acct,
:display-name and :locked from +table-account+.

Message index is an unique number  that identify the message after the
messages are sorted as below:

1. start with global message-index = 1

1. sort all the trees in a folder from the one with the older root to the newest

2. for each tree explore the messages starting from root

   a. visit  each node in the  tree with a classic  Depth First Search
   and set the value index for that node as the value of message-index

   b. increment message-index by 1
"
  (let ((query (make-filtered-message-select nil
                                             timeline
                                             folder
                                             nil
                                             `(:= :status.message-index ,message-index))))
    (fetch-single query)))

(defun message-index->tree (timeline folder message-index)
    "Returns   a  tree of  messages   identified   by   `status-id',   `timeline'
and `message-index'.

Message index is  an unique number that identify  the message."
  (let ((message (message-from-timeline-folder-message-index timeline folder message-index)))
    (assert message)
    (let ((message-status-id  (row-message-status-id message)))
      (message-root->tree timeline
                          folder
                          (row-message-status-id (message-root timeline
                                                               folder
                                                               message-status-id))))))

(defun find-status-id (status-id)
  "Find  a  status  by id,  notes  that  status  id  is not  a  unique
identifier despite the name."
  (fetch-single (select :*
                  (from :status)
                  (where (:= :status-id status-id)))))

(defun find-message-id (status-id)
  "Find a message  (status with other columns like acct)  by id, notes
that status id is not a unique identifier despite the name."
  (fetch-single (make-filtered-message-select nil
                                              nil
                                              nil
                                              nil
                                              `(:= :status-id ,status-id))))

(defun find-status-id-folder-timeline (status-id folder timeline)
  "Fetch a single message identified by `status-id', `folder' and `timeline'.

Note that the tuple (`status-id', `folder' and `timeline') is the only key
that identify a single message in table :status"
  (fetch-single (select :* (from +table-status+)
                        (where (:and (:= :status-id status-id)
                                     (:= :timeline  timeline)
                                     (:= :folder    folder))))))

(defmacro gen-message-select ()
  "Convenience macro for `make-filtered-message-select'"
  (let ((select `(select (:status.*
                          (:as :account.acct         :username)
                          (:as :account.display-name :display-name)
                          (:as :account.locked :locked))
                   (from :status)
                   (join :account :on (:= :account.id
                                          :status.account-id)))))
    select))

(defun make-filtered-message-select (other-columns
                                     timeline
                                     folder
                                     account-id
                                     &rest
                                       where-clauses)
  "Query the table status

- other-column fetch other column from  table :status or :account (the
  default columns  are all the  ones of  table status and  the columns
  :acct, :display-name and :locked  from +table-account+, use `nil' if
  you are OK with the defaults

- timeline
  folder
  account-id

  use nil if you do not want to filter with this additional criteria

- where-clause  a number of sxql where clause for even more filtering.  E.g:

  `(:= :status.timeline ,timeline)
  `(:= :in-reply-to-id  ,status-id)

   the two cluasuses will be connectd by :AND by default

  `(:or :like :spoiler-text
        ,actual-text-looking-for)
  `(:or :like :tags
        ,actual-text-looking-for)
  `(:or :like :username
         ,actual-text-looking-for)
  `(:and :>   :status.message-index
         ,start-status-message-index)

  note that the order matters in fact the following clauses

  `(:and :>   :status.message-index
         ,start-status-message-index)
  `(:or :like :spoiler-text
        ,actual-text-looking-for)
  `(:or :like :tags
        ,actual-text-looking-for)
  `(:or :like :username
         ,actual-text-looking-for)

  is not equivalent to the one below, the latter means

  (:status.message-index > start-status-message-index) OR  ...

  and does not means:

  (:status.message-index > start-status-message-index) AND ...

  as was likely intended."
  (let ((query (gen-message-select)))
    (loop for other-column in other-columns do
         (fields+ query other-column))
    (loop for where-clause in where-clauses do
         (cond
           ((eq (first where-clause)
                :and)
            (and-where query (rest where-clause)))
           ((eq (first where-clause)
                :or)
            (or-where query (rest where-clause)))
           (t
            (and-where query where-clause))))
    (when folder
      (and-where query `(:= :folder ,folder)))
    (when timeline
      (and-where query `(:= :timeline ,timeline)))
    (when account-id
      (and-where query `(:= :account-id ,account-id)))
    query))

(defun single-status-exists-p (status-id timeline folder)
  "Id timeline and  folder is the tuple that is primary key for table
:status"
  (find-status-id-folder-timeline status-id folder timeline))

(defun message-children (timeline folder status-id)
  "Return the direct children of this status, nil if there are none"
  (assert (stringp status-id))
  (when-let* ((parent-status (message-from-timeline-folder-id timeline folder status-id))
              (query         (make-filtered-message-select nil
                                                           nil
                                                           folder
                                                           nil
                                                           `(:= :status.timeline ,timeline)
                                                           `(:= :in-reply-to-id  ,status-id))))
    (fetch-all-rows query)))

(defun message-id->tree (timeline folder status-id)
  "Return an instance of  `mtree-utils:m-tree' filled with status that
forms a messages thread identified by the arguments."
  (message-root->tree timeline
                      folder
                      (row-message-status-id (message-root timeline folder status-id))))

(defun message-root->tree (timeline folder root-status-id)
  "Return an instance of  `mtree-utils:m-tree' filled with status that
forms a messages thread"
  (assert folder)
  (assert (stringp root-status-id))
  (when-let* ((root-status (message-from-timeline-folder-id timeline folder root-status-id))
              (results     (mtree:make-node root-status)))
    (labels ((add-children (node)
               (let ((children (message-children timeline folder
                                                 (row-message-status-id (mtree:data node)))))
                 (loop for child in children do
                      (mtree:add-child node
                                       (mtree:make-node child)))
                 (mtree:do-children (child node)
                   (add-children child))
                 node)))
      (add-children results))))

(defun message->thread-users (timeline folder status-id
                              &key
                                (local-name-prefix "")
                                (acct-prefix       ""))
  "Given a tuple that identify a message (`timeline' `folder' `status-id'),
returns an alist of (local-username . acct)."
  (let ((all-messages (mtree:collect-nodes-data (message-id->tree timeline folder status-id)))
        (results      ()))
    (loop for message in all-messages do
         (let* ((user-id    (db-getf message :account-id))
                (account    (user-id->user user-id))
                (local-name (db-getf account :username))
                (username   (user-id->username user-id))
                (pair       (cons (strcat local-name-prefix local-name)
                                  (strcat acct-prefix        username))))
           (pushnew pair results :test (lambda (a b) (and (string= (car a)
                                                                   (car b))
                                                          (string= (cdr a)
                                                                   (cdr b)))))))
    results))

(defun mention-local->global-alist ()
  "Returns an alist of all known acoounts as ('@'local-username . '@'acct)."
  (let* ((query (select (:username :acct) (from +table-account+)))
         (rows  (fetch-all-rows query)))
    (loop for row in rows collect
         (let ((local-name (db-getf row :username))
               (username   (db-getf row :acct)))
         (cons (msg-utils:add-mention-prefix local-name)
               (msg-utils:add-mention-prefix username))))))

(defmacro gen-access-message-row (name column
                                  &key
                                    (default nil)
                                    (only-empty-or-0-are-null nil))
  "Convenience macro to generate function to access a value of a table
row."
  `(defun ,(misc:format-fn-symbol t "row-~a" name) (row)
       (and row
            (db-getf row
                     ,column
                     :default ,default
                     :only-empty-or-0-are-null ,only-empty-or-0-are-null))))

(gen-access-message-row id                          :id)

(gen-access-message-row message-visibility          :visibility)

(gen-access-message-row message-status-id           :status-id)

(gen-access-message-row message-index               :message-index)

(gen-access-message-row message-folder              :folder)

(gen-access-message-row message-timeline            :timeline)

(gen-access-message-row message-username            :username)

(gen-access-message-row message-user-display-name   :display-name)

(gen-access-message-row message-content             :content)

(gen-access-message-row message-rendered-text       :rendered-text)

(gen-access-message-row message-creation-time       :created-at)

(gen-access-message-row message-subject             :spoiler-text)

(gen-access-message-row message-tags                :tags)

(gen-access-message-row message-reblog-id           :reblog-id)

(gen-access-message-row lockedp                     :locked)

(gen-access-message-row message-redp                :redp)

(gen-access-message-row user-username               :acct)

(gen-access-message-row tag-got-new-message         :got-new-message-p)

(gen-access-message-row conversation-folder         :folder)

(gen-access-message-row conversation-ignored-p      :ignoredp)

(gen-access-message-row conversation-root-status-id :root-status-id)

(gen-access-message-row poll-expired-p              :expired)

(gen-access-message-row poll-multiple-vote-p        :multiple)

(gen-access-message-row title                       :title :only-empty-or-0-are-null t)

(gen-access-message-row subtitle                    :subtitle :only-empty-or-0-are-null t)

(gen-access-message-row url                         :url)

(gen-access-message-row expire-date                 :expire-date)

(gen-access-message-row chat-id                     :chat-id)

(gen-access-message-row account-id                  :account-id)

(gen-access-message-row updated-at                  :updated-at)

(gen-access-message-row created-at                  :created-at)

(gen-access-message-row text-url                    :text-url)

(gen-access-message-row type                        :type)

(gen-access-message-row label                       :label)

(gen-access-message-row cache-key                   :key)

(gen-access-message-row cache-type                  :type)

(gen-access-message-row cache-accessed-at           :accessed-at)

(gen-access-message-row cache-created-at            :created-at)

(gen-access-message-row seenp                       :seenp)

(defun row-votes-count (row)
  (and row (db-getf row :votes-count :default 0)))

(defun row-message-reply-to-id (row)
  (and row
       (db-getf row :in-reply-to-id)))

(defun tree-data-id (tree-node)
  "Return  the `data'  slot of  node `tree-node'.  the argument  is an
instance of `mtree-utils:m-tree'"
  (row-message-status-id (mtree:data tree-node)))

(defun neighbor-tree (tree timeline-type &key (folder +default-status-folder+))
  "Unused"
  (when-let* ((all-roots-id (all-root-status-id timeline-type
                                                :sort-fn #'string<
                                                :folder  folder))
              (needle-root  (mtree:root-node tree))
              (needle-id    (tree-data-id needle-root))
              (needle-pos   (position needle-id all-roots-id :test #'string=)))
    (flet ((neighbor-ids ()
             (cond
               ((= needle-pos 0)
                (values nil (elt all-roots-id (1+ needle-pos))))
               ((= needle-pos (1- (length all-roots-id)))
                (values (elt all-roots-id (1- needle-pos))
                        nil))
               (t
                (values (elt all-roots-id (1- needle-pos))
                        (elt all-roots-id (1+ needle-pos)))))))
      (multiple-value-bind (previous-id next-id)
          (neighbor-ids)
        (values (and previous-id
                     (message-id->tree timeline-type folder previous-id))
                (and next-id
                     (message-id->tree timeline-type folder next-id)))))))

(defun next-status-tree (tree timeline-type &key (folder +default-status-folder+))
  "Unused"
  (multiple-value-bind (x next)
      (neighbor-tree tree timeline-type :folder folder)
    (declare (ignore x))
    next))

(defun previous-status-tree (tree timeline-type &key (folder +default-status-folder+))
  "Unused"
  (multiple-value-bind (previous x)
      (neighbor-tree tree timeline-type :folder folder)
    (declare (ignore x))
    previous))

(defun message-tree-root-equal (a b)
  (string= (tree-data-id (mtree:root-node a))
           (tree-data-id (mtree:root-node b))))

(defun all-status-trees (timeline folder)
  "Returns all the trees (instances of `mtree-utils:m-tree') belonging
to `timeline' and `folder'"
  (labels ((tree= (a b)
             (message-tree-root-equal a b)))
    (let ((res ())
          (status-ids (mapcar #'second (fetch-all-rows (select :status-id
                                                  (from :status)
                                                  (where (:folder folder))
                                                  (order-by (:asc :status-id)))))))
      (loop for status-id in status-ids do
           (let* ((status-id-root (row-message-status-id (message-root timeline
                                                                       folder
                                                                       status-id)))
                  (tree    (message-id->tree timeline folder status-id-root)))
             (pushnew tree res :test #'tree=)))
      (sort res
            (lambda (a b)
              (let* ((root-a (mtree:root-node a))
                     (root-b (mtree:root-node b))
                     (status-id-a   (row-message-status-id (mtree:data root-a)))
                     (status-id-b   (row-message-status-id (mtree:data root-b))))
                (string< status-id-a status-id-b)))))))

(defun fetch-status-trees (timeline folder &key (account-id nil))
  "Returns all the trees (instances of `mtree-utils:m-tree') belonging
to `timeline', `folder' and `account-id'"
  (labels ((tree= (a b)
             (string= (tree-data-id (mtree:root-node a))
                      (tree-data-id (mtree:root-node b)))))
    (let* ((res   ())
           (query (select :status-id
                    (from :status)
                    (where (:and (:= :folder     folder)
                                 (:= :timeline   timeline)))
                    (order-by (:asc :status-id)))))
      (when account-id
        (and-where query `(:= :account-id ,account-id)))
      (let* ((rows       (fetch-all-rows query))
             (status-ids (mapcar #'second rows)))
        (loop for status-id in status-ids do
             (let* ((status-id-root (row-message-status-id (message-root timeline
                                                                         folder
                                                                         status-id)))
                    (tree    (message-id->tree timeline folder status-id-root)))
               (pushnew tree res :test #'tree=)))
        (sort res
              (lambda (a b)
                (let* ((root-a (mtree:root-node a))
                       (root-b (mtree:root-node b))
                       (status-id-a   (row-message-status-id (mtree:data root-a)))
                       (status-id-b   (row-message-status-id (mtree:data root-b))))
                  (string< status-id-a status-id-b))))))))

(defun annotated-tree-line->data-plist (line)
  "See `mtree-utils:tree->annotated-lines"
  (rest (last-elt line)))

(defun renumber-timeline-message-index (timeline-type folder &key (account-id nil))
 "Add a  unique numeric index to each message thei is an unique ID
inside `timeline' and `folder'.

`account-id' additional  restrict the message to be processed to the
ones of a single author

Message index is an unique number  that identify the message after the
messages are sorted as below:

1. start with global message-index = 1

1. sort all the trees in a folder from the one with the older root to the newest

2. for each tree explore the messages starting from root

   a. visit  each node in the  tree with a classic  Depth First Search
   and set the value index for that node as the value of message-index

   b. increment message-index by 1"
  (let ((all-trees (remove-if-not (lambda (tree)
                                    (string= (db-getf (mtree:data tree) :timeline)
                                             timeline-type))
                                  (fetch-status-trees timeline-type
                                                      folder
                                                      :account-id account-id)))
        (new-index +message-index-start+))
    (loop for tree in all-trees do
         (let ((tree-lines (mtree:tree->annotated-lines tree
                                                        :print-data t
                                                        :print-data-fn #'identity)))
           (loop for line in tree-lines do
                (let* ((status-id    (row-message-status-id (annotated-tree-line->data-plist line)))
                       (query-update (update :status
                                       (set= :message-index new-index)
                                       (where (:and (:= :status-id status-id)
                                                    (:= :folder    folder)
                                                    (:= :timeline timeline-type))))))
                  (query query-update)
                  (incf new-index)))))))

(defun all-folders ()
  (let ((query (select (fields (:distinct :folder))
                       (from  :status))))
    (mapcar #'second
            (fetch-all-rows query))))

(defun all-status-timelines ()
  (mapcar #'second
          (fetch-all-rows (select (fields (:distinct :timeline)) (from :status)))))

(defun renumber-all-timelines ()
  (let ((all-folders   (all-folders))
        (all-timelines (all-status-timelines)))
    (loop for folder in all-folders do
         (loop for timeline in all-timelines do
              (renumber-timeline-message-index timeline folder :account-id nil)))))

(defun all-attachments-to-status (status-id)
  (fetch-all-rows (select :*
                    (from +table-attachment+)
                    (where (:= :attached-to-id status-id)))))

(defun attachment-to-chat-message (chat-message-id)
  (fetch-single (select :*
                  (from +table-attachment+)
                  (where (:= :attached-to-id chat-message-id)))))

(defun status->reblogged-status (wrapper-status-id)
  "Return   the status that identified by `wrapper-status-id'
reblogged (if exists)."
  (when-let* ((wrapper-status      (find-status-id wrapper-status-id))
              (reblogged-status-id (row-message-reblog-id wrapper-status)))
    (find-status-id reblogged-status-id)))

(defun all-attachments-urls-to-status (status-id &key (add-reblogged-urls nil))
  "Returns  all the attachments to status identified by `status-id'
  and (if `add-reblogged-urls' is non nil) reblogged status (if exists)"
  (let* ((res                 (mapcar (lambda (a) (db-getf a :url))
                                      (all-attachments-to-status status-id)))
         (reblogged-status    (status->reblogged-status status-id)))
    (when (and reblogged-status
               add-reblogged-urls)
      (setf res
            (append res
                    (all-attachments-urls-to-status (row-message-status-id reblogged-status)
                                                    :add-reblogged-urls add-reblogged-urls))))
    (remove-duplicates res :test #'string=)))

(defun debug-print-all-tree (timeline-type)
  (let ((all-trees  (remove-if-not (lambda (tree)
                                     (string= (db-getf (mtree:data tree) :timeline)
                                              timeline-type))
                                   (all-status-trees timeline-type +default-status-folder+))))
    (loop for tree in all-trees do
         (format t "tree:~%~{~a~%~}~%"
                 (mtree:tree->annotated-lines tree
                                              :print-data t
                                              :print-data-fn
                                              (lambda (a)
                                                (strcat
                                                 (db-getf a :status-id)
                                                 " "
                                                 (to-s (db-getf a :message-index)))))))))

(defun mark-status-boolean-value (timeline folder status-id column value)
  "Convenience function to set a boolean value for a single column of table status"
  (assert (numberp value))
  (assert (or (= value +db-true+)
              (= value +db-false+)))
  (let ((query (update :status
                 (set= column value)
                  (where (:and (:= :timeline timeline)
                               (:= :folder folder)
                               (:= :status-id status-id))))))
    (query query)))

(defun mark-status-red-p (timeline folder status-id)
  (mark-status-boolean-value timeline folder status-id :redp +db-true+))

(defun mark-status-unread (timeline folder status-id)
  (mark-status-boolean-value timeline folder status-id :redp +db-false+))

(defun mark-status-deleted-p (timeline folder status-id)
  "Mark status as need to be deleted."
  (mark-status-boolean-value timeline folder status-id :deletedp +db-true+))

(defun mark-status-prevent-deletion (timeline folder status-id)
  "Remove mark of status as need to be deleted."
  (mark-status-boolean-value timeline folder status-id :deletedp +db-false+))

(defun count-status-redp (timeline folder &key (account-id nil))
  (let ((query (select (fields (:count :*))
                       (from :status)
                       (where (:and (:= :folder     folder)
                                    (:= :timeline   timeline)
                                    (:= :redp       +db-true+))))))
    (when account-id
      (and-where query `(:= :account-id ,account-id)))
    (second (fetch-single query))))

(defun count-status (timeline folder &key (account-id nil))
  (let ((query   (select (fields (:count :*))
                   (from :status)
                   (where (:and (:= :folder     folder)
                                (:= :timeline   timeline))))))
    (when account-id
      (and-where query `(:= :account-id ,account-id)))
    (second (fetch-single query))))

(defun search-messages-text-body (timeline folder text-looking-for &key (account-id nil))
  "Search for `text-looking-for' inside the body of messages belonging to
`timeline' , `folder' and possibly `account-id'"
  (let* ((actual-text-looking-for (prepare-for-sql-like text-looking-for))
         (query (make-filtered-message-select nil
                                              timeline
                                              folder
                                              account-id
                                              `(:and :like :rendered-text
                                                     ,actual-text-looking-for))))
    (order-by= query :message-index)
    (fetch-all-rows query)))

(defun search-next-message-body (timeline
                                 folder
                                 text-looking-for
                                 start-status-message-index
                                 &key (account-id nil))
  "Search for `text-looking-for' inside the body of messages belonging
to  `timeline' ,  `folder'  and possibly  `account-id', newer  than
`start-status-message-index'"
  (let* ((actual-text-looking-for (prepare-for-sql-like text-looking-for))
         (query (make-filtered-message-select nil
                                              timeline
                                              folder
                                              account-id
                                              `(:and :like :rendered-text
                                                     ,actual-text-looking-for)
                                              `(:and :> :status.message-index
                                                     ,start-status-message-index))))
    (order-by= query :message-index)
    (fetch-single query)))

(defun search-previous-message-body (timeline
                                     folder
                                     text-looking-for
                                     start-status-message-index
                                     &key (account-id nil))
  "Search for `text-looking-for' inside the body of messages belonging
to  `timeline' ,  `folder'  and possibly  `account-id', older  than
`start-status-message-index'"
  (let* ((actual-text-looking-for (prepare-for-sql-like text-looking-for))
         (query (make-filtered-message-select nil
                                              timeline
                                              folder
                                              account-id
                                              `(:and :like :rendered-text
                                                     ,actual-text-looking-for)
                                              `(:and :< :status.message-index
                                                     ,start-status-message-index))))
    (order-by= query '(:desc :message-index))
    (fetch-single query)))

(defun search-next-message-meta (timeline
                                 folder
                                 text-looking-for
                                 start-status-message-index
                                 &key (account-id nil))
  "Search for `text-looking-for' inside the metadata of messages belonging
to  `timeline' ,  `folder'  and possibly  `account-id', newer  than
`start-status-message-index'

Metadata are:

- spoiler-text (subject of message)
- tags
- username"
  (let* ((actual-text-looking-for (prepare-for-sql-like text-looking-for))
         (query (make-filtered-message-select nil
                                              timeline
                                              folder
                                              account-id
                                              `(:or :like :spoiler-text
                                                     ,actual-text-looking-for)
                                              `(:or :like :tags
                                                    ,actual-text-looking-for)
                                              `(:or :like :username
                                                     ,actual-text-looking-for)
                                              `(:and :>   :status.message-index
                                                     ,start-status-message-index))))
    (order-by= query :message-index)
    (fetch-single query)))

(defun search-previous-message-meta (timeline
                                     folder
                                     text-looking-for
                                     start-status-message-index
                                     &key (account-id nil))
    "Search for `text-looking-for' inside the metadata of messages belonging
to  `timeline' ,  `folder'  and possibly  `account-id', older  than
`start-status-message-index'

Metadata are:

- spoiler-text (subject of message)
- tags
- username"
  (let* ((actual-text-looking-for (prepare-for-sql-like text-looking-for))
         (query (make-filtered-message-select nil
                                              timeline
                                              folder
                                              account-id
                                              `(:or :like :username
                                                    ,actual-text-looking-for)
                                              `(:or :like :spoiler-text
                                                    ,actual-text-looking-for)
                                              `(:or :like :tags
                                                    ,actual-text-looking-for)
                                              `(:and :<   :status.message-index
                                                     ,start-status-message-index))))
    (order-by= query '(:desc :message-index))
    (fetch-single query)))

(defun search-next-unread-message (timeline
                                   folder
                                   start-status-message-index
                                   &key (account-id nil))
    "Search the next unread message belonging
to  `timeline' ,  `folder'  and possibly  `account-id', older  than
`start-status-message-index'"
  (let* ((query (make-filtered-message-select nil
                                              timeline
                                              folder
                                              account-id
                                              `(:and :> :status.message-index
                                                     ,start-status-message-index)
                                              `(:and := :status.redp
                                                     ,+db-false+))))
    (order-by= query :message-index)
    (fetch-single query)))

(defmacro with-add-account-id-to-query ((query query-body) account-id &body body)
  `(let ((,query ,query-body))
     (when ,account-id
       (and-where ,query `(:= :account-id ,,account-id)))
     ,@body))

(defun add-where-timeline-folder (query timeline folder)
  (and-where query `(:= :timeline ,timeline))
  (and-where query `(:= :folder   ,folder)))

(defun last-message-index-status (timeline-type folder &key (account-id nil))
  (with-add-account-id-to-query
      (query (select ((:as (fields (:max :message-index)) :max))
               (from :status)
               (where (:and (:= :timeline timeline-type)
                            (:= :folder   folder)))))
      account-id
    (when-let ((row (fetch-single query)))
      (second row))))

(defun last-status-id-timeline-folder-table (timeline folder table)
  (let ((query (select ((:as (fields (:max :status-id)) :max))
                 (from table)
                 (where (:and (:= :timeline timeline)
                              (:= :folder   folder))))))
    (second (fetch-single query))))

(defun first-status-id-timeline-folder-table (timeline folder table)
  (let ((query (select ((:as (fields (:min :status-id)) :min))
                 (from table)
                 (where (:and (:= :timeline timeline)
                              (:= :folder   folder))))))
    (second (fetch-single query))))

(defun last-status-id-timeline-folder (timeline folder)
  (last-status-id-timeline-folder-table timeline folder :status))

(defun first-status-id-timeline-folder (timeline folder)
  (first-status-id-timeline-folder-table timeline folder :status))

(defun last-ignored-status-id-timeline-folder (timeline folder)
  (last-status-id-timeline-folder-table timeline folder :ignored-status))

(defun first-ignored-status-id-timeline-folder (timeline folder)
  (first-status-id-timeline-folder-table timeline folder :ignored-status))

(defun last-pagination-status-id-timeline-folder (timeline folder)
  (last-status-id-timeline-folder-table timeline folder :pagination-status))

(defun first-pagination-status-id-timeline-folder (timeline folder)
  (first-status-id-timeline-folder-table timeline folder :pagination-status))

(defun find-pagination-status (status-id folder timeline)
  (fetch-single (select :*
                  (from +table-pagination-status+)
                  (where (:and (:= :status-id status-id)
                               (:= :folder    folder)
                               (:= :timeline  timeline))))))

(defun add-to-pagination-status (status-id folder timeline &key (ensure-no-duplicates nil))
  (let ((no-duplicate-p (if ensure-no-duplicates
                            (not (find-pagination-status status-id folder timeline))
                            t)))
    (when no-duplicate-p
      (query (make-insert +table-pagination-status+
                          (:status-id :folder :timeline)
                          (status-id  folder  timeline))))))

(defun remove-pagination-status (folder timeline)
  "Removes  all  the pagination  data  (i.e.  all columns  from  table
:pagination-data) matching `folder' and `timeline'"
  (query (make-delete +table-pagination-status+
                      (:and (:= :folder   folder)
                            (:= :timeline timeline)))))

(defun delete-status (timeline-type folder status-id)
  "delete status and connect its children with their grandparent"
  (let* ((status           (find-status-id-folder-timeline status-id
                                                           folder
                                                           timeline-type))
         (parent-status-id (db-getf status :in-reply-to-id))
         (children         (message-children timeline-type folder status-id))
         (query-delete     (delete-from :status
                             (where (:and (:= :timeline  timeline-type)
                                          (:= :folder    folder)
                                          (:= :status-id status-id))))))
    (with-db-transaction
      (query query-delete)
      (loop for child in children do
           (query (make-update :status
                               (:in-reply-to-id)
                               (parent-status-id)
                               (:and (:= :timeline  timeline-type)
                                     (:= :folder    folder)
                                     (:= :status-id (row-message-status-id child)))))))))

(defun count-status-marked-to-delete ()
  (second (fetch-single (select (fields (:count :status-id))
                          (from :status)
                          (where (:= :deletedp +db-true+))))))

(defun statuses-id-marked-to-delete (timeline folder)
   (query (select :status-id
            (from :status)
            (where (:and (:= :deletedp +db-true+)
                         (:= :timeline timeline)
                         (:= :folder   folder))))))

(defun delete-all-statuses-marked-deleted ()
  (let ((all-folders   (all-folders))
        (all-timelines (all-status-timelines)))
    (loop for folder in all-folders do
         (loop for timeline in all-timelines do
              (let ((marked-to-delete (statuses-id-marked-to-delete timeline folder)))
                (loop for status-to-delete in marked-to-delete do
                     (delete-status timeline folder (row-message-status-id status-to-delete))))))))

(defun max-username-length (timeline-type folder)
  (let ((query (select (fields (:max (:length :account.acct)))
                 (from :status)
                 (join :account :on (:= :account.id
                                        :status.account-id))
                 (where (:and (:= :timeline timeline-type)
                              (:= :folder   folder))))))
    (second (fetch-single query))))

(defgeneric keyword->dbcolumn (object))

(defmethod keyword->dbcolumn ((object symbol))
  (string-downcase (symbol-name object)))

(defmethod keyword->dbcolumn ((object string))
  object)

(defun folder-exists-p (folder)
  (fetch-all-rows (select :*
                    (from :status)
                    (where (:= :folder folder)))))

(defun timeline-exists-p (folder timeline)
  (fetch-all-rows (select :*
                    (from :status)
                    (where (:and (:= :folder   folder)
                                 (:= :timeline timeline))))))

(defun move-message-to-folder (timeline folder status-id destination-folder)
  (let ((message-exists-p (message-from-timeline-folder-id timeline folder status-id)))
    (query (make-update :status
                        (:folder)
                        (destination-folder)
                        (:and (:= :status-id status-id)
                              (:= :timeline  timeline)
                              (:= :folder    folder))))
    message-exists-p))

(defun move-tree-to-folder (timeline folder message-index destination-folder)
  "Move the tree of messages (identified by `timeline', `folder' and `message-index' to
`destination-folder'"
  (let ((tree (message-index->tree timeline folder message-index)))
    (mtree:top-down-visit tree
                          (lambda (node)
                            (let ((id (row-message-status-id (mtree:data node))))
                              (move-message-to-folder timeline folder id destination-folder))))
    (renumber-timeline-message-index timeline folder)
    (renumber-timeline-message-index timeline destination-folder)))

(defun last-status-id-in-tree (tree)
  "Returns   the   newest   message   in  `tree'   (an   instance   of
`mtree-utils:mtree')"
  (let ((maximum ""))
    (mtree:top-down-visit tree
                          (lambda (node)
                            (let ((id (row-message-status-id (mtree:data node))))
                              (when (string> id maximum)
                                (setf maximum id)))))
    maximum))

(defun all-timelines-in-folder (folder &key (include-default-timelines nil))
  (assert folder)
  (let* ((query   (select (fields (:distinct :timeline))
                    (from  :status)
                    (where (:= :folder folder))))
         (dynamic (mapcar #'second
                          (fetch-all-rows query))))
    (if include-default-timelines
        (union (default-timelines)
               dynamic
               :test #'string=)
        dynamic)))

(defun set-ignore-status-author (status-id new-value)
  "Ignore  or  unignore  the  future   statuses  authored  by  the  user
identified  by  the  account  that  wrote  the  status  identified  by
`status-id'"
  (when-let* ((status     (find-status-id status-id))
              (account-id (db-getf status :account-id)))
    (query (make-update :account
                        (:ignoredp)
                        ((prepare-for-db new-value :to-integer t))
                        (:and (:= :id       account-id))))))

(defun ignore-status-author (status-id)
  "Ignore the future  statuses authored by the user  identified by the
account that wrote the status identified by `status-id'"
  (set-ignore-status-author status-id t))

(defun unignore-author (id)
  "Unignore the future  statuses authored by the user  identified by the
account that wrote the status identified by `status-id'"
  (query (make-update :account
                      (:ignoredp)
                      (+db-false+)
                      (:or (:= :id id)
                           (:= :acct id)))))

(defun all-usernames ()
  (mapcar #'second
          (fetch-all-rows (select :acct (from :account) (order-by :acct)))))

(defun all-ignored-usernames ()
  (mapcar #'second
          (fetch-all-rows (select :acct
                            (from :account)
                            (where (:= :ignoredp +db-true+))
                            (order-by :acct)))))

(defun all-followed-usernames ()
  (mapcar #'second
          (fetch-all-rows (select :account.acct
                            (from :account)
                            (join :followed-user :on (:= :account.id
                                                         :followed-user.user-id))))))

(defun all-unfollowed-usernames (&key (remove-ignored nil))
  (let ((all      (all-usernames))
        (followed (all-followed-usernames)))
    (when remove-ignored
      (setf all (set-difference all (all-ignored-usernames) :test #'string=)))
    (set-difference all followed :test #'string=)))

(defun status-ignored-p (status-id folder timeline)
  "Return non nil if this status should be ignored
(id timeline and  folder is the tuple that is primary key for table
:status)"
  (query (select :*
           (from :ignored-status)
           (where (:and (:= :status-id status-id)
                        (:= :folder    folder)
                        (:= :timeline  timeline))))))

(defun status-skipped-p (status-id folder timeline)
  "Return non nil if this status should be skipped because belong to an ignored account
(id timeline and  folder is the tuple that is primary key for table
:status)"
  (query (select :*
           (from +table-skipped-status+)
           (where (:and (:= :status-id status-id)
                        (:= :folder    folder)
                        (:= :timeline  timeline))))))

(defmacro with-db-current-timestamp ((timestamp) &body body)
  `(let ((,timestamp (prepare-for-db (local-time-obj-now))))
     ,@body))

(defun add-to-status-ignored (status-id folder timeline)
  "Ignore this status (id timeline and  folder is the tuple that is primary key for table
:status), Ignored status wont be downloaded again from the net."
  (when (not (status-ignored-p status-id folder timeline))
    (with-db-current-timestamp (now)
      (query (make-insert +table-ignored-status+
                          (:status-id :folder :timeline :created-at)
                          (status-id  folder  timeline  now))))))

(defun add-to-status-skipped (status-id folder timeline)
  "Skips this  status (id timeline and folder is the tuple that is
primary key for table :status), if in this table the
status has been downloaded from the net and ignored because belog to an ignored account."
  (when (not (status-skipped-p status-id folder timeline))
    (with-db-current-timestamp (now)
      (query (make-insert +table-skipped-status+
                          (:status-id :folder :timeline :created-at)
                          (status-id  folder  timeline  now))))))

(defun add-to-followers (user-id)
  (with-db-current-timestamp (now)
    (query (make-insert +table-followed-user+
                        (:user-id  :created-at)
                        (user-id now)))))

(defun remove-from-followers (user-id)
  (query (make-delete +table-followed-user+
                      (:= :user-id user-id))))

(defun forget-all-statuses-marked-deleted ()
  "Ignore all statuses marked for deletion"
  (let ((all-folders   (all-folders))
        (all-timelines (all-status-timelines)))
    (loop for folder in all-folders do
         (loop for timeline in all-timelines do
              (let ((marked-to-delete (statuses-id-marked-to-delete timeline folder)))
                (loop for status-to-delete in marked-to-delete do
                     (add-to-status-ignored (row-message-status-id status-to-delete)
                                            folder
                                            timeline)))))))

(defun status-id->username (status-id)
  (when-let ((message (fetch-single (make-filtered-message-select nil nil nil nil
                                                                  `(:= :status.status-id
                                                                       ,status-id)))))
    (row-message-username message)))

(defun subscribe-to-tag (tag)
  (assert (stringp tag))
  (assert (string-not-empty-p tag))
  (when (null (fetch-from-id +table-subscribed-tag+ tag))
    (with-db-current-timestamp (now)
      (query (make-insert +table-subscribed-tag+
                          (:id :created-at)
                          (tag now))))))

(defun unsubscribe-to-tag (tag)
  (assert (stringp tag))
  (assert (string-not-empty-p tag))
  (query (make-delete +table-subscribed-tag+
                      (:= :id tag))))

(defun all-subscribed-tags (&key (sort-data nil))
  (let ((query (select :*
                 (from +table-subscribed-tag+))))
    (when sort-data
      (order-by= query
                 '(:asc :id)))
    (fetch-all-rows query)))

(defun all-subscribed-tags-name (&key (sort-data nil) (as-folder-name t))
  (let ((names (mapcar #'row-id (all-subscribed-tags :sort-data sort-data))))
    (if as-folder-name
        names
        (mapcar #'folder-name->tag  names))))

(defun tag-folder-name-p (name)
  "Returns non nil if name is a valid folder name for subsribed tags"
  (scan (strcat "^" +folder-tag-prefix+) name))

(defun tag->folder-name (tag)
  "Add the tag prefix (usually '#') from folder to get the tag name"
  (if (tag-folder-name-p tag)
      tag
      (strcat +folder-tag-prefix+ tag)))

(defun tag->paginations-status (tag timeline)
  (let ((folder (tag->folder-name tag)))
    (values (first-pagination-status-id-timeline-folder timeline folder)
            (last-pagination-status-id-timeline-folder  timeline folder))))

(defun all-tag-paginations-status (tags &optional (timeline +default-tag-timeline+))
  (loop for tag in tags collect
       (multiple-value-bind (oldest newest)
           (tag->paginations-status tag timeline)
         (list oldest newest))))

(defun folder-name->tag (folder)
  "Strip the tag prefix (usually '#')  from tag name to get the folder
name"
  (cl-ppcre:regex-replace +folder-tag-prefix+ folder ""))

(defun max-status-id-subscribed-tag (tag)
  (let* ((max-status-id-row         (fetch-single (select (fields (:max :status-id))
                                                    (from +table-status+)
                                                    (where (:= :folder
                                                               (tag->folder-name tag))))))
         (max-status-id             (second max-status-id-row))
         (max-ignored-status-id-row (fetch-single (select (fields (:max :status-id))
                                                    (from +table-ignored-status+)
                                                    (where (:= :folder
                                                               (tag->folder-name tag))))))
         (max-ignored-status-id     (second max-ignored-status-id-row)))
    (or max-status-id
        max-ignored-status-id)))

(defun more-recent-tag-fetched-p (tag)
  "Returns the most recent message fetched that contains tag `tag', or
nil if no such message exists"
  (when-let* ((row (fetch-from-id +table-subscribed-tag+ tag)))
    (let* ((last-status-id        (db-getf row :last-status-id-fetched))
           (max-status-id-fetched (max-status-id-subscribed-tag tag)))
      (or (null last-status-id)
          (string> max-status-id-fetched
                   last-status-id)))))

(defun all-tags-with-new-message-fetched ()
  "Returns the most recent messages fetched that contains subscribed tags, or
nil if no such messages exist"
  (remove-if-not #'more-recent-tag-fetched-p
                 (all-subscribed-tags-name)))

(defun update-last-seen-status-with-tag (tag)
  (when-let* ((max-status-id (max-status-id-subscribed-tag tag)))
    (query (make-update +table-subscribed-tag+
                        (:last-status-id-fetched)
                        (max-status-id)
                        (:= :id tag)))))

(defun update-last-seen-status-subscribed-tag ()
  (loop for tag in (all-subscribed-tags-name) do
       (update-last-seen-status-with-tag tag)))

(defun tag-histogram (tag)
  (mapcar #'second
          (fetch-all (query (select :count
                              (from +table-tag-histogram+)
                              (where (:= :tag tag)))))))

(defun set-got-new-message-tag (tag value)
  (query (make-update +table-subscribed-tag+
                      (:got-new-message-p)
                      (value)
                      (:= :id tag))))

(defun mark-tag-got-new-messages (tag)
  (set-got-new-message-tag tag +db-true+))

(defun unmark-tag-got-new-messages (tag)
  (set-got-new-message-tag tag +db-false+))

(defun conversation-max-id ()
  (when-let ((row (query (select ((:as (fields (:max :id)) :max))
                           (from +table-conversation+)))))
    (second row)))

(defun all-conversations (&key (remove-ignored t))
  (let ((query (select :*
                 (from +table-conversation+))))
    (order-by= query :id)
    (when remove-ignored
      (and-where query `(:= :ignoredp ,+db-false+)))
    (fetch-all-rows query)))

(defun all-conversations-id (&key (remove-ignored t))
  (mapcar #'row-id (all-conversations :remove-ignored remove-ignored)))

(defun add-conversation (id root-message-status-id &key (folder id))
  "Create  a  new  conversation:  the timeline  for  messges  will  be
+default-converation-timeline+ and  default folder name will  be weual
to id."
  (assert (string-not-empty-p id))
  (assert (string-not-empty-p root-message-status-id))
  (assert (string-not-empty-p folder))
  (when (null (fetch-from-id +table-conversation+ id))
    (with-db-current-timestamp (now)
      (query (make-insert +table-conversation+
                          (:id :folder :root-status-id        :created-at)
                          (id  folder  root-message-status-id now))))))

(defun conversation-id->folder (id)
  (assert (stringp id))
  (assert (string-not-empty-p id))
  (second (fetch-single (select :folder
                          (from +table-conversation+)
                          (where (:= :id id))))))

(defun all-conversation-folders (&key (remove-ignored t))
  (let ((all (all-conversations :remove-ignored remove-ignored)))
    (mapcar #'row-conversation-folder all)))

(defun conversation-folder-exists-p (folder)
  (query (select :*
           (from +table-conversation+)
           (where (:= :folder folder)))))

(defun conversation-root-captured-p (root-status-id)
  "non  nil  if  this  root  status alerady  bleong  to  an  existsing
conversation"
  (query (select :*
           (from +table-conversation+)
           (where (:= :root-status-id root-status-id)))))

(defun update-folder (table old-folder-name new-folder-name)
  "Change folder name in `table'"
  (query (make-update table
                      (:folder)
                      (new-folder-name)
                      (:= :folder old-folder-name))))

(defun update-conversation-folder (old-folder-name new-folder-name)
  "Change conversation folder name"
  (update-folder +table-conversation+ old-folder-name new-folder-name))

(defun update-conversation-folder-by-id (id new-folder-name)
  "Unused"
  (query (make-update +table-conversation+
                      (:folder)
                      (new-folder-name)
                      (:= :id id))))

(defun update-conversation-by-id (id new-folder-name root-message-id)
  "Unused"
  (query (make-update +table-conversation+
                      (:folder :root-status-id)
                      (new-folder-name root-message-id)
                      (:= :id id))))

(defun update-status-folder (old-folder-name new-folder-name)
  "chane statuses folder name"
  (update-folder +table-status+ old-folder-name new-folder-name))

(defun change-conversation-name (old-name new-name)
  "This will update both column folder in table converstion and in table status"
  (with-db-transaction
    (update-status-folder       old-name new-name)
    (update-conversation-folder old-name new-name)))

(defun conversation-messages (name)
  "returns all the message in a conversation in folder `name'"
  (let ((statuses (query (select ((:as :conversation.id :conversation-id)
                                  (:as :account.acct   :username)
                                  (:as :account.locked :locked)
                                  :status.*)
                                 (from :status)
                                 (join :account :on (:= :account.id
                                                        :status.account-id))
                                 (join :conversation :on (:= :conversation.folder
                                                             :status.folder))
                                 (where (:= :conversation.folder name))))))
    statuses))

(defclass conversation-stats ()
  ((conversation-id
    :initform nil
    :initarg  :conversation-id
    :accessor conversation-id)
   (messages-red
    :initform -1
    :initarg  :messages-red
    :accessor messages-red)
   (messages-to-read
    :initform -1
    :initarg  :messages-to-read
    :accessor messages-to-read)
   (conversation-name
    :initform (_ "unknown")
    :initarg  :conversation-name
    :accessor conversation-name))
  (:documentation "Statistics for conversation"))

(defun conversation-read/red (name)
  (let* ((all-messages (conversation-messages name))
         (red          (remove-if-not #'row-message-redp all-messages))
         (to-read      (remove-if     #'row-message-redp all-messages)))
    (values to-read red)))

(defun all-conversation-stats (&key (remove-ignored t))
  "All  statistics  for  all   converstions  optionally  with  ignored
conversation removed (default: remove)"
  (let ((all (all-conversations :remove-ignored remove-ignored)))
    (loop for conversation in all collect
         (let ((name (row-conversation-folder conversation))
               (id   (row-id                  conversation)))
           (multiple-value-bind (to-read red)
               (conversation-read/red name)
             (make-instance 'conversation-stats
                            :conversation-id   id
                            :messages-red      (length red)
                            :messages-to-read  (length to-read)
                            :conversation-name name))))))

(defun ignore-conversation (folder-name)
  "Ignore a conversation, never got new messages"
  (query (make-update +table-conversation+
                      (:ignoredp)
                      (+db-true+)
                      (:= :folder folder-name))))

(defun delete-conversation (folder-name)
  "Delete a conversation from database"
  (query (make-delete +table-conversation+
                      (:= :folder folder-name))))

(defun import-crypto-data (user-id key)
  (assert user-id)
  (assert key)
  (with-db-transaction
    (when (fetch-from-id +table-account+ user-id)
      (query (make-insert +table-crypto-data+
                          (:key)
                          (key)))
      (let ((last-crypto-data (second (fetch-single (select (fields (:max :id))
                                                      (from +table-crypto-data+))))))
        (query (make-update +table-account+
                            (:encryption-key-id)
                            (last-crypto-data)
                            (:= :id user-id)))))))

(defun crypto-user-key (username)
  (assert username)
  (assert (stringp username))
  (let ((data (fetch-single (select ((:as :crypto.key :key))
                              (from :crypto)
                              (join :account :on (:= :account.encryption-key-id :crypto.id))
                              (where (:= :account.acct username))))))
    (second data)))

(defun cache-touch (key)
  "Update the existing cache row accessing time to current time."
  (with-db-current-timestamp (now)
    (query (make-update +table-cache+
                        (:accessed-at)
                        (now)
                        (:= :key key)))))

(defun cache-put (key &optional (type "generic"))
  "Insert a new cahe row with key `key'"
  (if (cache-get key)
      (with-db-transaction
        (cache-touch key)
        (cache-get-value key))
      (with-db-transaction
        (with-db-current-timestamp (now)
          (query (make-insert +table-cache+
                              (:key :type :created-at :accessed-at)
                              (key  type  now          now)))
          (last-inserted-rowid)))))

(defun cache-get (key)
  "Get cache row identified by `key'"
  (fetch-single (select :*
                  (from :cache)
                  (where (:= :key key)))))

(defun cache-get-value (key)
  "Get cache value identified by `key'"
  (row-id (cache-get key)))

(defun cache-invalidate (key)
  "delete cache row identified by `key'"
  (assert key)
  (query (make-delete +table-cache+
                      (:= :key key))))

(defun cache-expired-p (key &key (days-in-the-past (swconf:config-purge-cage-days-offset)))
  "Return non  nil if the last  time the cache was  accessed was older
than `days-in-the-past' days (default: `(swconf:config-purge-cache-days-offset)'"
  (let ((row (cache-get key)))
    (if (null row)
       t
       (let ((access-time (encode-datetime-string (db-getf row :accessed-at)))
             (offset      (threshold-time days-in-the-past)))
         (local-time:timestamp< access-time
                                offset)))))

(defun tofu-passes-p (host hash)
  (let ((known-host (fetch-single (select :*
                                    (from +table-gemini-tofu-cert+)
                                    (where (:= :host host))))))
    (cond
      (known-host
       (string= (db-getf known-host :hash) hash))
      (t
       (with-db-current-timestamp (now)
         (query (make-insert +table-gemini-tofu-cert+
                             (:host :hash :seen-at)
                             (host  hash  now)))
         t)))))

(defun tofu-delete (host)
  (query (delete-from +table-gemini-tofu-cert+ (where (:= :host host)))))

(defun ssl-cert-find (url)
  (when-let* ((text-looking-for (strcat url "%"))
              (query            (select :*
                                  (from +table-cache+)
                                  (where (:and (:like :key text-looking-for)
                                               (:= :type +cache-tls-certificate-type+)))))
              (in-cache         (fetch-single query))
              (id               (getf in-cache :id)))
    (values (strcat (os-utils:cached-file-path (to-s id))
                    fs:*directory-sep* os-utils:+ssl-cert-name+)
            (strcat (os-utils:cached-file-path (to-s id))
                    fs:*directory-sep* os-utils:+ssl-key-name+))))

(defun find-tls-certificates-rows (&optional (url ""))
  (when-let* ((text-looking-for (strcat url "%"))
              (query            (select :*
                                  (from +table-cache+)
                                  (where (:and (:like :key text-looking-for)
                                               (:= :type +cache-tls-certificate-type+)))
                                  (order-by (:desc :updated-at)))))
    (fetch-all-rows query)))

(defun gemini-subscribe-url (url title subtitle)
  (query (make-insert +table-gemini-subscription+
                      (:url :title :subtitle)
                      (url  title  subtitle))))

(defun gemini-find-subscription (url)
  (when-let* ((query (select :*
                       (from +table-gemini-subscription+)
                       (where (:= :url url))))
              (row     (fetch-single query)))
    row))

(defun row-unseen-count (row)
  (and row
       (db-getf row :unseen-count :default 0)))

(defun row-seen-count (row)
  (and row
       (db-getf row :seen-count :default 0)))

(defun gemini-all-subscriptions ()
  (when-let* ((query (select (:gemini-subscription.*
                              (:as (select (fields (:count :url))
                                     (from :gemlog-entries)
                                     (where (:and (:= :gemlog-entries.seenp
                                                      (prepare-for-db nil :to-integer t))
                                                  (:= :gemlog-entries.deletedp
                                                      (prepare-for-db nil :to-integer t))
                                                  (:= :gemlog-entries.gemlog-id
                                                   :gemini-subscription.url))))
                               :unseen-count)
                              (:as (select (fields (:count :url))
                                     (from :gemlog-entries)
                                     (where (:and (:= :gemlog-entries.seenp
                                                      (prepare-for-db t :to-integer t))
                                                  (:= :gemlog-entries.deletedp
                                                      (prepare-for-db nil :to-integer t))
                                                  (:= :gemlog-entries.gemlog-id
                                                   :gemini-subscription.url))))
                               :seen-count))
                       (from +table-gemini-subscription+)
                       (order-by :title)))
              (rows  (fetch-all-rows query)))
    rows))

(defun gemini-cancel-subscription (gemlog-url)
  (query (delete-from +table-gemini-subscription+ (where (:= :url gemlog-url)))))

(defun find-gemlog-entry (post-url)
  (when-let* ((query (select :*
                       (from +table-gemlog-entries+)
                       (where (:= :url post-url))))
              (row     (fetch-single query)))
    row))

(defun add-gemlog-entries (gemlog-iri post-url post-title post-date seenp)
  (query (make-insert +table-gemlog-entries+
                      (:url
                       :gemlog-id
                       :date
                       :title
                       :seenp)
                      (post-url
                       gemlog-iri
                       (decode-datetime-string post-date)
                       post-title
                       (prepare-for-db seenp :to-integer t)))))

(defun gemlog-mark-as-seen (post-url)
  (let ((update-query (make-update +table-gemlog-entries+
                                   (:seenp)
                                   ((prepare-for-db t :to-integer t))
                                   (:= :url post-url))))
    (query update-query)))

(gen-access-message-row gemlog-url      :gemlog-url)

(gen-access-message-row gemlog-title    :gemlog-title :only-empty-or-0-are-null t)

(gen-access-message-row gemlog-subtitle :gemlog-subtitle :only-empty-or-0-are-null t)

(gen-access-message-row post-date       :post-date)

(gen-access-message-row post-title      :post-title :only-empty-or-0-are-null t)

(gen-access-message-row post-link       :post-link)

(gen-access-message-row post-seenp      :seenp)

(defun gemlog-entries (gemlog-url &key (unseen-only nil) (seen-only nil))
  (assert (not (and unseen-only
                    seen-only)))
  (when-let* ((query         (select ((:as :gemini-subscription.url      :gemlog-url)
                                      (:as :gemini-subscription.title    :gemlog-title)
                                      (:as :gemini-subscription.subtitle :gemlog-subtitle)
                                      (:as :gemlog-entries.date          :post-date)
                                      (:as :gemlog-entries.title         :post-title)
                                      (:as :gemlog-entries.url           :post-link)
                                      (:as :gemlog-entries.seenp         :seenp))
                               (from :gemlog-entries)
                               (join :gemini-subscription
                                     :on (:= :gemlog-entries.gemlog-id
                                          :gemini-subscription.url))
                               (where (:and (:= :gemini-subscription.url gemlog-url)
                                            (:= :gemlog-entries.deletedp
                                                (prepare-for-db nil :to-integer t))))))
              (unordered-rows (fetch-all-rows query))
              (actual-rows    (cond
                                (unseen-only
                                 (remove-if-not (lambda (row) (db-nil-p (row-seenp row)))
                                                unordered-rows))
                                (seen-only
                                 (remove-if (lambda (row) (db-nil-p (row-seenp row)))
                                            unordered-rows))
                                (t
                                 unordered-rows))))
    (num:multisort actual-rows (list (num:gen-multisort-test (lambda (a b)
                                                               (if (and (db-nil-p a)
                                                                        (db-nil-p b))
                                                                   a
                                                                   (db-nil-p a)))
                                                             (lambda (a b)
                                                               (if (and (db-nil-p a)
                                                                        (db-nil-p b))
                                                                   b
                                                                   (db-nil-p b)))
                                                             (lambda (a)
                                                               (db-getf a :seenp)))
                                     (num:gen-multisort-test string>
                                                             string<
                                                             (lambda (a)
                                                               (row-post-date a)))))))

(defun delete-gemlog-entry (gemlog-url)
  (query (make-update +table-gemlog-entries+
                        (:deletedp)
                        ((prepare-for-db t :to-integer 1))
                        (where (:= :url gemlog-url)))))

(defun purge-seen-gemlog-entries ()
  "Remove expired gemlog and (seen) entries.

An entry is expired if older than (swconf:config-purge-history-days-offset)
days in the past"
  (let ((treshold (threshold-time -255)))
    (query (make-update +table-gemlog-entries+
                        (:deletedp)
                        ((prepare-for-db t :to-integer 1))
                        (:and (:= :seenp (prepare-for-db t :to-integer 1))
                              (:< :date  (prepare-for-db treshold)))))))
