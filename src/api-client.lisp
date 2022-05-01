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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :api-client)

(defparameter *client* nil
  "The tooter client, used to access mastodon")

(defparameter *credentials* nil
  "An istance of 'credentials' used to holds the intormation needed to
  access a mastodon instance")

(defparameter *client-lock* (bt:make-recursive-lock)
  "The Lock for prevent race conditions when accessing the mastodon server")

(define-constant +credentials-filename+ "client"    :test #'string=
                 :documentation   "The   name   of  the   file   where
                 credentials are stored")

(define-constant +protocol-scheme+      "https://" :test #'string=
                 :documentation "The  scheme of the protocol  that the
                 mastodon server understand.")

(defun make-base-slot ()
  "Makes  the  'base'  slots   for  credential,  used  in  credentials
initform."
  (strcat +protocol-scheme+ (swconf:config-server-name)))

(defclass credentials ()
  ((base
    :initform (make-base-slot)
    :initarg  :base
    :accessor base
    :documentation "The url of the mastodon instance to be accessed")
   (key
    :initform nil
    :initarg :key
    :accessor key
    :documentation "API key to access the instance")
   (secret
    :initform nil
    :initarg :secret
    :accessor secret
    :documentation "Secret key to access the instance")
   (access-token
    :initform nil
    :initarg  :access-token
    :accessor access-token
    :documentation "Access token to access the instance"))
  (:documentation  "Represents the  credentials to  access a  mastodon
  instance"))

(defmethod marshal:class-persistant-slots ((object credentials))
  "Serializer for class `credentials', see `cl-marshal'"
  (append '(base key secret access-token)
          (call-next-method)))

(defun dump-credentials ()
  "Serialize `*credential*' to disk"
  (let ((resource-file (res:get-data-file +credentials-filename+)))
    (fs:dump-sequence-to-file (serialize *credentials*)
                              resource-file)))

(defun forget-credentials ()
  "Remove credentials data file"
  (conditions:with-default-on-error (nil)
    (let ((resource-file (res:get-data-file +credentials-filename+)))
      (fs:delete-file-if-exists resource-file))))

(defun credentials-complete-p ()
  "Returns non nil if *credentials* contains all necessary data to log
into the mastodon server"
  (when *credentials*
    (with-accessors ((base         base)
                     (key          key)
                     (secret       secret)
                     (access-token access-token)) *credentials*
      (and base
           key
           secret
           access-token))))

(defclass api-client (tooter:client) ()
  (:documentation "A mastodon client instance"))

(defun copy-credentials-to-client ()
  "Copy credential data from `*credentials*' into `*client*'"
   (setf (tooter:base         *client*) (base         *credentials*))
   (setf (tooter:key          *client*) (key          *credentials*))
   (setf (tooter:secret       *client*) (secret       *credentials*))
   (setf (tooter:access-token *client*) (access-token *credentials*))
   (setf (tooter:name         *client*) +program-name+)
   *client*)

(defun copy-credentials-from-client ()
  "Copy credential data from `*client*' to `*credentials*'"
   (setf (base         *credentials*) (tooter:base         *client*))
   (setf (key          *credentials*) (tooter:key          *client*))
   (setf (secret       *credentials*) (tooter:secret       *client*))
   (setf (access-token *credentials*) (tooter:access-token *client*))
   *credentials*)

(defun authorize-dialog-message ()
  "Message printed when asking user to visit the autorization URL"
  (_ "Please visit the address below."))

(defun open-catch-code-socket ()
  "Returns  a server  socket on  an arbitrary  port, used  to get  the
authorization  code  from  mastondo  instance  with  its  `return-url'
parameter, returns nil if a socket can not be opened."
  (loop for port from 20000 to 50000 do
       (let ((server (ignore-errors
                       (usocket:socket-listen "127.0.0.1" port))))
         (when server
           (return-from open-catch-code-socket
             (values server port)))))
  nil)

(defparameter *http-auth-reply-headers*
  '("HTTP/1.1 200 OK"
    "Content-Type: text/html; charset=UTF-8"
    "Connection: close"))

(defun http-auth-reply-body-template ()
  (_ "<!doctype html><html><head><meta charset=\"utf-8\"/></head><body><h1>~a has been successfully authorized, you can close this tab.</h1></body></html>"))

(defun catch-auth-code (socket)
  "When an user authorize a client  to access mastodon the server send
an http request to an arbitrary URI chosen by the user.

This  URI contains  the authorization  code neede  to make  the client
trusted by the  server.  When tinmop starts  the authorization process
opens a  server on the local  machine and asks the  server to redirect
the user's browser to an URI  (which contains the autorization code on
a query string)  that points to the local machine.   The server on the
local machine  read the data  from the  browser's request and  get the
authorization code.

This  function perfom  the  latest  of this  actions  and returns  the
authorization code."
  (unwind-protect
       (let ((client-socket (usocket:socket-accept socket)))
         (unwind-protect
              (let* ((stream (usocket:socket-stream client-socket))
                     (line (read-line stream)))
                (multiple-value-bind (matched query-string)
                    (cl-ppcre:scan-to-strings "code=\(.+\)" line)
                  (when matched
                    (prog1
                        (first (cl-ppcre:split "(&)|(\\p{White_Space})" (first-elt query-string)))
                      (let* ((endline (format nil "~C~C" #\return #\linefeed))
                             (headers (join-with-strings *http-auth-reply-headers* endline)))
                        (format stream "~a~a~a" headers endline endline)
                        (format stream (http-auth-reply-body-template) +program-name+))))))
           (usocket:socket-close client-socket)))
    (usocket:socket-close socket)))

(defun make-redirect-url (port)
  "This is  part of the url  where the browser will  be redirect after
authorizations was performed with success."
  (strcat "http://127.0.0.1:" (to-s port)))

(defun make-default-client ()
  "Convenience funtion to build a `api-client' instance

Returns nil if the user did not provided a server in the configuration file"
  (when (text-utils:string-not-empty-p (swconf:config-server-name))
    (make-instance 'api-client
                   :base     (make-base-slot)
                   :name     +program-name+)))

(defun client-valid-p ()
  *client*)

(defun authorize ()
  "Perform all the steps to authorize this application"
  (setf *client* (make-default-client))
  (when (client-valid-p)
    (if (credentials-complete-p)
        (progn
          (copy-credentials-to-client)
          (tooter:authorize *client*)
          (handler-case
              (application-credentials)
            (error ()
              (ui:error-dialog-immediate
               (format nil
                       (_ "Credential invalid. Try to remove ~a and restart the software to authenticate again.")
                       (res:get-data-file +credentials-filename+))))))
        (multiple-value-bind (server-socket server-port)
            (open-catch-code-socket)
          (setf *client* (make-default-client))
          (setf (tooter:redirect *client*) (make-redirect-url server-port))
          #+debug-mode (misc:dbg "Client ~a not authorized" *client*)
          (multiple-value-bind (a url)
              (tooter:authorize *client*)
            (declare (ignore a))
            (let* ((dialog-msg (authorize-dialog-message))
                   (save-item   (_ "Save address"))
                   (open-item   (_ "Open address"))
                   (cancel-item (_ "Cancel"))
                   (choosen    (ui:info-dialog-immediate (format nil "~a~%~a" dialog-msg url)
                                                         :buttons (list save-item
                                                                        open-item
                                                                        cancel-item)
                                                         :append-ok-button nil)))
              (labels ((on-got-authorization-code (value)
                         (handler-case
                             (progn
                               (tooter:authorize *client* value)
                               (copy-credentials-from-client)
                               (dump-credentials)
                               (ui:notify (_ "This client has been authorized")))
                           (tooter:request-failed (error)
                             (ui:request-error-window error))
                           (error ()
                             (ui:error-dialog-immediate (_ "Got a generic error when registering client")))))
                       (notify-file-saved (filepath)
                         (ui:notify (format nil (_ "File ~a saved") filepath)))
                       (save-credentials ()
                         (let* ((message  (_ "Please enter below the file where to save the address"))
                                (filepath (ui:input-dialog-immediate message)))
                           (cond
                             ((null filepath)
                              (save-credentials))
                             ((fs:file-exists-p filepath)
                              (if (ui:confirm-file-overwrite-dialog-immediate filepath)
                                  (progn
                                    (fs:dump-sequence-to-file url filepath)
                                    (notify-file-saved filepath))
                                  (save-credentials)))
                             (t
                              (fs:dump-sequence-to-file url filepath)
                              (notify-file-saved filepath))))))
                (cond
                  ((string= choosen open-item)
                   (os-utils:xdg-open url)
                   (if server-socket
                       (let ((authcode (catch-auth-code server-socket)))
                         (on-got-authorization-code authcode))
                       (ui:error-dialog-immediate (_ "Error: was not able to create server socket to listen for authorization code"))))
                  ((string= choosen save-item)
                   (save-credentials))))))))))

(defmacro defun-api-call (name parameters &body body)
  (multiple-value-bind (remaining-forms declarations doc-string)
      (alexandria:parse-body body :documentation t)
    `(defun-w-lock ,name ,parameters *client-lock*
       ,doc-string
       ,declarations
       (when (client-valid-p)
         ,@remaining-forms))))

(defun-api-call favourite-status (status-id)
    "Favourite a status identified by `status-id'"
  (tooter:favourite *client*
                    status-id))

(defun-api-call unfavourite-status (status-id)
    "Unfavourite a status identified by `status-id'"
  (tooter:unfavourite *client*
                      status-id))

(defun-api-call reblog-status (status-id)
    "Reblog a status identified by `status-id'"
  (tooter:reblog *client*
                 status-id))

(defun-api-call unreblog-status (status-id)
    "Reblog a status identified by `status-id'"
  (tooter:unreblog *client*
                   status-id))

(define-constant +public-timeline+ "public" :test #'string=)

(defun-api-call get-timeline (kind &key local only-media max-id since-id min-id (limit 20))
  "Get messages (status) belonging to a timeline

- kind: one of
     api-client:+public-timeline+
     db:+home-timeline+

- local: get status local to the instance the client is connected to

- only-media get status with attachments only
- max-id get status until this id
- min-id starts getting messages newer than this id
- since-id cut the messages got starting from this id
- limit gets a maimum of messages up to this value."
  (assert (or (string-equal kind +public-timeline+)
              (string-equal kind db:+home-timeline+)))
  (tooter:timeline *client*
                   kind
                   :local      local
                   :only-media only-media
                   :max-id     max-id
                   :since-id   since-id
                   :min-id     min-id
                   :limit      limit))

(defun status-id< (a b)
  (string< (tooter:id a)
           (tooter:id b)))

(defun update-pagination-statuses-so-far (statuses timeline folder
                                          &key
                                            (event-priority program-events:+standard-event-priority+))
  (loop for status in statuses do
       (let ((add-fetched-event (make-instance 'program-events:add-pagination-status-event
                                               :priority  event-priority
                                               :status-id (tooter:id status)
                                               :timeline  timeline
                                               :folder    folder)))
         (program-events:push-event add-fetched-event))))

(defun-api-call update-timeline (timeline
                                 kind
                                 folder
                                 &key
                                 recover-from-skipped-statuses
                                 local
                                 only-media
                                 max-id
                                 since-id
                                 min-id
                                 (recover-count 0)
                                 (limit 20))
  "Update a timeline, this function will fetch new messages and generate and event to
  update the  program reflectings the  changes in the  timeline (saves
  messages in the database etc.)"
  (let* ((timeline-statuses (get-timeline kind
                                          :local      local
                                          :only-media only-media
                                          :max-id     max-id
                                          :since-id   since-id
                                          :min-id     min-id
                                          :limit      limit))
         (trees             (if command-line:*update-timeline-climb-message-tree*
                                (flatten (loop for node-status in timeline-statuses collect
                                              (expand-status-tree node-status)))
                                timeline-statuses))
         (event             (make-instance 'program-events:save-timeline-in-db-event
                                           :payload       trees
                                           :kind          kind
                                           :timeline-type timeline
                                           :folder        folder
                                           :localp        local
                                           :min-id        min-id
                                           :max-id        max-id
                                           :recover-count recover-count
                                           :recover-from-skipped-statuses
                                           recover-from-skipped-statuses)))
    ;; note that,  because events are  enqueued with priority  and the
    ;; first instanced event has better priority than another instanced
    ;; later,  the events  generated by  the function  below will  run
    ;; after  the  save-timeline-in-db-event, that is because we give the
    ;; event generated by 'update-pagination-statuses-so-far' the maximum priority
    (update-pagination-statuses-so-far timeline-statuses
                                       timeline
                                       folder
                                       :event-priority
                                       program-events:+maximum-event-priority+)
    (program-events:push-event event)))

(defun-api-call get-timeline-tag (tag &key min-id (limit 20))
  "Gets messages that contains tags identified by parameter `tag'"
  (tooter:timeline-tag *client*
                       tag
                       :local      nil
                       :only-media nil
                       :max-id     nil
                       :since-id   nil
                       :min-id     min-id
                       :limit      limit))

(defun-api-call update-timeline-tag (tag folder &key
                                                (recover-count 0)
                                                min-id
                                                (limit 20))
  "Update a tag timeline, this  function will fetch new messages (that
  contains tag  `tag') and  generate and event  to update  the program
  reflectings  the changes  in  the timeline  (saves  messages in  the
  database etc.)"
  (when tag
    (let* ((timeline-statuses         (get-timeline-tag tag
                                                        :min-id min-id
                                                        :limit  limit))
           (trees                     (if command-line:*update-timeline-climb-message-tree*
                                          (flatten (loop
                                                      for node-status in timeline-statuses
                                                      collect
                                                        (expand-status-tree node-status)))
                                          timeline-statuses))
           (save-timeline-in-db-event (make-instance 'program-events:save-timeline-in-db-event
                                                     :payload       trees
                                                     :timeline-type db:+federated-timeline+
                                                     :folder        folder
                                                     :localp        nil
                                                     :min-id        min-id
                                                     :recover-count recover-count)))
      (update-pagination-statuses-so-far timeline-statuses
                                         db:+default-tag-timeline+
                                         folder
                                         :event-priority
                                         program-events:+maximum-event-priority+)
      (program-events:push-event save-timeline-in-db-event))))

(defun tag-name (tag &key (return-empty-string-if-nil nil))
  "Returns a convevient tag name from `tooter:tag'.

if  `return-empty-string-if-nil' is  non nil  an apty  tag (nil)  will
become an emty string (\"\")
"
  (let ((name (tooter:name tag)))
    (or name
        (if return-empty-string-if-nil
            ""
            nil))))

(defun-api-call update-subscribed-tags (all-tags all-paginations &key (limit 20))
  "Update all tage in the list `all-tags'"
  (loop
     for tag    in all-tags
     for max-id in (mapcar #'second all-paginations)
     do
       (let ((tag-folder (db:tag->folder-name tag)))
         (update-timeline-tag tag
                              tag-folder
                              :limit  limit
                              :min-id max-id))))

(defun-api-call fetch-remote-status (status-id)
  "Fetch a single status identified by status-id and generate an event
`fetch-remote-status-event' that, in turn will  save the status to the
database."
  (when-let* ((status (tooter:find-status *client* status-id))
              (event  (make-instance 'program-events:fetch-remote-status-event
                                     :payload status)))
    (program-events:push-event event)))

(defun-api-call get-remote-status (status-id)
  "Get a single status identifird bu status-id"
  (ignore-errors
    (tooter:find-status *client* status-id)))

(defun-api-call get-status-context (status-id)
  "Get a parent and a child of a status (identified by status-id), if exists"
  (tooter:context *client* status-id))

(defun-api-call send-status (content
                             in-reply-to-id
                             attachments
                             attachments-alt-text
                             subject visibility)
  "Send a status
- content the actual text of the message
- in-reply-to-id status-id of the message  you are replying to (or nil
  if this message is not a reply
- attachments a list  of file path to attach or  nil il no attachments
  to this message exists
- subject the subkec of this message
- visibility one of `swconf:*allowed-status-visibility*'"
  (tooter:make-status *client*
                      content
                      :in-reply-to  in-reply-to-id
                      :media        (mapcar (lambda (path alt-text)
                                              (tooter:make-media *client*
                                                                 (fs:namestring->pathname path)
                                                                 :description alt-text))
                                            attachments
                                            attachments-alt-text)
                      :spoiler-text subject
                      :visibility   visibility))

(defun-api-call search-user (username &key (limit 1) (resolve nil))
  "Find user identified by username"
  (tooter:search-accounts *client* username :limit limit :resolve resolve))

(defun-api-call find-results (query
                              &key
                              (account-id         nil)
                              (max-id             nil)
                              (min-id             nil)
                              (kind               "statuses")
                              (exclude-unreviewed nil)
                              (resolve            t)
                              (limit             20)
                              (offset             0)
                              (following          nil))
  "Search stuff, default statuses"
  (tooter:find-results *client*
                       query
                       :account-id         account-id
                       :max-id             max-id
                       :min-id             min-id
                       :kind               kind
                       :exclude-unreviewed exclude-unreviewed
                       :resolve            resolve
                       :limit              limit
                       :offset             offset
                       :following          following))

(defun-api-call follow-user (user-id)
  "Follow user identified by user-id"
  (tooter:follow *client* user-id))

(defun-api-call unfollow-user (user-id)
  "Unfollow user identified by user-id"
  (tooter:unfollow *client* user-id))

(defun-api-call follow-requests ()
  "Gets the request to follow the user of this client"
  (let ((requests (tooter:follow-requests *client*)))
    (values requests
            (mapcar #'tooter:account-name requests))))

(defun-api-call accept-follow-request (user-id)
  "Accept a follow request from user identified by `user-id'"
  (when user-id
    (tooter:accept-request *client* user-id)))

(defun-api-call reject-follow-request (user-id)
  "Reject a follow request from user identified by `user-id'"
  (when user-id
    (tooter:reject-request *client* user-id)))

(defclass conversation-tree ()
  ((id
    :initform nil
    :initarg  :id
    :accessor id
    :type (or null string)
    :documentation "The conversation ID")
   (status-tree
    :initform nil
    :initarg  :status-tree
    :accessor status-tree
    :type list
    :documentation "A flat list of statuses that forms the conversation tree")
   (last-status
    :initform nil
    :initarg  :last-status
    :accessor last-status
    :type     (or null tooter:status)
    :documentation "The lastest status that forms this conversation")
   (root
    :initform nil
    :initarg  :root
    :accessor root
    :type     (or null tooter:status)
    :documentation "The first status that forms this conversation"))
  (:documentation "Represents a tree of message belonging to a conversation"))

(defmethod print-object ((object conversation-tree) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((id   id)
                     (status-tree status-tree)
                     (root root)) object
      (format stream "id: ~a tree: ~a root ~a" id status-tree root))))

(defgeneric conversation-root-id (object))

(defmethod conversation-root-id ((object conversation-tree))
  "Status id of the root of a conversation tree"
  (tooter:id (root object)))

(defun-api-call conversations (&key
                             (min-id    nil)
                             (since-id  nil)
                             (max-id    nil)
                             (limit     20)
                             (root-only nil))
  "Get trees of conversations
- max-id get status until this id
- min-id starts getting messages newer than this id
- since-id cut the messages got starting drom this id
- limit gets a maimum of messages up to this value
- root-only if non nil do not return the whole trees just the root of each."
  (let ((conversations (tooter:conversations *client*
                                             :min-id   min-id
                                             :since-id since-id
                                             :max-id   max-id
                                             :limit    limit)))
    (loop for conversation in conversations collect
         (let* ((conversation-id   (tooter:id conversation))
                (last-status       (tooter:last-status conversation))
                (conversation-tree (and (not root-only)
                                        (expand-status-tree last-status)))
                (sorted-tree       (sort conversation-tree
                                         (lambda (a b)
                                           (string< (tooter:id a)
                                                    (tooter:id b)))))
                (root-message      (if (not root-only)
                                       (first sorted-tree)
                                       (climb-fetch-statuses last-status))))
           (make-instance 'conversation-tree
                          :last-status last-status
                          :id          conversation-id
                          :status-tree sorted-tree
                          :root        root-message)))))

(defun expand-conversations-tree (message-root-id)
  "fetch   all   the   tree   that   stars   from   `message-root-id',
i.e. `message-root-id' is root for said tree."
  (expand-status-tree message-root-id))

(defun-api-call delete-conversation (conversation-id)
  "Delete a conversation identified by `conversation-id'"
  (tooter:delete-conversation *client* conversation-id))

(defun-api-call delete-status (status-id)
  (tooter:delete-status *client* status-id))

(defun-api-call make-report (account-id status-id comment forward)
  "Report    an   user    (identified   by    `account-id')   and    a
status (identified by `status-id') to and instance admin, if `forward'
is non nil the  report will be forwarded to the  non local admin where
the account belongs."
  (tooter:make-report *client*
                      account-id
                      :statuses (list status-id)
                      :comment  comment
                      :forward  forward))

(defun-api-call get-activity ()
  "Get instance stats"
  (tooter:get-activity *client*))

(defun-api-call application-credentials ()
  "Verify the  credentials to log  into the server with  the instance,
returns nil if the credentials are invalid"
  (tooter:verify-app-credentials *client*))

(defun-api-call bookmarks (&key
                         (min-id    nil)
                         (since-id  nil)
                         (max-id    nil)
                         (limit     20))
  "List Bookmarked statuses.
- max-id get status until this id
- min-id starts getting messages newer than this id
- since-id cut the messages got starting drom this id
- limit gets a maimum of messages up to this value."
  (tooter:bookmarks *client*
                    :min-id   min-id
                    :since-id since-id
                    :max-id   max-id
                    :limit    limit))

(defun-api-call bookmark (id)
  "Bookmark a status identified by `id'"
  (assert (stringp id))
  (tooter:bookmark *client* id))

(defun-api-call unbookmark (id)
  "Unbokmark a status identified by `id'"
  (assert (stringp id))
  (tooter:unbookmark *client* id))

(defun-api-call polls (id)
  "Get a poll identified by `id'"
  (assert (stringp id))
  (tooter:polls *client* id))

(defun-api-call poll-vote (poll-id choices)
  "Vote  for a  poll identified  by `poll-id',  choices is  a list  of
numerical indices identifying the option voting for"
  (assert (every (lambda (a)
                   (or (numberp a)
                       (parse-integer a :radix 10)))
                 choices))
  (tooter:poll-vote *client* poll-id choices))

(defun-api-call get-notifications (&key
                                   (max-id        nil)
                                   (min-id        nil)
                                   (since-id      nil)
                                   (limit         10)
                                   (exclude-types nil)
                                   (account-id    nil))
  "get notifications

- max-id get notification until this id
- min-id starts getting notification newer than this id
- since-id cut the notifications starting from this id
- limit gets a maximum of messages up to this value
- exclude-types list types of modifications that will not be fetched."
  (tooter:notifications *client*
                        :max-id        max-id
                        :min-id        min-id
                        :since-id      since-id
                        :limit         limit
                        :exclude-types exclude-types
                        :account-id    account-id))

(defun mentions (max-id &optional (excluded-types '(:follow :favourite :reblog)))
  "Get the  latest mentions, starting from `min-id` (pass nil to get
the latest 15 mentions)."
  (get-notifications :max-id        max-id
                     :exclude-types excluded-types))

(defun-api-call delete-notification (notification-id)
  "Delete a notification identified by `notification-id'"
  (tooter:delete-notification *client* notification-id))

(defun sort-id< (list)
  "Sort entities by id in descending order"
  (sort list #'status-id<))

(defun all-mentions ()
  "Get all mentions"
  (let ((mentions-so-far (sort-id< (mentions nil))))
    (when mentions-so-far
      (labels ((%mentions ()
                 (when-let* ((min-id   (tooter:id (first mentions-so-far)))
                             (mentions (sort-id< (mentions min-id))))
                   (loop for mention in mentions do
                        (pushnew mention mentions-so-far :test (make-id=)))
                   (setf mentions-so-far (sort-id< mentions-so-far))
                   (when mentions
                     (%mentions)))))
        (%mentions)))
    mentions-so-far))

(defun update-mentions-folder (&key (delete-mentions-on-server t))
  (let ((trees '()))
    (when-let* ((all-mentions      (all-mentions))
                (statuses          (loop for mention in all-mentions
                                         when (tooter:status mention)
                                           collect (tooter:status mention))))
      (loop for status in statuses
            when (not (member status trees))
              do
                 (loop for node in (expand-status-tree status)
                       do
                          (pushnew node trees)))
      (let ((event (make-instance 'program-events:save-timeline-in-db-event
                                  :payload       trees
                                  :timeline-type db:+home-timeline+
                                  :folder        db:+mentions-status-folder+
                                  :localp        t
                                  :min-id        nil)))
        (when delete-mentions-on-server
          (loop for mention in all-mentions do
            (delete-notification (tooter:id mention))))
        (program-events:push-event event)
        all-mentions))))

(defun expand-status-thread (status-id timeline folder force-saving-of-ignored-status-p)
  (when-let* ((tree  (expand-status-tree status-id))
              (event (make-instance 'program-events:save-timeline-in-db-event
                                    :payload       tree
                                    :timeline-type timeline
                                    :folder        folder
                                    :force-saving-of-ignored-status
                                    force-saving-of-ignored-status-p)))
    (program-events:push-event event)
    tree))

(defgeneric climb-fetch-statuses (object &optional branch))

(defmethod climb-fetch-statuses ((object tooter:status) &optional (branch ()))
  (climb-fetch-statuses (tooter:id object) branch))

(defmethod climb-fetch-statuses ((object string) &optional (branch ()))
  "Starting from  'object' (a toot's  ID) climbs messages  tree, fetch
parent status  from the internet if  needed until the message  root is
found, return two values the root of the tree and the statuses climbed
while  reaching  the  root  (a  branch, or  portion  of  a  branch  if
node-status-id is not a leaf)."
  (flet ((reply-id (status)
           (tooter:in-reply-to-id status)))
    (when-let ((status (get-remote-status object)))
      (if (null (reply-id status))      ; the root
          (values status (push status branch))
          (progn
            (climb-fetch-statuses (reply-id status)
                                  (push status branch)))))))

(defalias id= #'string=)

(defun make-id= (&optional (test #'id=))
  "Returns a comparator function that checks for id equality"
  (lambda (a b) (funcall test (tooter:id a) (tooter:id b))))

(defgeneric expand-status-tree (object))

(defmethod expand-status-tree ((object tooter:status))
  (expand-status-tree (tooter:id object)))

(defmethod expand-status-tree ((object string))
  "Given a status id returns the complete messages tree this status belong."
  (multiple-value-bind (root fetched-branch)
      (climb-fetch-statuses object)
    (let ((res (copy-list fetched-branch)))
      (labels ((descend (node-id)
                 (when node-id
                   (let* ((context  (get-status-context node-id))
                          (children (tooter:descendants context)))
                     (loop for child in children do
                          (when (not (find child res :test (make-id=)))
                            (push child res)
                            (descend (tooter:id child))))))))
        (descend root)
        res))))

(defun make-placeholder-tag-histogram ()
  "Make an  empty `tooter:tag-history' (empty  means all counts  are 0
and day is current time)"
  (make-instance 'tooter:tag-history
                 :account-count 0
                 :use-count     0
                 :day           (get-universal-time)))

(defun init ()
  "Initialize the client, prepare it for `authorize'."
  (flet ((credentials-filename ()
           (handler-bind ((error
                            (lambda (e)
                              (declare (ignore e))
                              (invoke-restart 'res:return-home-filename))))
             (res:get-data-file +credentials-filename+))))
    (let ((resource-file (credentials-filename)))
      (if (not (fs:file-exists-p resource-file))
          (progn
            #+debug-mode (misc:dbg (_ "Initializing empty credentials file in ~a")
                                   resource-file)
            (fs:dump-sequence-to-file (serialize (make-instance 'credentials))
                                      resource-file)
            (init))
          (setf *credentials* (deserialize t resource-file))))))
