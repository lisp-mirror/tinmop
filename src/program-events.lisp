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

(in-package :program-events)

(defparameter *id-lock* (bt:make-recursive-lock))

(defparameter *event-id* 0)

;; used only in batch mode from the command line
(defparameter *process-events-immediately* nil
  "Used only in batch mode from the command line. Instead of pushing the event on a priority queue that will be picked by a thread process the event immediately")

(define-constant +standard-event-priority+ 10 :test #'=)

;; keep  this  function  stricly  monotonic  otherwise  the  order  of
;; elements in priority queue is going to be messed up
(defun-w-lock next-id () *id-lock*
  (incf *event-id*)
  *event-id*)

(defmacro with-notify-errors (&body body)
  `(handler-case
       (progn
         ,@body)
     (error (e)
       (ui:notify (format nil (_ "Error: ~a") e)
                  :life     (* (swconf:config-notification-life) 5)
                  :as-error t))))

(defclass program-event ()
  ((event-id
    :initform (next-id)
    :initarg  :event-id
    :accessor event-id)
   (payload
    :initform nil
    :initarg  :payload
    :accessor payload)
   (priority
    :initform +standard-event-priority+
    :initarg  :priority
    :accessor priority)))

(defmethod print-object ((object program-event) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "id ~a priority ~a" (event-id object) (priority object))))

(defgeneric process-event (object))

(defgeneric reinitialize-id (object))

(defmethod  reinitialize-id ((object program-event))
  (setf (event-id object)
        (next-id))
  object)

(defclass events-queue (priority-queue)
  ((lock
    :initform (bt:make-recursive-lock)
    :initarg  :lock
    :accessor lock)))

(defun queue-compare-predicate (a b)
  (let ((same-priority-p (= (priority a)
                            (priority b))))
    (if same-priority-p
        (< (event-id a)
           (event-id b))
        (< (priority a)
           (priority b)))))

(defun queue-equals-predicate (a b)
  (= (event-id a)
     (event-id b)))

(defmethod initialize-instance :after ((object events-queue) &key &allow-other-keys)
  (with-accessors ((key-function    key-function)
                   (compare-function compare-function)
                   (equal-function   equal-function)) object
    (setf key-function     #'identity)
    (setf equal-function   #'queue-equals-predicate)
    (setf compare-function #'queue-compare-predicate)))

(defmacro wrapped-in-lock ((queue) &body body)
  (with-gensyms (lock)
    `(with-accessors ((,lock lock)) ,queue
       (with-lock (,lock)
         ,@body))))

(defparameter *events-queue* (make-instance 'events-queue))

(defun push-event (event)
  (wrapped-in-lock (*events-queue*)
    (if *process-events-immediately*
        (process-event event)
        (push-element *events-queue* event))))

(defun pop-event ()
  (wrapped-in-lock (*events-queue*)
    (pop-element *events-queue*)))

(defun remove-event (event)
  (wrapped-in-lock (*events-queue*)
    (remove-element *events-queue* event)))

(defun find-event (event &key (key-fn #'identity) (test-fn #'eq))
  (wrapped-in-lock (*events-queue*)
    (find-element *events-queue* event :test-fn test-fn :key-fn key-fn)))

(defun no-events-p ()
  (wrapped-in-lock (*events-queue*)
    (emptyp *events-queue*)))

(defun event-available-p ()
  (not (no-events-p)))

(defun count-events (predicate)
  (wrapped-in-lock (*events-queue*)
    (count-elements-if *events-queue* predicate :key-fn #'identity)))

(defclass event-on-own-thread (program-event)
  ((lock
    :initform (bt:make-recursive-lock)
    :initarg  :lock
    :accessor lock)
   (condition-variable
    :initform (bt:make-condition-variable)
    :initarg  :condition-variable
    :accessor condition-variable))
  (:documentation  "This  is  the  parent  of  all  events  that  are
   generated  in athread  that  is  not the  main  thread, contains  a
   condition variable and associated lock"))

(defclass ask-user-input-string-event (event-on-own-thread)
   ((prompt
    :initform +default-command-prompt+
    :initarg  :prompt
    :accessor prompt)
    (initial-value
    :initform nil
    :initarg  :initial-value
    :accessor initial-value)
    (complete-fn
    :initform nil
    :initarg  :complete-fn
    :accessor complete-fn))
  (:documentation  "This  events,  when processed,  will  prepare  the
  command-window   `specials:*command-window*'   to   ask   for   user
  input. The most  importatn thing is that the  process-event will set
  the slot `command-window:event-to-answer' with  this events and will
  set the payload of this events with the user provided string."))

(defmethod initialize-instance :after ((object ask-user-input-string-event)
                                       &key &allow-other-keys)
  (setf (priority object) (truncate (/ +standard-event-priority+ 2))))

(defmethod process-event ((object ask-user-input-string-event))
  (with-accessors ((prompt        prompt)
                   (initial-value initial-value)
                   (complete-fn   complete-fn)) object
    (setf (command-window:event-to-answer specials:*command-window*)
          object)
    (setf (point-tracker:prompt specials:*command-window*)
          prompt)
    (setf complete:*complete-function* complete-fn)
    (command-window:set-string-mode         specials:*command-window*)
    (command-window:set-history-most-recent specials:*command-window* prompt)
    (setf (command-window:command-line specials:*command-window*)
          initial-value)
    (point-tracker:move-point-to-end        specials:*command-window* initial-value)
    (windows:draw                           specials:*command-window*)))

(defclass user-input-string-event (ask-user-input-string-event)
  ()
  (:documentation  "When  user provided  a  string  as this  event  is
  generated. When processed it just wlii notify the condition variable
  of   the  slots   `command-window:event-to-answer'  in   the  object
  `specials:*command-window*' so  that the  callee thread  can restart
  the computation with the input."))

(defmethod initialize-instance :after ((object user-input-string-event)
                                       &key &allow-other-keys)
  (setf (priority object) (truncate (/ +standard-event-priority+ 2))))

(defmethod process-event ((object user-input-string-event))
  (with-accessors ((lock               lock)
                   (condition-variable condition-variable)) object
    (with-lock (lock)
      (bt:condition-notify condition-variable))))

(defclass notify-user-event (program-event)
  ((added-to-pending-p
    :initform nil
    :initarg  :added-to-pending
    :reader   added-to-pending-p
    :writer   (setf added-to-pending))
   (life
    :initform nil
    :initarg  :life
    :accessor life)
   (notify-error
    :initform nil
    :initarg  :notify-error
    :accessor notify-error)))

(defun notify-user-event-p (a)
  (typep a 'notify-user-event))

(defmethod process-event ((object notify-user-event))
  (with-accessors ((added-to-pending-p added-to-pending-p)
                   (notify-error       notify-error)) object
    (let ((other-notification-win (first (mtree:find-child-if specials:*main-window*
                                                              #'notify-window:notify-window-p)))
          (pending-before         (count-events #'notify-user-event-p)))
      (if (null other-notification-win)
          (let* ((life       (or (life object)
                                 (swconf:config-notification-life)))
                 (notify-win (notify-window:make-notification-window (payload object)
                                                                     life
                                                                     :pending
                                                                     pending-before
                                                                     :notify-error
                                                                     notify-error)))
            (notify-window:draw-pending notify-win))
          (progn
            (when (not added-to-pending-p)
              (setf (notify-window:pending other-notification-win)
                    (1+ pending-before))
              (notify-window:draw-pending other-notification-win)
              (setf (added-to-pending object) t))
            (progn
              (setf (event-id object) ; id must be monotonic, so we need to give the event a new one
                    (next-id))
              (push-event object)))))))

(defclass remove-notify-user-event (program-event) ())

(defmethod process-event ((object remove-notify-user-event))
  (let ((win (payload object)))
    (mtree:remove-child specials:*main-window* win)))

(defclass update-timeline-event (program-event)
  ((timeline-type
   :initform nil
   :initarg  :timeline-type
   :accessor timeline-type)
   (folder
   :initform nil
   :initarg  :folder
   :accessor folder)
   (local
   :initform nil
   :initarg  :localp
   :reader   localp
   :writer   (setf local))
   (min-id
   :initform nil
   :initarg  :min-id
   :accessor min-id)))

(defmethod process-event ((object update-timeline-event))
  "Update a timeline, save messages, performs topological sorts"
  (let ((statuses      (payload       object))
        (timeline-type (timeline-type object))
        (folder        (folder        object)))
    #+debug-mode
    (let ((dump (with-output-to-string (stream)
                  (mapcar (lambda (toot) (tooter::present toot stream))
                          statuses))))
      (dbg "statuses ~a" dump))
    (loop for status in statuses do
         (db:update-db status
                       :timeline       timeline-type
                       :folder         folder
                       :skip-ignored-p t))
    (db:renumber-timeline-message-index timeline-type
                                        folder
                                        :account-id nil)))

(defclass fetch-remote-status-event (program-event) ())

(defmethod process-event ((object fetch-remote-status-event))
  (let ((status (payload object)))
    #+debug-mode
    (let ((dump (with-output-to-string (stream)
                  (tooter::present status stream))))
      (dbg "fetch single status ~a" dump))
    (db:update-db status)))

(defclass search-regex-message-content-event (program-event) ())

(defmethod process-event ((object search-regex-message-content-event))
  (let ((regexp (payload object)))
    (message-window:search-regex specials:*message-window* regexp)))

(defclass thread-goto-message (program-event) ())

(defmethod process-event ((object thread-goto-message))
  (let ((message-index (payload object)))
    (thread-window:goto-message specials:*thread-window* message-index)))

(defclass thread-search-event (program-event)
  ((search-direction
    :initform nil
    :initarg  :search-direction
    :accessor search-direction)))

(defclass thread-search-message-body-event (thread-search-event) ())

(defmethod process-event ((object thread-search-message-body-event))
  (let ((text-looking-for (payload          object))
        (search-direction (search-direction object)))
    (if (eq :next search-direction)
        (thread-window:search-next-message-body     specials:*thread-window* text-looking-for)
        (thread-window:search-previous-message-body specials:*thread-window* text-looking-for))))

(defclass thread-search-message-meta-event (thread-search-event) ())

(defmethod process-event ((object thread-search-message-meta-event))
  (let ((text-looking-for (payload          object))
        (search-direction (search-direction object)))
    (if (eq :next search-direction)
        (thread-window:search-next-message-meta     specials:*thread-window* text-looking-for)
        (thread-window:search-previous-message-meta specials:*thread-window* text-looking-for))))

(defclass delete-all-status-event (program-event) ())

(defmethod process-event ((object delete-all-status-event))
  (db:forget-all-statuses-marked-deleted) ; do not change the order. Forget, then delete.
  (db:delete-all-statuses-marked-deleted)
  (db:renumber-all-timelines))

(defclass quit-program-event (program-event) ())

(defmethod process-event ((object quit-program-event))
  (ui:quit-program))

(defclass error-message-event (program-event) ())

(defmethod process-event ((object error-message-event))
  (command-window:add-error-message specials:*command-window* (payload object)))

(defclass info-message-event (program-event) ())

(defmethod process-event ((object info-message-event))
  (command-window:add-info-message specials:*command-window* (payload object)))

(defclass dialog-event (program-event)
  ((buttons
    :initform nil
    :initarg  :buttons
    :accessor buttons)
   (title
    :initform nil
    :initarg  :title
    :accessor title)))

(defclass error-dialog-event (dialog-event)
  ((buttons
    :initform nil
    :initarg  :buttons
    :accessor buttons)
   (title
    :initform nil
    :initarg  :title
    :accessor title)))

(defmethod process-event ((object error-dialog-event))
  (let ((dialog-window (windows:make-error-message-dialog specials:*main-window*
                                                          (title   object)
                                                          (payload object)
                                                          (buttons object))))
    (windows:menu-select dialog-window)))

(defclass info-dialog-event (dialog-event) ())

(defmethod process-event ((object info-dialog-event))
  (let ((dialog-window (windows:make-info-message-dialog specials:*main-window*
                                                         (title   object)
                                                         (payload object)
                                                         (buttons object))))
    (windows:menu-select dialog-window)))

(defclass move-selected-tree-event (program-event)
  ((new-folder
    :initform nil
    :initarg  :new-folder
    :accessor new-folder)))

(defmethod process-event ((object move-selected-tree-event))
  (let ((selected-fields (line-oriented-window:selected-row-fields
                          specials:*thread-window*)))
    (if selected-fields
        (db:move-tree-to-folder (db:row-message-timeline selected-fields)
                                (db:row-message-folder   selected-fields)
                                (db:row-message-index    selected-fields)
                                (new-folder object))
        (ui:error-message (_ "No message selected!")))))

(defclass event-with-message-index ()
  ((message-index
    :initform db:+message-index-start+
    :initarg  :message-index
    :accessor message-index)))

(defclass event-with-timeline-and-folder ()
  ((new-folder
    :initform nil
    :initarg  :new-folder
    :accessor new-folder)
   (new-timeline
    :initform nil
    :initarg  :new-timeline
    :accessor new-timeline)))

(defclass refresh-thread-windows-event (program-event
                                        event-with-message-index
                                        event-with-timeline-and-folder)
  ())

(defmethod process-event ((object refresh-thread-windows-event))
  (with-accessors ((new-folder    new-folder)
                   (new-timeline  new-timeline)
                   (message-index message-index)) object
    (assert message-index)
    (when new-timeline
      (setf (thread-window:timeline-type specials:*thread-window*)
            new-timeline))
    (when new-folder
      (setf (thread-window:timeline-folder specials:*thread-window*)
            new-folder))
    (line-oriented-window:resync-rows-db specials:*thread-window*
                                         :suggested-message-index message-index
                                         :redraw                  t)))

(defun change-status-values (event function-change)
  (with-accessors ((payload       payload)
                   (message-index message-index)) event
    (when-let ((status-to-change payload))
      (funcall function-change status-to-change)
      (client:fetch-remote-status status-to-change)
      (let* ((refresh-event (make-instance 'refresh-thread-windows-event
                                           :message-index message-index)))
        (push-event refresh-event)))))

(defclass favourite-status-event (program-event event-with-message-index) ())

(defmethod process-event ((object favourite-status-event))
  (with-notify-errors
    (change-status-values object #'api-client:favourite-status)))

(defclass unfavourite-status-event (program-event event-with-message-index) ())

(defmethod process-event ((object unfavourite-status-event))
  (with-notify-errors
    (change-status-values object #'api-client:unfavourite-status)))

(defclass reblog-status-event (program-event event-with-message-index) ())

(defmethod process-event ((object reblog-status-event))
  (with-notify-errors
    (change-status-values object #'api-client:reblog-status)))

(defclass unreblog-status-event (program-event event-with-message-index) ())

(defmethod process-event ((object unreblog-status-event))
  (with-notify-errors
    (change-status-values object #'api-client:unreblog-status)))

(defclass unignore-user-event (program-event) ())

(defmethod process-event ((object unignore-user-event))
  (let ((username (payload object)))
    (db:unignore-author username)))

(defclass send-message-change-subject-event (program-event) ())

(defmethod process-event ((object send-message-change-subject-event))
  (let ((new-subject (payload object)))
    (setf (sending-message:subject (sending-message:message-data specials:*send-message-window*))
          new-subject)
    (windows:draw specials:*send-message-window*)))

(defclass send-message-change-visibility-event (program-event) ())

(defmethod process-event ((object send-message-change-visibility-event))
  (let ((new-visibility (payload object))
        (message-data   (sending-message:message-data specials:*send-message-window*)))
    (setf (sending-message:visibility message-data) new-visibility)
    (windows:draw specials:*send-message-window*)))

(defclass open-send-message-window-event (program-event) ())

(defmethod process-event ((object open-send-message-window-event))
  (let ((message-data (payload object)))
    (sending-message:init message-data specials:*main-window*)
    (ui:focus-to-send-message-window)
    (windows:draw specials:*send-message-window*)))

(defclass send-message-add-attachment-event (program-event) ())

(defmethod process-event ((object send-message-add-attachment-event))
  (with-accessors ((croatoan-window windows:croatoan-window)) specials:*send-message-window*
    (let* ((new-attachment (payload object))
           (fg             (croatoan:fgcolor croatoan-window))
           (bg             (croatoan:bgcolor croatoan-window))
           (line           (make-instance 'line-oriented-window:line
                                          :normal-text   new-attachment
                                          :selected-text new-attachment
                                          :normal-bg     bg
                                          :normal-fg     fg
                                          :selected-bg   fg
                                          :selected-fg   bg)))
      (setf (line-oriented-window:rows specials:*send-message-window*)
            (append (line-oriented-window:rows specials:*send-message-window*)
                    (list line)))
      (line-oriented-window:unselect-all specials:*send-message-window*)
      (line-oriented-window:select-row   specials:*send-message-window* 0)
      (windows:draw specials:*send-message-window*))))

(defclass send-message-event (program-event)
  ((use-ui-notification
    :initform nil
    :initarg  :use-ui-notification
    :reader    use-ui-notification-p
    :writer   use-ui-notification)))

(defmethod process-event ((object send-message-event))
  (with-accessors ((message-data sending-message:message-data)
                   (rows         line-oriented-window:rows)) specials:*send-message-window*
    (with-accessors ((body       sending-message:body)
                     (subject    sending-message:subject)
                     (reply-to   sending-message:reply-to)
                     (visibility sending-message:visibility)) message-data
      (let* ((attachments (mapcar #'line-oriented-window:normal-text rows)))
        (hooks:run-hook 'hooks:*before-sending-message* object)
        (msg-utils:maybe-crypt-message specials:*send-message-window*
                                       :notify-cant-crypt (use-ui-notification-p object))
        (let ((exceeding-characters (ui:message-exceeds-server-limit-p body)))
          (if exceeding-characters
              (ui:exceeding-characters-notify exceeding-characters)
              (progn
                (client:send-status body
                                    reply-to
                                    attachments
                                    subject
                                    (make-keyword (string-upcase visibility)))
                (ui:notify (_ "Message sent."))
                (ui:close-send-message-window))))))))

(defclass follow-user-event (program-event) ())

(defmethod process-event ((object follow-user-event))
  (when-let ((username (payload object)))
    (when (find username (db:all-unfollowed-usernames) :test #'string=)
      (let ((user-id  (db:acct->id username)))
        (client:follow-user  user-id)
        (db:add-to-followers user-id)))))

(defclass unfollow-user-event (program-event) ())

(defmethod process-event ((object unfollow-user-event))
  (when-let ((username (payload object)))
    (when (find username (db:all-followed-usernames) :test #'string=)
      (let ((user-id  (db:acct->id username)))
        (client:unfollow-user  user-id)
        (db:remove-from-followers user-id)))))

(defclass open-follow-requests-window-event (program-event) ())

(defmethod process-event ((object open-follow-requests-window-event))
  (with-notify-errors
    (multiple-value-bind (accounts usernames)
        (api-client:follow-requests)
      (when accounts
        (follow-requests:init accounts usernames specials:*main-window*)
        (ui:focus-to-follow-requests-window)
        (windows:draw specials:*follow-requests-window*)))))

(defclass subscribe-tags-event (program-event) ())

(defmethod process-event ((object subscribe-tags-event))
  (when-let* ((tags  (payload object)))
    (loop for tag in (cl-ppcre:split db:+tag-separator+ tags) do
         (db:subscribe-to-tag tag))))

(defclass unsubscribe-tags-event (program-event) ())

(defmethod process-event ((object unsubscribe-tags-event))
  (when-let* ((tag  (payload object)))
    (db:unsubscribe-to-tag tag)))

(defclass update-last-refresh-subscribe-tags-event (program-event) ())

(defmethod process-event ((object update-last-refresh-subscribe-tags-event))
  (db:update-last-seen-status-subscribed-tag))

(defclass notify-fetched-new-tag-messages-event (program-event) ())

(defmethod process-event ((object notify-fetched-new-tag-messages-event))
  (loop for tag in (db:all-tags-with-new-message-fetched) do
       (let ((message (format nil
                              (_ "Downloaded new messages for tag ~a")
                              (db:tag->folder-name tag))))
         (ui:notify message))))

(defclass tag-mark-got-messages-event (program-event) ())

(defmethod process-event ((object tag-mark-got-messages-event))
  (loop for tag in (db:all-tags-with-new-message-fetched) do
       (db:mark-tag-got-new-messages tag)))

(defclass refresh-tag-window-event (program-event) ())

(defmethod process-event ((object refresh-tag-window-event))
  (tags-window:resync-rows-db specials:*tags-window*))

(defclass update-conversations-event (program-event
                                      event-with-timeline-and-folder)
  ())

(defun add-new-conversations ()
  (let* ((new-conversations    (api-client:conversations :root-only t))
         (all-conversations-id (db:all-conversations-id :remove-ignored nil))
         (new-conversations    (remove-if (lambda (conversation)
                                            (find-if (lambda (a)
                                                       (string= (api-client:id conversation)
                                                                a))
                                                     all-conversations-id))
                                          new-conversations)))
    (loop for new-conversation in new-conversations do
         (let ((root-id (client:conversation-root-id new-conversation)))
           (when (not (db:conversation-root-captured-p root-id))
             (db:add-conversation (api-client:id new-conversation)
                                  root-id))))))

(defun fetch-conversations (message-root-id conversation-folder)
  (let* ((conversation-tree (api-client:expand-conversations-tree message-root-id))
         (event             (make-instance 'update-timeline-event
                                           :payload       conversation-tree
                                           :timeline-type db:+default-converation-timeline+
                                           :folder        conversation-folder
                                           :localp        nil)))
           (push-event event)
    conversation-tree))

(defmethod process-event ((object update-conversations-event))
  (with-accessors ((new-timeline new-timeline)
                   (new-folder  new-folder)) object
    (with-notify-errors
      (add-new-conversations)
      (let* ((all-conversations (db:all-conversations)))
        (loop for conversation in all-conversations do
             (let* ((conversation-root   (db:row-conversation-root-status-id conversation))
                    (conversation-folder (db:row-conversation-folder         conversation)))
               (fetch-conversations conversation-root conversation-folder)))
        ;; refresh-ui
        (let ((refresh-thread (make-instance 'refresh-thread-windows-event
                                             :new-timeline new-timeline
                                             :new-folder   new-folder))
              (refresh-conversation
               (make-instance 'refresh-conversations-window-event)))
          (push-event refresh-thread)
          (push-event refresh-conversation))))))

(defclass change-conversation-name-event (program-event)
  ((old-name
    :initform nil
    :initarg  :old-name
    :accessor old-name)
   (new-name
    :initform nil
    :initarg  :new-name
    :accessor new-name)))

(defmethod process-event ((object change-conversation-name-event))
  (db:change-conversation-name (old-name object)
                               (new-name object)))

(defclass refresh-conversations-window-event (program-event) ())

(defmethod process-event ((object refresh-conversations-window-event))
  (conversations-window:resync-rows-db specials:*conversations-window*))

(defclass ignore-conversations-event (program-event) ())

(defmethod process-event ((object ignore-conversations-event))
  (when-let* ((selected-row  (line-oriented-window:selected-row
                              specials:*conversations-window*))
              (folder        (line-oriented-window:normal-text selected-row))
              (refresh-event (make-instance 'refresh-conversations-window-event)))
    (db:ignore-conversation folder)))

(defclass delete-conversations-event (program-event) ())

(defmethod process-event ((object delete-conversations-event))
  (when-let* ((selected-row  (line-oriented-window:selected-row
                              specials:*conversations-window*))
              (fields        (line-oriented-window:selected-row-fields
                              specials:*conversations-window*))
              (folder        (line-oriented-window:normal-text selected-row))
              (id            (db:conversation-id fields))
              (refresh-event (make-instance 'refresh-conversations-window-event)))
    (with-notify-errors
      (api-client:delete-conversation id)
      (db:delete-conversation folder))))

(defclass report-status-event (program-event)
  ((status-id
    :initform nil
    :initarg  :status-id
    :accessor status-id)
   (account-id
    :initform nil
    :initarg  :account-id
    :accessor account-id)
   (comment
    :initform nil
    :initarg  :comment
    :accessor comment)
   (forwardp
    :initform nil
    :initarg  :forwardp
    :accessor forwardp)))

(defmethod process-event ((object report-status-event))
  (with-accessors ((status-id  status-id)
                   (account-id account-id)
                   (comment    comment)
                   (forwardp   forwardp)) object
    (with-notify-errors
      (api-client:make-report account-id status-id comment forwardp))))

(defclass add-crypto-data-event (program-event)
  ((username
    :initform nil
    :initarg  :username
    :accessor username)
   (key
    :initform nil
    :initarg  :key
    :accessor key)))

(defmethod process-event ((object add-crypto-data-event))
  (with-accessors ((username username)
                   (key      key)) object
    (db:import-crypto-data (db:acct->id username)
                           key)))

;;;; end events

(defun dispatch-program-events ()
  (when (event-available-p)
    (process-event (pop-event))))
