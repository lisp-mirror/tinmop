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

(in-package :ui-goodies)

(defun boolean-input-accepted-p (user-input)
  (string-equal user-input (_ "y")))

(defun quit-program ()
  "This  is not the right function to quit the program, use
  'clean-close-program'."
  (hooks:run-hooks 'hooks:*before-quit*)
  (db-utils:close-db)
  (os-utils:exit-program))

(defun clean-close-program ()
  "Use this to close the program"
  (flet ((on-input-complete (maybe-accepted)
           (if (boolean-input-accepted-p maybe-accepted)
               (let ((delete-event (make-instance 'delete-all-status-event))
                     (quit-event   (make-instance 'quit-program-event)))
                 (push-event delete-event)
                 (push-event quit-event))
               (quit-program))))
    (let ((delete-count (db:count-status-marked-to-delete)))
      (if (> delete-count 0)
          (ask-string-input #'on-input-complete
                            :prompt (format nil
                                            (n_ "Delete ~a message? [y/N] "
                                                "Delete ~a messages? [y/N] "
                                                delete-count)
                                            delete-count))
          (quit-program)))))

(defun notify (message &key (life nil) (as-error nil))
  (let ((event (make-instance 'notify-user-event
                              :life         life
                              :notify-error as-error
                              :payload      message)))
  (push-event event)))

(defun notify-procedure (procedure starting-message
                         &key
                           (ending-message (_ "Task completed"))
                           (life-start     nil)
                           (life-end       nil))
  (bt:make-thread (lambda ()
                    (notify starting-message :life life-start)
                    (funcall procedure)
                    (notify ending-message :life life-end))))

(defmacro with-blocking-notify-procedure ((starting-message
                                           &optional (ending-message `(_ "Task completed")))
                                          &body body)
  `(progn
     (notify ,starting-message)
     ,@body
     (notify ,ending-message)))

(defun info-dialog (message &key (buttons nil) (title (_ "Information")))
  (let ((event (make-instance 'info-dialog-event
                              :buttons buttons
                              :title   title
                              :payload message)))
    (push-event event)))

(defun error-dialog (message &key (buttons nil) (title (_ "Error")))
  (let ((event (make-instance 'error-dialog-event
                              :buttons buttons
                              :title   title
                              :payload message)))
    (push-event event)))

(defun info-dialog-immediate (message
                              &key
                                (buttons nil)
                                (title (_ "Information"))
                                (append-ok-button t))
  (let ((dialog-window (windows:make-info-message-dialog specials:*main-window*
                                                         title
                                                         message
                                                         buttons
                                                         append-ok-button)))
    (windows:menu-select dialog-window)))

(defun error-dialog-immediate (message
                               &key
                                 (buttons nil)
                                 (title (_ "Error"))
                                 (append-ok-button t))
  (let ((dialog-window (windows:make-error-message-dialog specials:*main-window*
                                                         title
                                                         message
                                                         buttons
                                                         append-ok-button)))
    (windows:menu-select dialog-window)))

(defun input-dialog-immediate (message)
  (windows:make-input-dialog specials:*main-window* specials:*main-window* message))

(defun error-message (message)
  (let ((event (make-instance 'error-message-event
                              :payload message)))
    (push-event event)))

(defun info-message (message)
  (let ((event (make-instance 'info-message-event
                              :payload message)))
    (push-event event)))

(defun confirm-file-overwrite-dialog-immediate (filepath)
  (let ((res (info-dialog-immediate (format nil
                                  (_ "File \"~a\" exists, overwrite?")
                                  filepath)
                          :buttons (list (_ "Cancel")))))
    (string= res +menu-button-ok+)))

(defun confirm-dialog-immediate (message)
  (let ((res (info-dialog-immediate message
                          :buttons (list (_ "Cancel")))))
    (string= res +menu-button-ok+)))

(defun request-error-window (condition-object)
  (error-dialog (format nil
                        (_ "Request failed: error code ~d message \"~a\"")
                        (tooter:code    condition-object)
                        (tooter:message condition-object))))

(defun ask-string-input (on-input-complete-fn
                         &key
                           (initial-value nil)
                           (prompt        +default-command-prompt+)
                           (complete-fn   #'complete:directory-complete))
  (flet ((thread-fn ()
           (let ((event (make-instance 'ask-user-input-string-event
                                       :initial-value initial-value
                                       :complete-fn   complete-fn
                                       :prompt        prompt
                                       :payload       (box:dbox nil))))
             (with-accessors ((lock               lock)
                              (condition-variable condition-variable)) event
               (push-event event)
               (with-lock (lock)
                 (bt:condition-wait condition-variable lock)
                 (funcall on-input-complete-fn (box:dunbox (payload event))))))))
      (bt:make-thread #'thread-fn)))

(defun thread-go-up ()
  (thread-window:go-message-up specials:*thread-window*))

(defun thread-go-down ()
  (thread-window:go-message-down specials:*thread-window*))

(defun thread-goto-message ()
  "Jump to message"
  (flet ((on-input-complete (index)
           (when-let* ((index-as-number (num:safe-parse-number index))
                       (event          (make-instance 'thread-goto-message
                                                       :payload index-as-number)))
             (push-event event))))
    (ask-string-input #'on-input-complete :prompt (_ "Jump to message: "))))

(defun thread-goto-first-message ()
  "Jump to first message"
  (thread-window:goto-first-message specials:*thread-window*))

(defun thread-goto-last-message ()
  "Jump to last message"
  (thread-window:goto-last-message specials:*thread-window*))

(defun thread-search-message-body (direction)
  "Search in messages body"
  (flet ((on-input-complete (text-looking-for)
           (let ((event (make-instance 'thread-search-message-body-event
                                       :payload          text-looking-for
                                       :search-direction direction)))
             (push-event event))))
    (ask-string-input #'on-input-complete :prompt (_ "Search key: "))))

(defun thread-search-next-message-body ()
  "Search next matching message's body"
  (thread-search-message-body :next))

(defun thread-search-previous-message-body ()
  "Search previous matching messages body"
  (thread-search-message-body :previous))

(defun thread-search-message-meta (direction)
  "Search in messages metadata.

Metadata includes:


- spoiler-text (subject of message)
- tags
- username"
  (flet ((on-input-complete (text-looking-for)
           (let ((event (make-instance 'thread-search-message-meta-event
                                       :payload          text-looking-for
                                       :search-direction direction)))
             (push-event event))))
    (ask-string-input #'on-input-complete :prompt (_ "Search key: "))))

(defun thread-search-next-message-meta ()
  "Search next matching message's metadata

Metadata includes:


- spoiler-text (subject of message)
- tags
- username"

  (thread-search-message-meta :next))

(defun thread-search-previous-message-meta ()
  "Search previous matching message's metadata

Metadata includes:


- spoiler-text (subject of message)
- tags
- username"

  (thread-search-message-meta :previous))

(defun thread-search-next-unread-message ()
  "Jump to next unread message"
  (thread-window:search-next-unread specials:*thread-window*))

(defun thread-open-selected-message ()
  "Open selected message"
  (thread-window:open-message specials:*thread-window*))

(defun thread-mark-delete-selected-message ()
  "Mark selected message for deletion"
  (thread-window:mark-selected-message-to-delete specials:*thread-window*
                                                 :move-down-selected-message t))

(defun thread-mark-prevent-delete-selected-message ()
  "Unmark selected message for deletion"
  (thread-window:mark-selected-message-prevent-delete specials:*thread-window*
                                                      :move-down-selected-message t))

(defun subscribe-to-hash ()
  "Subscribe to hashtag"
  (flet ((on-input-complete (tags)
           (let ((event          (make-instance 'subscribe-tags-event
                                       :payload tags))
                 (refresh-event  (make-instance 'refresh-tag-window-event)))
             (push-event refresh-event)
             (push-event event))))
    (let* ((selected-row (line-oriented-window:selected-row-fields specials:*thread-window*))
           (tags         (and selected-row
                              (db:row-message-tags selected-row))))
      (ask-string-input #'on-input-complete
                        :initial-value tags
                        :prompt        (_ "Subscribe to: ")))))

(defun unsubscribe-to-hash ()
  "Unsubscribe to hashtag"
  (flet ((on-input-complete (tag)
           (db-utils:with-ready-database (:connect nil)
             (when (and (string-not-empty-p tag)
                        (> (length tag)
                           (length +folder-tag-prefix+)))
               (let ((unsubscribe-event (make-instance 'unsubscribe-tags-event
                                                       :payload (db:folder-name->tag tag)))
                     (refresh-event  (make-instance 'refresh-tag-window-event)))
                 (push-event unsubscribe-event)
                 (push-event refresh-event))))))
    (ask-string-input #'on-input-complete
                      :initial-value +folder-tag-prefix+
                      :prompt        (_ "Unsubscribe to: ")
                      :complete-fn   #'complete:tags-complete)))

(defun message-scroll-up ()
  (message-window:scroll-up specials:*message-window*))

(defun message-scroll-down ()
  (message-window:scroll-down specials:*message-window*))

(defun message-scroll-begin ()
  (message-window:scroll-begin specials:*message-window*))

(defun message-scroll-end ()
  (message-window:scroll-end specials:*message-window*))

(defun message-scroll-next-page ()
  (message-window:scroll-next-page specials:*message-window*))

(defun message-scroll-previous-page ()
  (message-window:scroll-previous-page specials:*message-window*))

(defun message-search-regex ()
  "Search regular expression in message"
  (flet ((on-input-complete (regex)
           (let ((event (make-instance 'search-regex-message-content-event
                                       :payload regex)))
             (push-event event))))
    (ask-string-input #'on-input-complete :prompt (_ "Search key: "))))

(defun give-focus (win info-change-focus-message &rest windows-lose-focus)
  (setf (main-window:focused-window specials:*main-window*)
        win)
  (setf (windows:in-focus win) t)
  (loop for win in windows-lose-focus when win do
       (setf (windows:in-focus win) nil))
  (windows:draw-all)
  (when info-change-focus-message
    (info-message info-change-focus-message)))

(defmacro gen-focus-to-window (function-suffix window-get-focus
                               &key
                                 (info-change-focus-message (_ "Focus changed"))
                                 (windows-lose-focus nil)
                                 (documentation nil))
  `(defun ,(misc:format-fn-symbol t "focus-to-~a" function-suffix) (&key (print-message t))
     ,documentation
     (give-focus ,window-get-focus
                 (if print-message
                     ,info-change-focus-message
                     nil)
                 ,@windows-lose-focus)))

(gen-focus-to-window thread-window
                     specials:*thread-window*
                     :documentation      "Move focus on thread window"
                     :info-change-focus-message (_ "Focus passed on threads window")
                     :windows-lose-focus (specials:*open-message-link-window*
                                          specials:*open-attach-window*
                                          specials:*conversations-window*
                                          specials:*tags-window*
                                          specials:*send-message-window*
                                          specials:*message-window*
                                          specials:*follow-requests-window*))

(gen-focus-to-window message-window
                     specials:*message-window*
                     :documentation      "Move focus on message window"
                     :info-change-focus-message (_ "Focus passed on message window")
                     :windows-lose-focus (specials:*open-message-link-window*
                                          specials:*open-attach-window*
                                          specials:*conversations-window*
                                          specials:*tags-window*
                                          specials:*thread-window*
                                          specials:*send-message-window*
                                          specials:*follow-requests-window*))


(gen-focus-to-window send-message-window
                     specials:*send-message-window*
                     :documentation      "Move focus on send message window"
                     :info-change-focus-message (_ "Focus passed on send message window")
                     :windows-lose-focus (specials:*open-message-link-window*
                                          specials:*open-attach-window*
                                          specials:*conversations-window*
                                          specials:*tags-window*
                                          specials:*thread-window*
                                          specials:*message-window*
                                          specials:*follow-requests-window*))

(gen-focus-to-window follow-requests-window
                     specials:*follow-requests-window*
                     :documentation      "Move focus on follow requests window"
                     :info-change-focus-message (_ "Focus passed on follow requests window")
                     :windows-lose-focus (specials:*open-message-link-window*
                                          specials:*open-attach-window*
                                          specials:*conversations-window*
                                          specials:*tags-window*
                                          specials:*thread-window*
                                          specials:*message-window*
                                          specials:*send-message-window*))

(gen-focus-to-window tags-window
                     specials:*tags-window*
                     :documentation      "Move focus on tags window"
                     :info-change-focus-message (_ "Focus passed on tags window")
                     :windows-lose-focus (specials:*open-message-link-window*
                                          specials:*open-attach-window*
                                          specials:*conversations-window*
                                          specials:*follow-requests-window*
                                          specials:*thread-window*
                                          specials:*message-window*
                                          specials:*send-message-window*))
(gen-focus-to-window conversations-window
                     specials:*conversations-window*
                     :documentation      "Move focus on conversations window"
                     :info-change-focus-message (_ "Focus passed on conversation window")
                     :windows-lose-focus (specials:*open-message-link-window*
                                          specials:*open-attach-window*
                                          specials:*tags-window*
                                          specials:*follow-requests-window*
                                          specials:*thread-window*
                                          specials:*message-window*
                                          specials:*send-message-window*))

(gen-focus-to-window open-attach-window
                     specials:*open-attach-window*
                     :documentation      "Move focus on open-attach window"
                     :info-change-focus-message (_ "Focus passed on attach window")
                     :windows-lose-focus (specials:*open-message-link-window*
                                          specials:*conversations-window*
                                          specials:*tags-window*
                                          specials:*follow-requests-window*
                                          specials:*thread-window*
                                          specials:*message-window*
                                          specials:*send-message-window*))

(gen-focus-to-window open-message-link-window
                     specials:*open-message-link-window*
                     :documentation      "Move focus on open-link window"
                     :info-change-focus-message (_ "Focus passed on link window")
                     :windows-lose-focus (specials:*conversations-window*
                                          specials:*open-attach-window*
                                          specials:*tags-window*
                                          specials:*follow-requests-window*
                                          specials:*thread-window*
                                          specials:*message-window*
                                          specials:*send-message-window*))

(defun print-quick-help ()
  "Print a quick help"
  (keybindings:print-help specials:*main-window*))

(defun move-message-tree ()
  "Move messages tree"
  (flet ((on-input-complete (new-folder)
           (let ((move-event     (make-instance 'move-selected-tree-event
                                                :new-folder new-folder))
                 (refresh-event  (make-instance 'refresh-thread-windows-event)))
             (if (string-not-empty-p new-folder)
                 (with-blocking-notify-procedure
                     ((format nil (_ "Saving messages in ~s") new-folder)
                      (format nil (_ "Saved message in ~s") new-folder))
                   (push-event move-event)
                   (push-event refresh-event))
                 (error-message (_ "No folder specified."))))))
    (ask-string-input #'on-input-complete
                      :prompt      (_ "Move to folder: ")
                      :complete-fn #'complete:folder-complete)))

(defun change-folder ()
  "Change folder"
  (flet ((on-input-complete (new-folder)
           (db-utils:with-ready-database (:connect nil)
             (let ((refresh-event   (make-instance 'refresh-thread-windows-event
                                                   :new-folder new-folder))
                   (folder-exists-p (db:folder-exists-p new-folder)))
               (if (string-not-empty-p new-folder)
                   (if folder-exists-p
                       (push-event refresh-event)
                       (error-message (format nil
                                              (_ "Folder ~s does not exists.")
                                              new-folder)))
                   (error-message (_ "No folder specified.")))))))
    (ask-string-input #'on-input-complete
                      :prompt      (_ "Change folder: ")
                      :complete-fn #'complete:folder-complete)))

(defun change-timeline ()
  "Change timeline"
  (let ((folder (thread-window:timeline-folder specials:*thread-window*)))
    (flet ((on-input-complete (new-timeline)
             (let* ((refresh-event (make-instance 'refresh-thread-windows-event
                                                  :new-timeline new-timeline)))
               (cond
                 ((string-empty-p new-timeline)
                  (error-message (_ "No timeline specified.")))
                 ((db:hidden-recipient-p new-timeline)
                  (error-message (_ "This timeline is protected.")))
                 (t
                  (push-event refresh-event))))))
      (ask-string-input #'on-input-complete
                        :prompt      (_ "Change timeline: ")
                        :complete-fn (complete:timeline-complete-fn folder)))))

(defun timeline->kind (timeline)
  "Return two  values: the kind of timeline (on the server) to fetch
and if fetch local (again, to server) statuses only."
  (cond
    ((string= timeline db:+federated-timeline+)
     (values :public nil))
    ((string= timeline db:+local-timeline+)
     (values :public t))
    ((string= timeline db:+home-timeline+)
     (values :home nil))))

(defun update-current-timeline ()
  "Update current timeline

This  command  also checks  notifications  about  mentioning the  user
and (if  such mentions  exists) download the  mentioning toots  in the
folder \"mentions\"."
  (let* ((timeline (thread-window:timeline-type specials:*thread-window*))
         (folder   (thread-window:timeline-folder specials:*thread-window*))
         (max-id   (db:last-pagination-status-id-timeline-folder timeline folder)))
    (multiple-value-bind (kind localp)
        (timeline->kind timeline)
      (flet ((update ()
               (with-notify-errors
                 (client:update-timeline timeline
                                         kind
                                         folder
                                         :min-id max-id
                                         :local  localp)
                 (let ((update-mentions-event (make-instance 'update-mentions-event))
                       (refresh-event         (make-instance 'refresh-thread-windows-event)))
                   ;; updating home also triggers the checks for mentions
                   (when (eq kind :home)
                     (push-event update-mentions-event))
                   (push-event refresh-event)))))
        (notify-procedure #'update
                          (_ "Downloading messages.")
                          :ending-message (_ "Messages downloaded.")
                          :life-start     (* (swconf:config-notification-life) 5))))))

(defun update-current-timeline-backwards ()
  "Update current timeline backwards

Starting from the oldest toot and going back."
  (let* ((timeline          (thread-window:timeline-type specials:*thread-window*))
         (folder            (thread-window:timeline-folder specials:*thread-window*))
         (min-id            (db:first-pagination-status-id-timeline-folder timeline folder)))
    (multiple-value-bind (kind localp)
        (timeline->kind timeline)
      (flet ((update ()
               (with-notify-errors
                 (client:update-timeline timeline
                                         kind
                                         folder
                                         :max-id min-id
                                         :local  localp)
                 (let ((refresh-event (make-instance 'refresh-thread-windows-event)))
                   (push-event refresh-event)))))
        (notify-procedure #'update
                          (_ "Downloading messages.")
                          :ending-message (_ "Messages downloaded.")
                          :life-start     (* (swconf:config-notification-life) 5))))))

(defun refresh-thread ()
  "Check and download a thread

Force the checking for new message in the thread the selected message belong."
  (when-let* ((selected-message (line-oriented-window:selected-row-fields specials:*thread-window*))
              (timeline         (thread-window:timeline-type specials:*thread-window*))
              (folder           (thread-window:timeline-folder specials:*thread-window*))
              (status-id        (db:row-message-status-id selected-message))
              (expand-event     (make-instance 'expand-thread-event
                                               :new-folder   folder
                                               :new-timeline timeline
                                               :status-id    status-id))
              (refresh-event  (make-instance 'refresh-thread-windows-event
                                             :priority +minimum-event-priority+)))
    (with-blocking-notify-procedure ((_ "Expanding thread"))
      (push-event expand-event)
      (push-event refresh-event))))

(defun refresh-tags ()
  "Update messages for subscribed tags"
  (let* ((all-tags        (db:all-subscribed-tags-name))
         (all-paginations (db:all-tag-paginations-status all-tags)))
    (flet ((update ()
             (client:update-subscribed-tags all-tags all-paginations)
             (let ((update-got-message-event
                    (make-instance 'tag-mark-got-messages-event))
                   (notify-event
                    (make-instance 'notify-fetched-new-tag-messages-event))
                   (update-subscribed-event
                    (make-instance 'update-last-refresh-subscribe-tags-event))
                   (refresh-window-event  (make-instance 'refresh-tag-window-event)))
               (push-event update-got-message-event)
               (push-event notify-event)
               (push-event update-subscribed-event)
               (push-event refresh-window-event))))
      (notify-procedure #'update
                        (_ "Downloading tags messages.")
                        :ending-message (_ "Messages downloaded.")
                        :life-start     (* (swconf:config-notification-life) 5)))))

(defun confirm-selected-row-action (message)
  (when-let* ((selected-row  (line-oriented-window:selected-row-fields specials:*thread-window*))
              (status-id     (db:row-message-status-id selected-row))
              (confirmedp    (confirm-dialog-immediate message)))
    (values status-id selected-row)))

(defun favourite-selected-status ()
  "Favourite selected status"
  (multiple-value-bind (selected-id selected-message)
      (confirm-selected-row-action (_ "Favorite this message?"))
    (when selected-id
      (let ((selected-index (db:row-message-index selected-message)))
        (flet ((update ()
                 (let* ((favourite-event (make-instance 'favourite-status-event
                                                        :payload       selected-id
                                                        :message-index selected-index)))
                   (push-event favourite-event))))
          (notify-procedure #'update
                            (_ "Favouring message.")
                            :ending-message (_ "Favoured message.")))))))

(defun unfavourite-selected-status ()
  "Unfavourite selected status"
  (multiple-value-bind (selected-id selected-message)
      (confirm-selected-row-action  (_ "Remove this message from your favourites?"))
    (when selected-id
      (let ((selected-index (db:row-message-index selected-message)))
        (flet ((update ()
                 (let* ((unfavourite-event (make-instance 'unfavourite-status-event
                                                          :payload selected-id
                                                          :message-index selected-index)))
                   (push-event unfavourite-event))))
          (notify-procedure #'update
                            (_ "Unfavouring message.")
                            :ending-message (_ "Unfavoured message.")))))))

(defun boost-selected-status ()
  "Boost selected status"
  (multiple-value-bind (selected-id selected-message)
      (confirm-selected-row-action (_ "Boost this message?"))
    (when selected-id
      (let ((selected-index (db:row-message-index selected-message)))
        (flet ((update ()
                 (let* ((reblog-event (make-instance 'reblog-status-event
                                                     :payload       selected-id
                                                     :message-index selected-index)))
                   (push-event reblog-event))))
          (notify-procedure #'update
                            (_ "Boosting message.")
                            :ending-message (_ "Boosted message.")))))))

(defun unboost-selected-status ()
  "Unboost selected status"
  (multiple-value-bind (selected-id selected-message)
      (confirm-selected-row-action  (_ "Unboost this message?"))
    (when selected-id
      (let ((selected-index (db:row-message-index selected-message)))
        (flet ((update ()
                 (let* ((unreblog-event (make-instance 'unreblog-status-event
                                                       :payload selected-id
                                                       :message-index selected-index)))
                   (push-event unreblog-event))))
          (notify-procedure #'update
                            (_ "Uboosting message.")
                            :ending-message (_ "Unboosted message.")))))))

(defun ignore-user ()
  "Ignore user"
  (when-let* ((selected-row (line-oriented-window:selected-row-fields
                             specials:*thread-window*))
              (username     (db:row-message-username selected-row))
              (selected-id  (confirm-selected-row-action (format nil
                                                                (_ "Ignore ~s?")
                                                                username))))
    (with-blocking-notify-procedure
        ((format nil (_ "Ignoring ~s") username)
         (format nil (_ "User ~s ignored") username))
      (db:ignore-status-author selected-id))))

(defun unignore-user ()
  "Unignore user"
  (flet ((on-input-complete (username)
           (let* ((event (make-instance 'unignore-user-event
                                        :payload username)))
             (if (string-not-empty-p username)
                 (push-event event)
                 (error-message (_ "No username specified."))))))
    (ask-string-input #'on-input-complete
                      :prompt      (_ "Unignore username: ")
                      :complete-fn #'complete:ignored-username-complete)))

(defun attach-move (amount)
  (ignore-errors
    (line-oriented-window:unselect-all specials:*send-message-window*)
    (line-oriented-window:row-move     specials:*send-message-window* amount)
    (draw specials:*send-message-window*)))

(defun attach-go-down ()
  (attach-move 1))

(defun attach-go-up ()
  (attach-move -1))

(defun attach-delete ()
  "Delete an attach"
  (line-oriented-window:selected-row-delete specials:*send-message-window*)
  (win-clear specials:*send-message-window*)
  (draw specials:*send-message-window*))

(defun attach-add ()
  "Add an attach"
  (flet ((on-add-attach (attach-path)
           (if (string-not-empty-p attach-path)
               (let ((add-event (make-instance 'send-message-add-attachment-event
                                               :payload attach-path)))
                 (if (fs:file-exists-p attach-path)
                     (push-event add-event)
                     (error-message (format nil (_ "File ~s does not exists.") attach-path)))
                 (attach-add))
               (info-message (_ "Message ready to be sent")))))
    (ask-string-input #'on-add-attach
                      :prompt      (_ "Add attachment: ")
                      :complete-fn #'complete:directory-complete)))

(defun change-subject ()
  "Change subject"
  (flet ((on-change-subject (new-subject)
           (let* ((event (make-instance 'send-message-change-subject-event
                                        :payload new-subject)))
             (push-event event))))
    (ask-string-input #'on-change-subject
                      :prompt (_ "New subject: "))))

(defun change-visibility ()
  "Change message's visibility"
  (flet ((on-change-visibility (new-visibility)
           (let* ((event (make-instance 'send-message-change-visibility-event
                                        :payload new-visibility)))
             (push-event event))))
    (ask-string-input #'on-change-visibility
                      :prompt      (_ "New visibility: ")
                      :complete-fn #'complete:visibility-complete)))

(defmacro close-window-and-return-to-threads (window-to-close)
  `(progn
     (win-close ,window-to-close)
     (setf ,window-to-close nil)
     (focus-to-thread-window)))

(defun cancel-send-message ()
  "Cancel sending operation"
  (close-window-and-return-to-threads specials:*send-message-window*))

(defun edit-message-body ()
  "Edit message"
  (when (and specials:*send-message-window*
             (sending-message:message-data specials:*send-message-window*))
    (with-accessors ((body       sending-message:body)
                     (subject    sending-message:subject)
                     (reply-to   sending-message:reply-to)
                     (visibility sending-message:visibility))
        (sending-message:message-data specials:*send-message-window*)
      (let ((temp-file (fs:temporary-filename)))
        (with-open-file (stream temp-file
                                :direction         :output
                                :if-exists         :supersede
                                :if-does-not-exist :error)
          (write-sequence body stream))
        (croatoan:end-screen)
        (os-utils:open-with-editor temp-file)
        (setf body (fs:slurp-file temp-file))))))

(defun close-send-message-window ()
  "Close message window and cancel operation"
  (cancel-send-message))

(defparameter *message-to-send* nil)

(defun message-exceeds-server-limit-p (body)
  (if (> (length body)
         (swconf:max-message-length))
      (- (length body)
         (swconf:max-message-length))
      nil))

(defun exceeding-characters-notify (exceeding)
  (error-message (format nil
                         (n_ "Your message is ~a character too long."
                             "Your message is ~a characters too long."
                             exceeding)
                         exceeding)))

(defun compose-message (&optional reply-id subject (visibility +status-public-visibility+))
  "Compose a new message"
  (setf *message-to-send* (make-instance 'sending-message:message-ready-to-send
                                         :visibility visibility
                                         :reply-to   reply-id
                                         :subject    subject))
  (labels ((open-window ()
             (let ((event (make-instance 'open-send-message-window-event
                                         :payload *message-to-send*)))
               (push-event event)))
           (add-subject ()
             (flet ((on-add-subject (new-subject)
                      (setf (sending-message:subject *message-to-send*)
                            new-subject)
                      (open-window)
                      (attach-add)))
               (let ((old-subject (sending-message:subject *message-to-send*)))
                 (if (string-empty-p old-subject)
                     (ask-string-input #'on-add-subject
                                       :prompt (_ "Add subject: "))
                     (progn
                       (open-window)
                       (attach-add))))))
           (prepare-reply-body (file)
             (when reply-id
               ;; we  do  not need  to  take  into account  folder  or
               ;; timeline here as  the id in unique  identifier for a
               ;; single message *content*  regardless of the position
               ;; in db (folder, timeline).
               (when-let* ((message        (db:find-message-id reply-id))
                           (reply-username (db:row-message-username message))
                           (quoted-text    (db:row-message-rendered-text message))
                           (lines          (split-lines quoted-text))
                           (quote-mark     (swconf:quote-char))
                           (quoted-lines   (mapcar (lambda (a) (strcat quote-mark a))
                                                   lines)))
                 (with-open-file (stream file
                                         :if-exists    :append
                                         :direction    :output
                                         :element-type 'character)
                   (format stream "~a~%" (msg-utils:add-mention-prefix reply-username))
                   (loop for line in quoted-lines do
                        (format stream "~a~%" line))))))
           (add-body ()
             (let ((temp-file           (fs:temporary-filename))
                   (reference-open-file (get-universal-time)))
               (prepare-reply-body temp-file)
               (croatoan:end-screen)
               (os-utils:open-with-editor temp-file)
               (when (and (> (fs:file-size temp-file)
                             0)
                          (> (fs:get-stat-mtime temp-file)
                             reference-open-file))
                 (let ((body (fs:slurp-file temp-file)))
                   (setf (sending-message:body *message-to-send*) body)
                   (add-subject))))))
    (add-body)))

(defun reply-message ()
  "Reply to message"
  (when-let* ((win              specials:*thread-window*)
              (selected-message (line-oriented-window:selected-row-fields win))
              (username         (db:row-message-username   selected-message))
              (visibility       (db:row-message-visibility selected-message))
              (reply-id         (db:row-message-status-id  selected-message)))
    (let ((subject (db:row-message-subject selected-message)))
      (compose-message reply-id subject visibility))))

(defun send-message ()
  "Send message"
  (when (and specials:*send-message-window*
             (sending-message:message-data specials:*send-message-window*))
    (let ((data               (sending-message:message-data specials:*send-message-window*))
          (attachments        (line-oriented-window:rows specials:*send-message-window*))
          (max-allowed-attach (swconf:max-attachments-allowed)))
        (if (> (length attachments)
               max-allowed-attach)
            (error-message (format nil
                                   (_ "The maximum allowed number of media is ~a.")
                                   (swconf:max-attachments-allowed)))
            (progn
              (notify (_ "Sending message"))
              (let ((event (make-instance 'send-message-event
                                          :use-ui-notification t
                                          :payload             data)))
                (push-event event)))))))

(defun open-message-attach ()
  "Open message links window

Browse and optionally open the links the messages contains."
  (when-let* ((win              specials:*thread-window*)
              (selected-message (line-oriented-window:selected-row-fields win)))
    (open-attach-window:init (db:row-message-status-id selected-message))
    (focus-to-open-attach-window)))

(defun open-message-attach-move (amount)
  (ignore-errors
    (line-oriented-window:unselect-all specials:*open-attach-window*)
    (line-oriented-window:row-move     specials:*open-attach-window* amount)
    (draw specials:*open-attach-window*)))

(defun open-message-attach-go-down ()
  (open-message-attach-move 1))

(defun open-message-attach-go-up ()
  (open-message-attach-move -1))

(defun open-message-attach-perform-opening ()
  (when-let* ((selected-line (line-oriented-window:selected-row specials:*open-attach-window*))
              (url           (line-oriented-window:normal-text selected-line)))
  (open-attach-window:open-attachment url)))

(defun close-open-attach-window ()
  (close-window-and-return-to-threads specials:*open-attach-window*))

(defun open-message-link ()
  "Open message attachments window"
  (when-let* ((win              specials:*thread-window*)
              (selected-message (line-oriented-window:selected-row-fields win)))
    (open-message-link-window:init (db:row-message-status-id selected-message))
    (focus-to-open-message-link-window)))

(defun open-message-link-move (amount)
  (ignore-errors
    (line-oriented-window:unselect-all specials:*open-message-link-window*)
    (line-oriented-window:row-move     specials:*open-message-link-window* amount)
    (draw specials:*open-message-link-window*)))

(defun open-message-link-go-down ()
  (open-message-link-move 1))

(defun open-message-link-go-up ()
  (open-message-link-move -1))

(defun open-message-link-perform-opening ()
  (when-let* ((selected-line (line-oriented-window:selected-row specials:*open-message-link-window*))
              (url           (line-oriented-window:normal-text selected-line)))
  (open-message-link-window:open-message-link url)))

(defun close-open-message-link-window ()
  (close-window-and-return-to-threads specials:*open-message-link-window*))

(defun prompt-for-username (prompt complete-function event
                            notify-starting-message
                            notify-ending-message)
  (flet ((on-input-complete (username)
           (when (string-not-empty-p username)
             (with-blocking-notify-procedure
                 ((format nil notify-starting-message username)
                  (format nil notify-ending-message   username))
               (let ((event (make-instance event
                                           :payload username)))
                 (push-event event))))))
    (ask-string-input #'on-input-complete
                      :prompt      prompt
                      :complete-fn complete-function)))
(defun follow-user ()
  "Follow user"
  (prompt-for-username (_ "Follow: ")
                       #'complete:unfollowed-user-complete
                       'follow-user-event
                       (_ "Following ~a")
                       (_ "Followed  ~a")))

(defun unfollow-user ()
  "Unfollow user"
  (prompt-for-username (_ "Unfollow: ")
                       #'complete:followed-user-complete
                       'unfollow-user-event
                       (_ "Unfollowing ~a")
                       (_ "Unfollowed  ~a")))

(defun follow-request-move (amount)
  (ignore-errors
    (line-oriented-window:unselect-all specials:*follow-requests-window*)
    (line-oriented-window:row-move     specials:*follow-requests-window* amount)
    (draw specials:*follow-requests-window*)))

(defun follow-request-go-down ()
  (follow-request-move 1))

(defun follow-request-go-up ()
  (follow-request-move -1))

(defun follow-request-delete ()
  (line-oriented-window:selected-row-delete specials:*follow-requests-window*)
  (draw specials:*follow-requests-window*))

(defun start-follow-request-processing ()
  (let ((event (make-instance 'open-follow-requests-window-event)))
    (push-event event)))

(defun close-follow-requests-window ()
  (close-window-and-return-to-threads specials:*follow-requests-window*))

(defun cancel-follow-requests ()
  (close-follow-requests-window))

(defun process-follow-requests ()
  (when (confirm-dialog-immediate (_ "Confirm operation?"))
    (follow-requests:process-requests))
  (close-follow-requests-window))

(defun tag-move (amount)
  (ignore-errors
    (line-oriented-window:unselect-all specials:*tags-window*)
    (line-oriented-window:row-move     specials:*tags-window* amount)
    (draw specials:*tags-window*)))

(defun tag-go-down ()
  (tag-move 1))

(defun tag-go-up ()
  (tag-move -1))

(defun open-tag-folder ()
  "Open tag folder"
  (when-let* ((selected-line  (line-oriented-window:selected-row specials:*tags-window*))
              (tag            (line-oriented-window:normal-text  selected-line))
              (refresh-thread (make-instance 'refresh-thread-windows-event
                                             :new-timeline db:+default-tag-timeline+
                                             :new-folder   tag))
              (refresh-tags   (make-instance 'refresh-tag-window-event)))
    (db:unmark-tag-got-new-messages (db:folder-name->tag tag))
    (push-event refresh-tags)
    (push-event refresh-thread)))

(defun update-conversations ()
  "Update conversations"
  (flet ((update ()
           (let* ((timeline     (thread-window:timeline-type   specials:*thread-window*))
                  (folder       (thread-window:timeline-folder specials:*thread-window*))
                  (update-event (make-instance 'update-conversations-event
                                               :new-timeline timeline
                                               :new-folder   folder)))
             (push-event update-event))))
    (notify-procedure #'update
                      (_ "Updating conversations.")
                      :ending-message (_ "Conversations updated."))))

(defun open-conversation ()
  "Open conversation"
  (flet ((on-input-complete (folder)
           (let ((refresh-event (make-instance 'refresh-thread-windows-event
                                               :new-timeline db:+default-converation-timeline+
                                               :new-folder   folder)))
             (push-event refresh-event))))
    (ask-string-input #'on-input-complete
                      :prompt        (_ "Open conversation: ")
                      :complete-fn   #'complete:conversation-folder)))

(defun conversation-move (amount)
  (ignore-errors
    (line-oriented-window:unselect-all specials:*conversations-window*)
    (line-oriented-window:row-move     specials:*conversations-window* amount)
    (draw specials:*conversations-window*)))

(defun conversation-go-down ()
  (conversation-move 1))

(defun conversation-go-up ()
  (conversation-move -1))

(defun goto-conversation ()
  (when-let* ((selected-row  (line-oriented-window:selected-row
                              specials:*conversations-window*))
              (folder        (line-oriented-window:normal-text selected-row))
              (refresh-event (make-instance 'refresh-thread-windows-event
                                            :new-timeline db:+default-converation-timeline+
                                            :new-folder   folder)))
    (push-event refresh-event)))

(defparameter *conversation-old-name* nil)

(defun change-conversation-name ()
  "Change conversation's name"
  (setf *conversation-old-name* nil)
  (labels ((add-old-name ()
             (flet ((on-add-old-name (old-name)
                      (when (string-not-empty-p old-name)
                        (setf *conversation-old-name* old-name)
                        (add-new-name))))
               (ask-string-input #'on-add-old-name
                                 :prompt      (_ "Old name: ")
                                 :complete-fn
                                 #'complete:conversation-folder)))
           (add-new-name ()
             (flet ((on-add-new-name (new-name)
                      (db-utils:with-ready-database (:connect nil)
                        (let ((update-event (make-instance 'change-conversation-name-event
                                                           :old-name *conversation-old-name*
                                                           :new-name new-name))
                              (refresh-event
                               (make-instance 'refresh-conversations-window-event)))
                          (when (string-not-empty-p new-name)
                            (if (db:conversation-folder-exists-p new-name)
                                (error-message (format nil
                                                       (_ "A conversation with name ~a already exists.")
                                                       new-name))
                                (progn
                                  (push-event update-event)
                                  (push-event refresh-event))))))))
               (ask-string-input #'on-add-new-name
                                 :prompt      (_ "New name: ")))))
    (add-old-name)))

(defun ignore-conversation ()
  "Ignore conversation"
  (flet ((on-input-complete (maybe-accepted)
           (when (boolean-input-accepted-p maybe-accepted)
             (let ((ignore-event  (make-instance 'ignore-conversations-event))
                   (refresh-event (make-instance 'refresh-conversations-window-event)))
               (push-event ignore-event)
               (push-event refresh-event)))))
    (when-let* ((selected-row (line-oriented-window:selected-row
                                specials:*conversations-window*))
                (folder       (line-oriented-window:normal-text selected-row)))
      (ask-string-input #'on-input-complete
                        :prompt (format nil
                                        (_ "Ignore conversation ~s? [y/N] ")
                                        folder)))))

(defun delete-conversation ()
  "Delete conversation"
  (flet ((on-input-complete (maybe-accepted)
           (when (boolean-input-accepted-p maybe-accepted)
             (let ((delete-event  (make-instance 'delete-conversations-event))
                   (refresh-event (make-instance 'refresh-conversations-window-event)))
               (push-event delete-event)
               (push-event refresh-event)))))
    (when-let* ((selected-row (line-oriented-window:selected-row
                                specials:*conversations-window*))
                (folder       (line-oriented-window:normal-text selected-row)))
      (ask-string-input #'on-input-complete
                        :prompt (format nil
                                        (_ "Delete conversation ~s? [y/N] ")
                                        folder)))))

(defun report-status ()
  "Report status to admins"
  (let* ((selected-row (line-oriented-window:selected-row-fields specials:*thread-window*))
         (status-id    (db:row-message-status-id selected-row))
         (username     (db:row-message-username selected-row))
         (account-id   (db:acct->id username)))
    (flet ((on-input-complete (comment)
             (when (string-not-empty-p comment)
               (let ((event              (make-instance 'report-status-event
                                                        :account-id account-id
                                                        :status-id  status-id
                                                        :comment    comment))
                     (max-comment-length (swconf:max-report-comment-length)))
                 (if (> (length comment)
                        max-comment-length)
                     (error-message  (format nil
                                             (n_ "Comment too long by ~a character"
                                                 "Comment too long by ~a characters"
                                                 (-  max-comment-length
                                                     (length comment)))
                                             (-  max-comment-length
                                                     (length comment))))
                     (with-blocking-notify-procedure
                         ((format nil (_ "Reporting user: ~s") username)
                          (_ "Report trasmitted."))
                       (push-event event)))))))
      (ask-string-input #'on-input-complete
                        :prompt        (_ "Comment on reports: ")))))

(defparameter *crypto-username* nil)

(defun crypto-import-key ()
  "Import crypto key for an user"
  (setf *crypto-username* nil)
  (labels ((add-username ()
             (flet ((on-add-username (username)
                      (db-utils:with-ready-database (:connect nil)
                        (when (string-not-empty-p username)
                          (if (db:user-exists-p username)
                              (progn
                                (setf *crypto-username* username)
                                (add-key))
                              (error-message (format nil
                                                     (_ "User ~s does not exists in database")
                                                     username)))))))
               (ask-string-input #'on-add-username
                                 :prompt      (_ "Username: ")
                                 :complete-fn #'complete:username-complete)))
           (add-key ()
             (flet ((on-add-key (key)
                      (let ((event (make-instance 'add-crypto-data-event
                                                  :username *crypto-username*
                                                  :key      key)))
                        (when (string-not-empty-p key)
                          (push-event event)
                          (notify (format nil
                                          (_ "Added crypto key for user ~s")
                                          *crypto-username*))))))
               (ask-string-input #'on-add-key
                                 :prompt (_ "Key: ")))))
    (add-username)))

(defun crypto-generate-key ()
  "Generate a crypto key for an user"
  (labels ((on-add-username (username)
             (db-utils:with-ready-database (:connect nil)
               (when (string-not-empty-p username)
                 (if (db:user-exists-p username)
                     (let* ((key   (crypto-utils:generate-key))
                            (event (make-instance 'add-crypto-data-event
                                                  :username username
                                                  :key      key)))
                       (push-event event)
                       (notify (format nil (_ "Generated key for user ~s") username))
                       (info-message (format nil (_ "user ~s key ~s") username key)))
                     (error-message (format nil
                                            (_ "User ~s does not exists in database")
                                            username)))))))
    (ask-string-input #'on-add-username
                      :prompt      (_ "Username: ")
                      :complete-fn #'complete:username-complete)))

(defun crypto-export-key ()
  "Show crypto key for an user"
  (labels ((on-add-username (username)
             (db-utils:with-ready-database (:connect nil)
               (when (string-not-empty-p username)
                 (if (db:user-exists-p username)
                     (let* ((key (db:crypto-user-key username)))
                       (info-message (format nil
                                             (_ "Added key for user ~s: ~a")
                                             username
                                             key)))
                     (error-message (format nil
                                            (_ "User ~s does not exists in database")
                                            username)))))))
    (ask-string-input #'on-add-username
                      :prompt      (_ "Username: ")
                      :complete-fn #'complete:username-complete)))

(defun show-about-window ()
  "Show an informative window about this program"
  (let ((lines (text-utils:split-lines +help-about-message+))
        (bg    (swconf:win-bg swconf:+key-help-dialog+))
        (fg    (swconf:win-fg swconf:+key-help-dialog+)))
    (windows:make-blocking-message-dialog specials:*main-window*
                                          nil
                                          (_ "About this software")
                                          lines
                                          bg
                                          fg)))
(defun reset-timeline-pagination ()
  "Removes the pagination data for current timeline and folder

For each timeline the software keep tracks of the oldest and newes toot fetched from the instance, This way we can expand the messages thread from the point we left after the latest update.

This command will remove those limits so that we can just jump to the last messages posted on the instance and start expanding toots from there."
  (let* ((timeline (thread-window:timeline-type specials:*thread-window*))
         (folder   (thread-window:timeline-folder specials:*thread-window*)))
    (with-blocking-notify-procedure ((_ "Clearing pagination data"))
      (db:remove-pagination-status folder timeline))))

(defun poll-vote ()
  "Vote in a poll"
  (labels ((valid-indices-p (choices options)
             (let ((max-index (length options)))
               (every (lambda (a) (and (>= a 0)
                                       (< a max-index)))
                      choices)))
           (on-input-complete (choices)
             (let ((choices-list (split-words choices)))
               (if (or (null choices-list)
                       (notevery (lambda (a)
                                   (let ((idx (parse-integer a :junk-allowed t)))
                                     (and idx
                                          (>= idx 0))))
                                 choices-list))
                   (error-message
                    (_ "Invalid choices, usa a space separated list of positive integers."))
                   (db-utils:with-ready-database (:connect nil)
                     (when-let* ((fields    (line-oriented-window:selected-row-fields
                                             specials:*thread-window*))
                                 (status-id (db:row-message-status-id fields))
                                 (poll      (db:find-poll-bound-to-status status-id))
                                 (poll-id   (db:row-id poll))
                                 (event     (make-instance 'poll-vote-event
                                                           :poll-id poll-id
                                                           :choices choices-list))
                                 (actual-choices (mapcar (lambda (a)
                                                           (parse-integer a :junk-allowed t))
                                                         choices-list))
                                 (options        (db:all-poll-options poll-id)))
                       (if (not (valid-indices-p actual-choices options))
                           (error-message
                            (format nil
                                    (_ "Invalid choices, index choice out of range (max ~a).")
                                    (1- (length options))))
                           (with-blocking-notify-procedure ((_ "Voting... ")
                                                            (_ "Choice sent."))
                             (push-event event)))))))))
    (when-let* ((fields (line-oriented-window:selected-row-fields
                         specials:*thread-window*))
                (status-id (db:row-message-status-id fields)))
      (let ((poll (db:find-poll-bound-to-status status-id)))
        (if poll
            (ask-string-input #'on-input-complete
                              :prompt
                              (_ "Type the index (or space separated indices) of selected choices: "))
            (error-message (_ "This in not a poll")))))))
