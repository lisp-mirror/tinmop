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

(in-package :ui-goodies)

(defun boolean-input-accepted-p (user-input)
  (string-equal user-input (_ "y")))

(defun quit-program ()
  "This  is not the right function to quit the program, use
  'clean-close-program'."
  (hooks:run-hooks 'hooks:*before-quit*)
  (db-utils:close-db)
  (os-utils:exit-program))

(defun clean-temporary-files ()
  "Use this to close the program"
  (flet ((on-input-complete (maybe-accepted)
           (when (boolean-input-accepted-p maybe-accepted)
             (fs:clean-temporary-directories)
             (fs:clean-temporary-files))
           (push-event (make-instance 'quit-program-event))))
    (let ((temporary-text        (strcat (format nil
                                                 (_ "~a Temporary files~2%")
                                                 (swconf:gemini-h1-prefix))
                                         (format nil
                                                 "~{- ~a~%~}"
                                                 fs:*temporary-files-created*)
                                         (format nil
                                                 "~{- ~a~%~}"
                                                 fs:*temporary-directories-created*)))
          (temporary-files-count (length fs:*temporary-files-created*)))
      (if (> temporary-files-count 0)
          (progn
            (message-window:prepare-for-rendering *message-window* temporary-text)
            (windows:draw *message-window*)
            (ask-string-input #'on-input-complete
                              :prompt (format nil
                                              (n_ "Delete ~a temporary file? [y/N] "
                                                  "Delete ~a temporary files? [y/N] "
                                                  temporary-files-count)
                                              temporary-files-count)))
          (push-event (make-instance 'quit-program-event))))))

(defun clean-close-program ()
  "Use this to close the program"
  (flet ((on-input-complete (maybe-accepted)
           (if (boolean-input-accepted-p maybe-accepted)
               (let ((delete-event (make-instance 'delete-all-status-event)))
                 (push-event delete-event))
               (db-utils:with-ready-database (:connect nil)
                 (db:renumber-all-timelines '())))
             (clean-temporary-files)))
    (let ((delete-count        (db:count-status-marked-to-delete))
          (stop-download-event (make-instance 'gemini-abort-all-downloading-event
                                              :priority +maximum-event-priority+)))
      (push-event stop-download-event)
      (if (> delete-count 0)
          (ask-string-input #'on-input-complete
                            :prompt (format nil
                                            (n_ "Delete ~a message? [y/N] "
                                                "Delete ~a messages? [y/N] "
                                                delete-count)
                                            delete-count))
          (progn
            (db:renumber-all-timelines '())
            (clean-temporary-files))))))

(defun notify (message &key (life nil) (as-error nil) (priority +standard-event-priority+))
  (let ((event (make-instance 'notify-user-event
                              :priority     priority
                              :life         (if as-error
                                                (tui:standard-error-notify-life)
                                                life)
                              :notify-error as-error
                              :payload      message)))
  (push-event event)))

(defun notify-procedure (procedure starting-message
                         &key
                           (ending-message (_ "Task completed"))
                           (life-start     nil)
                           (life-end       nil))
  (bt:make-thread (lambda ()
                    (when (string-not-empty-p starting-message)
                      (notify starting-message :life life-start))
                    (funcall procedure)
                    (when (string-not-empty-p ending-message)
                      (notify ending-message :life life-end)))))

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
  (let ((dialog-window (windows:make-info-message-dialog *main-window*
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
  (let ((dialog-window (windows:make-error-message-dialog *main-window*
                                                         title
                                                         message
                                                         buttons
                                                         append-ok-button)))
    (windows:menu-select dialog-window)))

(defun input-dialog-immediate (message)
  (windows:make-input-dialog *main-window* *main-window* message))

(defun error-message (message &optional (priority +standard-event-priority+))
  (let ((event (make-instance 'error-message-event
                              :priority priority
                              :payload message)))
    (push-event event)))

(defun info-message (message &optional (priority +standard-event-priority+))
  (let ((event (make-instance 'info-message-event
                              :priority priority
                              :payload  message)))
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
                           (hide-input      nil)
                           (priority        nil)
                           (initial-value   nil)
                           (prompt          +default-command-prompt+)
                           (complete-fn     #'complete:complete-always-empty))
  (flet ((thread-fn ()
           (let* ((password-echo (and hide-input
                                      (swconf:config-password-echo-character)))
                  (event         (make-instance 'ask-user-input-string-event
                                                :echo-character  password-echo
                                                :forced-priority priority
                                                :initial-value   initial-value
                                                :complete-fn     complete-fn
                                                :prompt          prompt
                                                :payload         (box:dbox nil))))
             (with-accessors ((lock               lock)
                              (condition-variable condition-variable)) event
               (push-event event)
               (with-lock (lock)
                 (bt:condition-wait condition-variable lock)
                 (funcall on-input-complete-fn (box:dunbox (payload event))))))))
      (bt:make-thread #'thread-fn)))

(defun thread-go-up ()
  (thread-window:go-message-up *thread-window*))

(defun thread-go-down ()
  (thread-window:go-message-down *thread-window*))

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
  (thread-window:goto-first-message *thread-window*))

(defun thread-goto-last-message ()
  "Jump to last message"
  (thread-window:goto-last-message *thread-window*))

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
  (thread-window:search-next-unread *thread-window*))

(defun repeat-search ()
  "Repeat the last search performed"
  (push-event (make-instance 'search-next-event
                             :priority +maximum-event-priority+)))

(defun thread-open-selected-message ()
  "Open selected message"
  (setf (windows:keybindings specials:*message-window*)
        keybindings:*message-keymap*)
  (thread-window:open-message *thread-window*))

(defun thread-mark-delete-selected-message ()
  "Mark selected message for deletion"
  (thread-window:mark-selected-message-to-delete *thread-window*
                                                 :move-down-selected-message t))

(defun thread-mark-prevent-delete-selected-message ()
  "Unmark selected message for deletion"
  (thread-window:mark-selected-message-prevent-delete *thread-window*
                                                      :move-down-selected-message t))

(defun subscribe-to-hash ()
  "Subscribe to hashtag"
  (flet ((on-input-complete (tags)
           (let ((event          (make-instance 'subscribe-tags-event
                                       :payload tags))
                 (refresh-event  (make-instance 'refresh-tag-window-event)))
             (push-event refresh-event)
             (push-event event))))
    (let* ((selected-row (line-oriented-window:selected-row-fields *thread-window*))
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
  (message-window:scroll-up *message-window*))

(defun message-scroll-down ()
  (message-window:scroll-down *message-window*))

(defun message-scroll-left ()
  (message-window:scroll-left *message-window*))

(defun message-scroll-right ()
  (message-window:scroll-right *message-window*))

(defun message-scroll-begin ()
  (message-window:scroll-begin *message-window*))

(defun message-scroll-end ()
  (message-window:scroll-end *message-window*))

(defun message-scroll-next-page ()
  (message-window:scroll-next-page *message-window*))

(defun message-scroll-previous-page ()
  (message-window:scroll-previous-page *message-window*))

(defun message-search-regex ()
  "Search regular expression in message"
  (flet ((on-input-complete (regex)
           (let ((event (make-instance 'search-regex-message-content-event
                                       :priority +maximum-event-priority+
                                       :payload  regex)))
             (push-event event))))
    (ask-string-input #'on-input-complete :prompt (_ "Search key: "))))

(defun message-toggle-preformatted-block ()
  "Toggles on/of preformatted block from text and shows alt text, if exists"
  (message-window:toggle-preformatted-block *message-window*))

(defun give-focus (win info-change-focus-message)
  (setf (main-window:focused-window *main-window*)
        win)
  (remove-focus-to-all-windows)
  (setf (windows:in-focus win) t)
  (windows:draw-all)
  (when info-change-focus-message
    (info-message info-change-focus-message +maximum-event-priority+)))

(defun remove-focus-to-all-windows ()
  (stack:do-stack-element (window windows::*window-stack*)
    (when (typep window 'main-window::focus-marked-window)
      (setf (windows:in-focus window) nil))))

(defun pass-focus (all-adjacent-win-fn intersecting-fn sort-predicate)
  (let* ((window                   (main-window:focused-window *main-window*))
              (all-adjacent-win    (stack:stack-select windows::*window-stack*
                                                       all-adjacent-win-fn))
              (to-intersecting-win (remove-if-not intersecting-fn
                                                  all-adjacent-win))
              (intersect-sorted    (sort to-intersecting-win
                                         sort-predicate)))
    (setf intersect-sorted
          (remove window intersect-sorted))
    (setf intersect-sorted
          (remove-if-not (lambda(a) (typep a 'main-window::focus-marked-window))
                         intersect-sorted))
    (when intersect-sorted
      (remove-focus-to-all-windows)
      (give-focus (first-elt intersect-sorted) nil))))

(defun pass-focus-on-right ()
  "Pass the focus on the window placed on the right of the window that
current has focus"
  (let* ((window    (main-window:focused-window *main-window*))
         (x-focused (win-x window))
         (y-focused (win-y window))
         (w-focused (win-width window)))
    (labels ((all-adjacent-fn (w)
               (>= (win-x w)
                   (+ x-focused
                      w-focused)))
             (intersect-fn (w)
               (<= (win-y w)
                   y-focused
                   (+ (win-y w) (win-height w))))
             (sort-predicate (a b)
               (< (win-y a) (win-y b))))
      (pass-focus #'all-adjacent-fn #'intersect-fn #'sort-predicate))))

(defun pass-focus-on-left ()
  "Pass the focus on the window placed on the left of the window that current has focus"
  (let* ((window    (main-window:focused-window *main-window*))
         (x-focused (win-x window))
         (y-focused (win-y window)))
    (labels ((all-adjacent-fn (w)
               (< (win-x w)
                   x-focused))
             (intersect-fn (w)
               (<= (win-y w)
                   y-focused
                   (+ (win-y w) (win-height w))))
             (sort-predicate (a b)
               (< (win-y a) (win-y b))))
      (pass-focus #'all-adjacent-fn #'intersect-fn #'sort-predicate))))

(defun pass-focus-on-bottom ()
  "Pass the focus on the window placed below the window that current has focus"
  (let* ((window    (main-window:focused-window *main-window*))
         (x-focused (win-x window))
         (y-focused (win-y window)))
    (labels ((all-adjacent-fn (w)
               (> (win-y w)
                  y-focused))
             (intersect-fn (w)
               (<= (win-x w)
                   x-focused
                   (+ (win-x w) (win-width w))))
             (sort-predicate (a b)
               (> (win-x a) (win-x b))))
      (pass-focus #'all-adjacent-fn #'intersect-fn #'sort-predicate))))

(defun pass-focus-on-top ()
  "Pass the focus on the window placed above the window that current has focus"
  (let* ((window    (main-window:focused-window *main-window*))
         (x-focused (win-x window))
         (y-focused (win-y window)))
    (labels ((all-adjacent-fn (w)
               (< (win-y w)
                  y-focused))
             (intersect-fn (w)
               (<= (win-x w)
                   x-focused
                   (+ (win-x w) (win-width w))))
             (sort-predicate (a b)
               (> (win-x a) (win-x b))))
      (pass-focus #'all-adjacent-fn #'intersect-fn #'sort-predicate))))

(defmacro gen-focus-to-window (function-suffix window-get-focus
                               &key
                                 (info-change-focus-message (_ "Focus changed"))
                                 (documentation nil))
  `(defun ,(misc:format-fn-symbol t "focus-to-~a" function-suffix) (&key (print-message t))
     ,documentation
     (give-focus ,window-get-focus
                 (if print-message
                     ,info-change-focus-message
                     nil))))

(defun focus-to-thread-window (&key (print-message t))
  "move focus on thread window"
  (message-window:prepare-for-display-status-mode *message-window*)
  (give-focus *thread-window*
              (if print-message
                  (_ "focus passed on threads window")
                  nil))
  (when-window-shown (*chats-list-window*)
    (close-chats-list-window))
  (when-window-shown (*gemini-subscription-window*)
    (close-gemlog-window)))

(gen-focus-to-window message-window
                     *message-window*
                     :documentation      "Move focus on message window"
                     :info-change-focus-message (_ "Focus passed on message window"))

(gen-focus-to-window send-message-window
                     *send-message-window*
                     :documentation      "Move focus on send message window"
                     :info-change-focus-message (_ "Focus passed on send message window"))

(gen-focus-to-window follow-requests-window
                     *follow-requests-window*
                     :documentation      "Move focus on follow requests window"
                     :info-change-focus-message (_ "Focus passed on follow requests window"))

(gen-focus-to-window tags-window
                     *tags-window*
                     :documentation      "Move focus on tags window"
                     :info-change-focus-message (_ "Focus passed on tags window"))

(gen-focus-to-window conversations-window
                     *conversations-window*
                     :documentation      "Move focus on conversations window"
                     :info-change-focus-message (_ "Focus passed on conversation window"))

(gen-focus-to-window open-attach-window
                     *open-attach-window*
                     :documentation      "Move focus on open-attach window"
                     :info-change-focus-message (_ "Focus passed on attach window"))

(gen-focus-to-window open-message-link-window
                     *open-message-link-window*
                     :documentation      "Move focus on open-link window"
                     :info-change-focus-message (_ "Focus passed on link window"))

(gen-focus-to-window open-gemini-stream-windows
                     *gemini-streams-window*
                     :documentation      "Move focus on open gemini streams window"
                     :info-change-focus-message (_ "Focus passed on gemini-stream window"))

(gen-focus-to-window chats-list-window
                     *chats-list-window*
                     :documentation      "Move focus on chats list window"
                     :info-change-focus-message (_ "Focus passed on chats list window"))

(gen-focus-to-window open-gemini-certificates-window
                     *gemini-certificates-window*
                     :documentation      "Move focus on open-gemini certificates window"
                     :info-change-focus-message (_ "Focus passed on TLS certificates window."))

(gen-focus-to-window open-gemini-subscription-window
                     *gemini-subscription-window*
                     :documentation      "Move focus on open-gemini certificates window"
                     :info-change-focus-message (_ "Focus passed on gemlog subscriptions window."))

(gen-focus-to-window gemini-toc-window
                     *gemini-toc-window*
                     :documentation      "Move focus on gemini page table of contents window"
                     :info-change-focus-message (_ "Focus passed on gemini toc window."))

(defun print-quick-help ()
  "Print a quick help"
  (keybindings:print-help *main-window*))

(defun apropos-help ()
  "Print a command's documentation matching a regular expression."
  (flet ((on-input-complete (regex)
           (let ((event (make-instance 'help-apropos-event
                                       :regex  regex)))
             (push-event event))))
    (ask-string-input #'on-input-complete
                      :prompt      (_ "Search for commands (regexp): ")
                      :complete-fn #'complete:complete-always-empty)))

(defun move-message-tree ()
  "Move messages tree to a different folder. If folder does not exist will be created."
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
  (let ((folder (thread-window:timeline-folder *thread-window*)))
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

(defun %update-timeline-event (event-payload-function)
  (let ((event (make-instance 'function-event :payload event-payload-function)))
    (notify (_ "Downloading messages.")
            :priority +maximum-event-priority+
            :life     (swconf:config-notification-life))
    (push-event event)
    (notify (_ "Messages downloaded.")
            :priority +minimum-event-priority+
            :life     (swconf:config-notification-life))))

(defun update-current-timeline (&optional (recover-count 0))
  "Update current timeline

This  command  also checks  notifications  about  mentioning the  user
and (if  such mentions  exists) download the  mentioning toots  in the
folder \"mentions\"."
  (flet ((update-payload ()
           (let* ((timeline (thread-window:timeline-type *thread-window*))
                  (folder   (thread-window:timeline-folder *thread-window*))
                  (max-id   (db:last-pagination-status-id-timeline-folder timeline folder)))
             (multiple-value-bind (kind localp)
                 (timeline->kind timeline)
               (with-notify-errors
                   (client:update-timeline timeline
                                           kind
                                           folder
                                           :recover-from-skipped-statuses t
                                           :recover-count                 recover-count
                                           :min-id                        max-id
                                           :local                         localp)
                 (let ((update-mentions-event (make-instance 'update-mentions-event))
                       (refresh-event         (make-instance 'refresh-thread-windows-event)))
                   ;; updating home also triggers the checks for mentions
                   (when (eq kind :home)
                     (push-event update-mentions-event))
                   (push-event refresh-event)))))))
    (%update-timeline-event #'update-payload)))

(defun update-current-timeline-backwards (&optional (recover-count 0))
  "Update current timeline backwards

Starting from the oldest toot and going back."
  (flet ((update-payload ()
           (let* ((timeline          (thread-window:timeline-type *thread-window*))
                  (folder            (thread-window:timeline-folder *thread-window*))
                  (min-id            (db:first-pagination-status-id-timeline-folder timeline folder)))
             (multiple-value-bind (kind localp)
                 (timeline->kind timeline)
               (with-notify-errors
                 (client:update-timeline timeline
                                         kind
                                         folder
                                         :recover-count                 recover-count
                                         :recover-from-skipped-statuses t
                                         :max-id                        min-id
                                         :local                         localp)
                 (let ((refresh-event (make-instance 'refresh-thread-windows-event)))
                   (push-event refresh-event)))))))
    (%update-timeline-event #'update-payload)))

(defun expand-status-tree (force)
  (flet ((update ()
           (when-let* ((selected-message
                        (line-oriented-window:selected-row-fields *thread-window*))
                       (timeline         (thread-window:timeline-type *thread-window*))
                       (folder           (thread-window:timeline-folder *thread-window*))
                       (status-id        (actual-author-message-id selected-message))
                       (expand-event     (make-instance 'expand-thread-event
                                                        :force-saving-of-ignored-status force
                                                        :new-folder   folder
                                                        :new-timeline timeline
                                                        :status-id    status-id))
                       (refresh-event    (make-instance 'refresh-thread-windows-event
                                                        :priority +minimum-event-priority+)))
             (push-event expand-event)
             (push-event refresh-event))))
    (notify-procedure #'update (_ "Expanding thread"))))

(defun refresh-thread ()
  "Check and download a thread

Expand the post until all the reply and parents are downloaded."
  (expand-status-tree nil))

(defun refresh-thread-totally ()
  "Check and download a thread

Expand the post until all the reply and parents are downloaded.

If some posts was deleted before, download them again."
  (expand-status-tree t))

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
  (when-let* ((selected-row  (line-oriented-window:selected-row-fields *thread-window*))
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
                             *thread-window*))
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
    (line-oriented-window:unselect-all *send-message-window*)
    (line-oriented-window:row-move     *send-message-window* amount)
    (draw *send-message-window*)))

(defun attach-go-down ()
  (attach-move 1))

(defun attach-go-up ()
  (attach-move -1))

(defun attach-delete ()
  "Delete an attach"
  (line-oriented-window:selected-row-delete *send-message-window*)
  (win-clear *send-message-window*)
  (draw *send-message-window*))

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

(defun change-mentions ()
  "Change mentions"
  (flet ((on-add-mentions (new-mentions)
           (let* ((event (make-instance 'send-message-change-mentions-event
                                        :payload new-mentions)))
             (push-event event))))
    (ask-string-input #'on-add-mentions
                      :prompt      (_ "Add mentions: ")
                      :complete-fn #'complete:username-complete)))

(defmacro close-window-and-return-to-threads (window-to-close)
  `(progn
     (win-close ,window-to-close)
     (setf ,window-to-close nil)
     (focus-to-thread-window)))

(defmacro close-window-and-return-to-message (window-to-close)
  `(progn
     (win-close ,window-to-close)
     (setf ,window-to-close nil)
     (focus-to-message-window)))

(defun cancel-send-message ()
  "Cancel sending operation"
  (close-window-and-return-to-threads *send-message-window*))

(defun edit-message-body ()
  "Edit message"
  (when (and *send-message-window*
             (sending-message:message-data *send-message-window*))
    (with-accessors ((body       sending-message:body)
                     (subject    sending-message:subject)
                     (reply-to   sending-message:reply-to)
                     (visibility sending-message:visibility))
        (sending-message:message-data *send-message-window*)
      (let ((temp-file (fs:temporary-file)))
        (with-open-file (stream temp-file
                                :direction         :output
                                :if-exists         :supersede
                                :if-does-not-exist :error)
          (write-sequence body stream))
        (croatoan:end-screen)
        (tui:with-notify-errors
          (os-utils:open-with-editor temp-file))
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

(defun compose-message (&key timeline folder reply-id subject (visibility +status-public-visibility+) (message-header-text nil))
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
                                                   lines))
                           (thread-users   (db:message->thread-users timeline
                                                                     folder
                                                                     reply-id
                                                                     :local-name-prefix
                                                                     message-rendering-utils:+temp-mention-prefix+
                                                                     :acct-prefix
                                                                     +mention-prefix+)))
                 (with-open-file (stream file
                                         :if-exists    :append
                                         :direction    :output
                                         :element-type 'character)
                   (format stream "~a~%" (msg-utils:add-mention-prefix reply-username))
                   (loop for line in quoted-lines do
                        (let ((line-fixed-mentions
                               (message-rendering-utils:local-mention->acct line
                                                                            thread-users)))
                          (format stream "~a~%" line-fixed-mentions)))))))
           (add-signature (file)
             (when-let ((signature (message-rendering-utils:signature)))
               (with-open-file (stream
                                file
                                :direction    :output
                                :element-type 'character
                                :if-exists    :append)
                 (write-sequence signature stream))))
           (insert-header-text (file)
             (when (string-not-empty-p message-header-text)
               (with-open-file (stream file
                                       :if-exists    :append
                                       :direction    :output
                                       :element-type 'character)
                 (format stream "~a~%" message-header-text))))
           (add-body ()
             (let ((temp-file (fs:temporary-file)))
               (insert-header-text temp-file)
               (prepare-reply-body temp-file)
               (add-signature temp-file)
               (let ((reference-open-file (get-universal-time)))
                 (croatoan:end-screen)
                 (tui:with-notify-errors
                   (os-utils:open-with-editor temp-file))
                 (when (and (> (fs:file-size temp-file)
                               0)
                            (> (fs:get-stat-mtime temp-file)
                               reference-open-file))
                   (let ((body (fs:slurp-file temp-file)))
                     (setf (sending-message:body *message-to-send*) body)
                     (add-subject)))))))
    (add-body)))

(defun actual-author-message-id (message-row)
  (or (db:row-message-reblog-id message-row)
      (db:row-message-status-id message-row)))

(defun reply-message ()
  "Reply to message"
  (when-let* ((win              *thread-window*)
              (selected-message (line-oriented-window:selected-row-fields win))
              (actual-message   (if (db:row-message-reblog-id selected-message)
                                    (db:find-message-id (db:row-message-reblog-id selected-message))
                                    selected-message))
              (timeline         (db:row-message-timeline actual-message))
              (folder           (thread-window:timeline-folder win))
              (username         (db:row-message-username   actual-message))
              (visibility       (db:row-message-visibility actual-message))
              (reply-id         (actual-author-message-id  actual-message)))
    (let* ((subject (db:row-message-subject actual-message)))
      (compose-message :timeline   timeline
                       :folder     folder
                       :reply-id   reply-id
                       :subject    subject
                       :visibility visibility))))

(defun send-message ()
  "Send message"
  (when (and *send-message-window*
             (sending-message:message-data *send-message-window*))
    (let ((data               (sending-message:message-data *send-message-window*))
          (attachments-count  (line-oriented-window:rows-length *send-message-window*))
          (max-allowed-attach (swconf:max-attachments-allowed)))
        (if (> attachments-count
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
  "Open message attachments window"
  (when-let* ((win              *thread-window*)
              (selected-message (line-oriented-window:selected-row-fields win)))
    (open-attach-window:init (db:row-message-status-id selected-message))
    (focus-to-open-attach-window)))

(defun open-message-attach-move (amount)
  (ignore-errors
    (line-oriented-window:unselect-all *open-attach-window*)
    (line-oriented-window:row-move     *open-attach-window* amount)
    (draw *open-attach-window*)))

(defun open-message-attach-go-down ()
  (open-message-attach-move 1))

(defun open-message-attach-go-up ()
  (open-message-attach-move -1))

(defun open-message-attach-perform-opening ()
  (when-let* ((selected-line (line-oriented-window:selected-row *open-attach-window*))
              (url           (line-oriented-window:normal-text selected-line)))
  (open-attach-window:open-attachment url)))

(defun close-open-attach-window ()
  (close-window-and-return-to-threads *open-attach-window*))

(defun search-link-window ()
  "Search a link window with a text matching a regular expression"
  (flet ((on-input-complete (regex)
           (when-let* ((window (main-window:focused-window *main-window*)))
             (let ((event (make-instance 'search-link-event
                                         :window window
                                         :regex  regex)))
               (push-event event)))))
    (ask-string-input #'on-input-complete
                      :prompt      (_ "Search key: ")
                      :complete-fn #'complete:complete-always-empty)))

(defun open-gemini-message-link-window ()
  (let* ((window   *message-window*)
         (metadata (message-window:metadata window))
         (links    (gemini-viewer:gemini-metadata-links metadata)))
    (open-message-link-window:init-gemini-links links)
    (focus-to-open-message-link-window)))

(defun open-message-link ()
  "Open message links window

Browse and optionally open the links the text of the message window contains."
  (if (message-window:display-gemini-text-p *message-window*)
      (open-gemini-message-link-window)
      (when-let* ((win              *thread-window*)
                  (selected-message (line-oriented-window:selected-row-fields win)))
        (open-message-link-window:init (db:row-message-status-id selected-message))
        (focus-to-open-message-link-window))))

(defun line-window-move (win amount)
  (ignore-errors
    (line-oriented-window:unselect-all win)
    (line-oriented-window:row-move     win amount)
    (draw win)))

(defun line-window-go-up (win)
  (line-window-move win -1))

(defun line-window-go-down (win)
  (line-window-move win 1))

(defun open-message-link-move (amount)
  (ignore-errors
    (line-oriented-window:unselect-all *open-message-link-window*)
    (line-oriented-window:row-move     *open-message-link-window* amount)
    (draw *open-message-link-window*)))

(defun open-message-link-go-down ()
  (open-message-link-move 1))

(defun open-message-link-go-up ()
  (open-message-link-move -1))

(defun %open-message-link-perform-opening (enqueue)
  (when-let* ((selected-line (line-oriented-window:selected-row *open-message-link-window*))
              (url           (line-oriented-window:normal-text selected-line)))
    (open-message-link-window:open-message-link url enqueue)))

(defun open-message-link-perform-opening ()
  (%open-message-link-perform-opening nil))

(defun open-message-link-open-enqueue ()
  "Open the url and keep the data stream in background

This makes sense only for gemini file stream, if not this command performs the same as
'open-message-link-perform-opening'"
  (%open-message-link-perform-opening t))

(defun close-open-message-link-window ()
  (when-window-shown (*open-message-link-window*)
    (when (message-window:display-gemini-text-p *open-message-link-window*)
      (open-message-link-window:forget-gemini-link-window))
    (if (message-window:display-gemini-text-p *message-window*)
        (close-window-and-return-to-message *open-message-link-window*)
        (close-window-and-return-to-threads *open-message-link-window*))))

(defun gemini-open-certificates-window ()
  "Open a window with all the  client certificated generated so far to
authenticate this client on a gemini server."
  (gemini-certificates-window:open-gemini-certificates-window)
  (focus-to-open-gemini-certificates-window))

(defun gemini-certificate-window-move (amount)
  (line-window-move *gemini-certificates-window* amount))

(defun gemini-certificate-window-go-down ()
  (line-window-go-down *gemini-certificates-window*))

(defun gemini-certificate-window-go-up ()
  (line-window-go-up *gemini-certificates-window*))

(defun gemini-close-certificate-window ()
  (close-window-and-return-to-message *gemini-certificates-window*))

(defun gemini-delete-certificate ()
  "Delete a gemini  certificate, this could makes all user data on the
server unreachable as the server will not be able to identify the client.

Of course  could be possible to generate a new identity (i.e. a new
certificate).
"
  (flet ((on-input-complete (answer)
           (when (boolean-input-accepted-p answer)
             (db-utils:with-ready-database (:connect nil)
               (let* ((selected-row (line-oriented-window:selected-row-fields
                                     *gemini-certificates-window*))
                      (cache-key    (db:row-cache-key selected-row))
                      (event        (make-instance 'function-event
                                                   :payload
                                                   (lambda ()
                                                     (line-oriented-window:resync-rows-db
                                                      *gemini-certificates-window*
                                                      :suggested-message-index 0)))))
                 (db:cache-invalidate cache-key)
                 (push-event event))))))
    (ask-string-input #'on-input-complete
                      :prompt      (_ "Delete this certificate? [Y/n] ")
                      :complete-fn #'complete:complete-always-empty)))

(defun gemini-open-gemlog-window ()
  "Open a window with all the  gemlog subscribed."
  (gemini-subscription-window:open-gemini-subscription-window)
  (focus-to-open-gemini-subscription-window))

(defun close-gemlog-window ()
  (close-window-and-return-to-threads *gemini-subscription-window*))

(defmacro with-selected-gemlog-id ((fields gemlog-id) &body body)
  `(when-let* ((,fields    (line-oriented-window:selected-row-fields *gemini-subscription-window*))
               (,gemlog-id (db:row-url ,fields)))
     ,@body))

(defun gemlog-cancel-subscription ()
  (with-selected-gemlog-id (fields gemlog-id)
    (when-let* ((event (make-instance 'program-events:gemlog-cancel-subscription-event
                                      :payload gemlog-id)))
      (with-blocking-notify-procedure ((format nil (_ "Canceling subscription for ~s") gemlog-id))
        (program-events:push-event event)))))

(defun show-gemlog-to-screen ()
  (with-selected-gemlog-id (fields gemlog-id)
    (when-let* ((entries   (db:gemlog-entries gemlog-id))
                (event     (make-instance 'program-events:gemlog-show-event
                                          :gemlog-url gemlog-id
                                          :title      (db:row-title fields)
                                          :subtitle   (db:row-subtitle fields)
                                          :entries    entries)))
      (program-events:push-event event))))

(defun gemlog-refresh-all ()
  (program-events:push-event (make-instance 'program-events:gemlog-refresh-all-event)))

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
    (line-oriented-window:unselect-all *follow-requests-window*)
    (line-oriented-window:row-move     *follow-requests-window* amount)
    (draw *follow-requests-window*)))

(defun follow-request-go-down ()
  (follow-request-move 1))

(defun follow-request-go-up ()
  (follow-request-move -1))

(defun follow-request-delete ()
  (line-oriented-window:selected-row-delete *follow-requests-window*)
  (win-clear *follow-requests-window*)
  (draw *follow-requests-window*))

(defun start-follow-request-processing ()
  (let ((event (make-instance 'open-follow-requests-window-event)))
    (push-event event)))

(defun close-follow-requests-window ()
  (close-window-and-return-to-threads *follow-requests-window*))

(defun cancel-follow-requests ()
  (close-follow-requests-window))

(defun process-follow-requests ()
  (when (confirm-dialog-immediate (_ "Confirm operation?"))
    (follow-requests:process-requests))
  (close-follow-requests-window))

(defun tag-move (amount)
  (ignore-errors
    (line-oriented-window:unselect-all *tags-window*)
    (line-oriented-window:row-move     *tags-window* amount)
    (draw *tags-window*)))

(defun tag-go-down ()
  (tag-move 1))

(defun tag-go-up ()
  (tag-move -1))

(defun open-tag-folder ()
  "Open tag folder"
  (when-let* ((selected-line  (line-oriented-window:selected-row *tags-window*))
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
           (let* ((timeline     (thread-window:timeline-type   *thread-window*))
                  (folder       (thread-window:timeline-folder *thread-window*))
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
    (line-oriented-window:unselect-all *conversations-window*)
    (line-oriented-window:row-move     *conversations-window* amount)
    (draw *conversations-window*)))

(defun conversation-go-down ()
  (conversation-move 1))

(defun conversation-go-up ()
  (conversation-move -1))

(defun goto-conversation ()
  (when-let* ((selected-row  (line-oriented-window:selected-row
                              *conversations-window*))
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
                                *conversations-window*))
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
                                *conversations-window*))
                (folder       (line-oriented-window:normal-text selected-row)))
      (ask-string-input #'on-input-complete
                        :prompt (format nil
                                        (_ "Delete conversation ~s? [y/N] ")
                                        folder)))))

(defun report-status ()
  "Report status to admins"
  (let* ((selected-row (line-oriented-window:selected-row-fields *thread-window*))
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
    (windows:make-blocking-message-dialog *main-window*
                                          nil
                                          (_ "About this software")
                                          lines
                                          bg
                                          fg)))

(defun show-welcome-window ()
  "Show an informative window about this program"
  (let ((lines (text-utils:split-lines +welcome-message+))
        (bg    (swconf:win-bg swconf:+key-help-dialog+))
        (fg    (swconf:win-fg swconf:+key-help-dialog+)))
    (windows:make-blocking-message-dialog *main-window*
                                          nil
                                          (_ "Welcome")
                                          lines
                                          bg
                                          fg)))

(defun reset-timeline-pagination ()
  "Removes the pagination data for current timeline and folder

For each timeline the software keep tracks of the oldest and newest toot fetched from the instance, This way we can expand the messages thread from the point we left after the latest update.

This command will remove those limits so that we can just jump to the last messages posted on the instance and start expanding toots from there."
  (let* ((timeline (thread-window:timeline-type *thread-window*))
         (folder   (thread-window:timeline-folder *thread-window*)))
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
                    (_ "Invalid choices, use a space separated list of positive integers."))
                   (db-utils:with-ready-database (:connect nil)
                     (when-let* ((fields    (line-oriented-window:selected-row-fields
                                             *thread-window*))
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
                           (with-blocking-notify-procedure ((_ "Voting… ")
                                                            (_ "Choice sent."))
                             (push-event event)))))))))
    (when-let* ((fields (line-oriented-window:selected-row-fields
                         *thread-window*))
                (status-id (db:row-message-status-id fields)))
      (let ((poll (db:find-poll-bound-to-status status-id)))
        (if poll
            (ask-string-input #'on-input-complete
                              :prompt
                              (_ "Type the index (or space separated indices) of selected choices: "))
            (error-message (_ "This in not a poll")))))))

;;;; chats

(defun refresh-chats ()
  "Refresh the chats lists, but not the chat's messages"
  (program-events:push-event (make-instance 'program-events:get-chats-event)))

(defun refresh-chat-messages ()
  "Force the refresh of the chat's messages"
  (when-let* ((fields  (line-oriented-window:selected-row-fields *chats-list-window*))
              (chat-id (db:row-id fields)))
    (let* ((min-message-id  (db:last-chat-message-id chat-id))
           (event           (make-instance 'program-events:get-chat-messages-event
                                           :chat-id        chat-id
                                           :min-message-id min-message-id)))
      (program-events:push-event event))))

(defun open-chats-list-window ()
  "open a window  containing the list of active chat  ordered from the
mot recent updated to least recent"
  (chats-list-window:open-chats-list-window)
  (focus-to-chats-list-window))

(defun close-chats-list-window ()
  (close-window-and-return-to-threads *chats-list-window*))

(defun update-all-chats-messages ()
  (program-events:push-event (make-instance 'program-events:update-all-chat-messages-event
                                            :priority +minimum-event-priority+)))

(defun update-all-chats-data ()
  (refresh-chats)
  (update-all-chats-messages))

(defun show-chat-to-screen ()
  (when-let* ((fields  (line-oriented-window:selected-row-fields *chats-list-window*))
              (chat-id (db:row-id fields))
              (chat    (db:find-chat chat-id))
              (event   (make-instance 'program-events:chat-show-event
                                      :chat chat)))
    (close-chats-list-window)
    (program-events:push-event event)
    (focus-to-message-window)
    (chat-loop chat)))

(defun chat-loop (chat)
  "Start writing to chat"
  (labels ((post-message (message)
             (let ((event (make-instance 'program-events:chat-post-message-event
                                         :priority +maximum-event-priority+
                                         :message  message
                                         :chat-id  (db:row-id chat))))
               (push-event event)))
           (%loop ()
             (labels ((on-message-composed (message)
                        (when (string-not-empty-p message)
                          (post-message message)
                          (update-all-chats-messages)
                          (let ((show-event (make-instance 'program-events:chat-show-event
                                                           :priority +minimum-event-priority+
                                                           :chat     chat)))
                            (push-event show-event)
                            (%loop))))
                      (ask-fn ()
                        (lambda ()
                          (ask-string-input #'on-message-composed
                                            :priority    +minimum-event-priority+
                                            :prompt      (_ "Add message (enter to quit): ")
                                            :complete-fn #'complete:complete-chat-message))))
                 (push-event (make-instance 'function-event
                                            :priority    +minimum-event-priority+
                                            :payload (ask-fn))))))
    (%loop)))

(defun open-chat-link-window ()
  (let* ((window   *message-window*)
         (chat     (message-window:metadata window))
         (chat-id  (db:row-id chat))
         (links    (db:all-chat-links chat-id)))
    (open-message-link-window:init-chat-links links)
    (focus-to-open-message-link-window)))

(defun change-chat-label ()
  "Change the name (called label) of a chat"
  (let* ((fields  (line-oriented-window:selected-row-fields *chats-list-window*))
         (chat-id (db:row-id fields)))
    (flet ((on-input-complete (new-label)
             (when (string-not-empty-p new-label)
               (push-event (make-instance 'chat-change-label-event
                                          :chat-id chat-id
                                          :label   new-label)))))
      (ask-string-input #'on-input-complete
                        :prompt      (_ "Type the new label of the chat: ")
                        :complete-fn #'complete:complete-chat-message))))

(defun chat-create-new ()
  "Start a new chat"
  (let ((chat-user-id  nil)
        (chat-username nil))
    (labels  ((on-user-id-complete (username)
                (when (string-not-empty-p username)
                  (when-let* ((user-id (db:username->id username)))
                    (setf chat-user-id  user-id)
                    (setf chat-username username)
                    (ask-string-input #'on-label-complete
                                      :prompt (_ "Type the new label of the chat: ")))))
              (on-label-complete (chat-label)
                (when (string-not-empty-p chat-label)
                  (push-event (make-instance 'chat-create-event
                                             :chat-label chat-label
                                             :user-id    chat-user-id))
                  (update-all-chats-data)
                  (notify (format nil
                                  (_ "Chat ~a with ~a created")
                                  chat-label
                                  chat-username)))))
      (ask-string-input #'on-user-id-complete
                        :prompt      (_ "Type the user to chat with: ")
                        :complete-fn #'complete:username-complete))))

(defun chat-list-move (amount)
  (ignore-errors
    (line-oriented-window:unselect-all *chats-list-window*)
    (line-oriented-window:row-move     *chats-list-window* amount)
    (draw *chats-list-window*)))

(defun chat-list-go-up ()
  (chat-list-move -1))

(defun chat-list-go-down ()
  (chat-list-move 1))

;;;; gemini

(defun gemini-open-url-prompt ()
  "This is used when opening gemini link too, see:
open-message-link-window:open-message-link"
  (_ "Open Gemini url: "))

(defun open-gemini-address ()
  "Ask for a gemini address and try to load it"
  (flet ((on-input-complete (url)
           (gemini-viewer:load-gemini-url (trim-blanks url)
                                          :use-cached-file-if-exists t
                                          :priority program-events:+maximum-event-priority+)))

    (let ((prompt (gemini-open-url-prompt)))
      (ask-string-input #'on-input-complete
                        :prompt      prompt
                        :complete-fn (complete:make-complete-gemini-iri-fn prompt)))))

(defun gemini-history-back ()
  "Reopen a previous visited gemini address"
  (push-event (make-instance 'gemini-back-event)))

(defun gemini-view-source ()
  "Shows the source of current gemini page"
  (gemini-viewer:view-source *message-window*))

(defun gemini-abort-download ()
  "Stop a transferring data from a gemini server"
  (when-let* ((fields       (line-oriented-window:selected-row-fields *gemini-streams-window*))
              (iri-to-abort (gemini-viewer:download-iri fields))
              (event        (make-instance 'gemini-abort-downloading-event
                                           :payload  iri-to-abort
                                           :priority program-events:+maximum-event-priority+)))
    (push-event event)))

(defun gemini-open-streams-window ()
  "Open a window listing the gemini streams"
  (gemini-viewer:open-gemini-stream-window)
  (focus-to-open-gemini-stream-windows))

(defun trivial-line-oriented-window-move (win amount)
  (ignore-errors
   (line-oriented-window:unselect-all win)
   (line-oriented-window:row-move     win amount)
   (draw win)))

(defun gemini-streams-move (amount)
  (ignore-errors
    (line-oriented-window:unselect-all *gemini-streams-window*)
    (line-oriented-window:row-move     *gemini-streams-window* amount)
    (draw *gemini-streams-window*)))

(defun gemini-streams-window-up ()
  "Move to the upper stream in the list."
  (gemini-streams-move -1))

(defun gemini-streams-window-down ()
  "Move to the lower stream in the list."
  (gemini-streams-move 1))

(defun gemini-streams-window-close ()
  "Close the streams window."
  (close-window-and-return-to-message *gemini-streams-window*))

(defun gemini-streams-window-open-stream ()
  "Open the selected stream."
  (when-let* ((fields (line-oriented-window:selected-row-fields *gemini-streams-window*))
              (iri-to-open (gemini-viewer:download-iri fields)))
    (gemini-viewer:db-entry-to-foreground iri-to-open)))

(defun gemini-refresh-page ()
  "Refresh current gemini page"
  (when-let* ((url         (gemini-viewer:current-gemini-url))
              (event-abort (make-instance 'gemini-abort-downloading-event
                                          :payload  url
                                          :priority program-events:+maximum-event-priority+))
              (event-open  (make-instance 'gemini-request-event
                                          ;; :priority
                                          ;; program-events:+maximum-event-priority+
                                          :use-cached-file-if-exists nil
                                          :url                       url)))
    (push-event event-abort)
    (push-event event-open)))

(defun gemlogs-subscription-move (amount)
  (ignore-errors
    (line-oriented-window:unselect-all *gemini-subscription-window*)
    (line-oriented-window:row-move     *gemini-subscription-window* amount)
    (draw  *gemini-subscription-window*)))

(defun gemlogs-subscription-go-down ()
  (gemlogs-subscription-move 1))

(defun gemlogs-subscription-go-up ()
  (gemlogs-subscription-move -1))

(defun gemini-subscribe-gemlog ()
  "Subscribe to the gemlog shown in the main window.

The page must be formatted according to gemini subscription specifications:

gemini://gemini.circumlunar.space/docs/companion/subscription.gmi

"
  (when-let ((url (gemini-viewer:current-gemini-url)))
    (with-blocking-notify-procedure ((format nil (_ "Subscribing to ~s") url))
      (let ((event (make-instance 'gemini-gemlog-subscribe-event
                                  :payload url)))
        (push-event event)))))

(defun send-to-pipe-on-input-complete (command data)
  (when (and (string-not-empty-p command)
             data)
    (push-event (make-instance 'send-to-pipe-event
                               :data    data
                               :command command))
    (info-message (format nil (_ "Command ~s completed") command))))

(defun send-to-pipe ()
  "Send contents of window to a command"
  (flet ((on-input-complete (command)
           (let ((data (line-oriented-window:rows->text *message-window*)))
             (send-to-pipe-on-input-complete command data))))
    (ask-string-input #'on-input-complete
                      :prompt (format nil (_ "Send to command: ")))))

(defun send-message-to-pipe ()
  "Send contents of a message to a command"
  (when-let* ((selected-message (line-oriented-window:selected-row-fields *thread-window*))
              (message          (db:row-message-rendered-text selected-message)))
    (flet ((on-input-complete (command)
             (send-to-pipe-on-input-complete command message)))
      (ask-string-input #'on-input-complete
                        :prompt (format nil (_ "Send message to command: "))))))


(let ((tour ()))

  (defun tour-mode-on-input-completed-clsr (links)
    (lambda (data)
      (when (string-not-empty-p data)
        (let ((parsed-tour (ignore-errors (tour-mode-parser:parse-tour-mode data))))
          (if (not parsed-tour)
              (when-let ((scanner (create-scanner data)))
                (loop for link in links do
                  (when (or (scan scanner (gemini-parser:name link))
                            (scan scanner (gemini-parser:target link)))
                    (pushnew link tour :test (lambda (a b)
                                               (string= (gemini-parser:target a)
                                                        (gemini-parser:target b)))))))
              (let ((all-indices ()))
                (loop for index in parsed-tour do
                  (if (tour-mode-parser:range-p index)
                      (let ((from (tour-mode-parser:range-from index))
                            (to   (tour-mode-parser:range-to   index)))
                        (loop for i from (min from to) to (max from to) do
                          (pushnew i all-indices :test #'=)))
                      (pushnew index all-indices :test #'=)))
                (loop for index in (reverse all-indices) do
                  (if (<= 0 index (length links))
                      (push (elt links index) tour)
                      (notify (format nil (_ "Index ~a out of range") index)
                              :as-error t))))))
        (info-message (_ "Tour saved")))))

  (defun tour-mode-link ()
    "Enable   \"tour  mode\".   Ask  for   link  indices,   each  link
    corresponding to the  index will be saved in a  special queue that
    can be opened using `next-tour-link' in a last-in last-out way.

    If user input is made by a single word only (i.e. a string with no
    spaces),  the input  is used  as a  regular expression  to collect
    matching links (matching name or URI)."
    (with-accessors ((links open-message-link-window::links)) *open-message-link-window*
      (ask-string-input (tour-mode-on-input-completed-clsr links)
                        :prompt (format nil (_ "link indices: ")))))

  (defun next-tour-link ()
    "Open the next link in the tour queue."
    (let* ((queue (reverse tour))
           (link  (first queue)))
      (if (null queue)
          (error-message (_ "Tour completed"))
          (let ((url (gemini-parser:target link)))
            (setf tour (reverse (rest queue)))
            (open-message-link-window:open-message-link url nil)))))

  (defun show-tour-links ()
    "Show a link window with all the links in the tour queue."
    (open-message-link-window:init-gemini-links (reverse tour))
    (focus-to-open-message-link-window))

  (defun save-selected-message-in-tour ()
    "Save the selected link in the tour queue"
    (ignore-errors
     (let ((win *open-message-link-window*))
       (with-accessors ((links open-message-link-window::links)) win
         (when-let* ((selected-index (line-oriented-window:row-selected-index win))
                     (selected-link  (elt links selected-index))
                     (label          (or (gemini-parser:name   selected-link)
                                         (gemini-parser:target selected-link))))
           (push selected-link tour)
           (info-message (format nil (_ "~s saved in tour") label))))))))

(defun open-gemini-toc ()
  "Opend a windows that contains a  generated table of contents of the
gemini page the program is rendering."
  (push-event (make-instance 'gemini-toc-open)))

(defun gemini-toc-jump-to-entry ()
  (let* ((selected-row    (line-oriented-window:selected-row-fields *gemini-toc-window*))
         (gid-looking-for (message-window:gemini-toc-group-id selected-row)))
    (push-event (make-instance 'gemini-toc-jump-to-section
                               :toc-win         *gemini-toc-window*
                               :message-win     *message-window*
                               :gid-looking-for gid-looking-for))))

(defun gemini-toc-scroll-up ()
  (trivial-line-oriented-window-move *gemini-toc-window* -1)
  (gemini-toc-jump-to-entry))

(defun gemini-toc-scroll-down ()
  (trivial-line-oriented-window-move *gemini-toc-window* 1)
  (gemini-toc-jump-to-entry))

(defun gemini-toc-close ()
  (close-window-and-return-to-message *gemini-toc-window*))

(defun gemini-toc-scroll-down-page ()
  (message-window:scroll-down *message-window*))

(defun gemini-toc-scroll-up-page ()
  (message-window:scroll-up *message-window*))

(defun ask-input-on-tofu-error (condition fn)
  (let ((host (gemini-client:host condition)))
    (flet ((on-input-complete (maybe-accepted)
             (when (ui::boolean-input-accepted-p maybe-accepted)
               (db-utils:with-ready-database (:connect nil)
                 (db:tofu-delete host)
                 (funcall fn)))))
      (ui:ask-string-input #'on-input-complete
                           :prompt
                           (format nil
                                   (_ "Host ~s signature changed! This is a potential security risk! Ignore this warning? [y/N] ")
                                   host)
                           :priority program-events:+standard-event-priority+))))

(defun import-gemini-certificate ()
  "Import a TLS certificate, not generated from tinmop, to authenticate this client."
  (let ((cert-file      nil)
        (cert-key-file  nil))
    (labels ((file-valid-p (path)
               (cond
                 ((string-empty-p path)
                  (ui:notify (_ "Empty path") :as-error t)
                  nil)
                 ((not (fs:file-exists-p path))
                  (error-message (format nil (_ "No such file ~s") path))
                  nil)
                 ((= (fs:file-size path) 0)
                  (error-message (format nil (_ "File ~s is empty") path))
                  nil)
                 (t :file-valid)))
             (on-cert-path-input-complete (cert-path)
               (when (file-valid-p cert-path)
                 (setf cert-file cert-path)
                 (ui:ask-string-input #'on-cert-key-path-input-complete
                                      :prompt (format nil (_ "Insert certificate key file: "))
                                      :complete-fn #'complete:directory-complete)))
             (on-cert-key-path-input-complete (key-path)
               (let ((prompt-history (gemini-open-url-prompt))
                     (prompt         (_ "Insert the gemini IRI where where credential are valid: ")))
                 (when (file-valid-p key-path)
                   (setf cert-key-file key-path)
                   (ui:ask-string-input #'on-valid-uri-complete
                                        :prompt prompt
                                        :complete-fn
                                        (complete:make-complete-gemini-iri-fn prompt-history)))))
             (on-valid-uri-complete (uri)
               (db-utils:with-ready-database (:connect nil)
                 (if (gemini-parser:gemini-iri-p uri)
                     (let* ((id             (to-s (db:cache-put uri +cache-tls-certificate-type+)))
                            (cert-filename  (fs:path-last-element cert-file))
                            (key-filename   (fs:path-last-element cert-key-file))
                            (cache-dir      (os-utils:cached-file-path id))
                            (cert-out-path  (strcat cache-dir
                                                    fs:*directory-sep*
                                                    cert-filename))
                            (key-out-path  (strcat cache-dir
                                                   fs:*directory-sep*
                                                   key-filename)))
                       (fs:make-directory cache-dir)
                       (fs:copy-a-file cert-file cert-out-path :overwrite t)
                       (fs:copy-a-file cert-key-file key-out-path :overwrite t)
                       (info-message (format nil (_ "Certificate imported for ~s") uri)))
                     (error-message (format nil
                                            (_ "~s is not a valid gemini address")
                                            uri))))))
      (ui:ask-string-input #'on-cert-path-input-complete
                           :prompt (format nil (_ "Insert certificate file: "))
                           :complete-fn #'complete:directory-complete))))

(defun bookmark-gemini-page ()
  (if (message-window:gemini-window-p)
      (let* ((link        (gemini-viewer:current-gemini-url))
             (metadata    (message-window:metadata *message-window*))
             (source      (gemini-viewer:gemini-metadata-source-file metadata))
             (description (gemini-parser:gemini-first-h1 source)))
        (labels ((on-description-completed (new-description)
                   (if (text-utils:string-empty-p new-description)
                       (error-message (_ "Empty description"))
                       (progn
                         (setf description new-description)
                         (ui:ask-string-input #'on-section-completed
                                              :prompt (format nil (_ "Insert bookmark section: "))
                                              :complete-fn #'complete:bookmark-section-complete))))
                 (on-section-completed (section)
                   (db-utils:with-ready-database (:connect nil)
                     (db:bookmark-add db:+bookmark-gemini-type-entry+
                                      link
                                      :section     section
                                      :description description))
                   (notify (format nil (_ "Added ~s in bookmark") link))))
          (ui:ask-string-input #'on-description-completed
                               :prompt        (format nil (_ "Insert bookmark description: "))
                               :initial-value description)))
      (error-message (_ "The window is not displaying a gemini document"))))

(defun generate-bookmark-page ()
  (let ((bookmarks-sections (db:bookmark-all-grouped-by-section)))
    (with-output-to-string (stream)
      (format stream (gemini-parser:geminize-h1 (_ "My bookmark~2%")))
      (loop for section in bookmarks-sections do
        (let ((header    (car section))
              (bookmarks (cdr section)))
          (when (string-empty-p header)
            (setf header (_ "Uncategorized")))
          (write-string (gemini-parser:geminize-h2 header) stream)
          (write-char #\Newline stream)
          (write-char #\Newline stream)
          (loop for bookmark in bookmarks do
            (let ((link (join-with-strings* " "
                                            (db:row-value       bookmark)
                                            (db:row-description bookmark))))
              (write-string (gemini-parser:geminize-link link) stream)
              (write-char #\Newline stream)))
          (write-char #\Newline stream))))))

(defun display-bookmark ()
  (let* ((bookmark-page (generate-bookmark-page))
         (event         (make-instance 'gemini-display-data-page
                                       :window *message-window*
                                       :payload bookmark-page)))
    (push-event event)))

(defun delete-gemini-bookmark ()
  (flet ((on-description-completed (selected)
           (if (text-utils:string-empty-p selected)
               (error-message (_ "No entry selected"))
               (when-let ((id (db:bookmark-complete->id selected)))
                 (db-utils:with-ready-database (:connect nil)
                   (db:bookmark-delete id))))))
    (ui:ask-string-input #'on-description-completed
                               :prompt        (format nil (_ "Delete bookmark: "))
                               :complete-fn
                               (complete:bookmark-description-complete-clsr db:+bookmark-gemini-type-entry+))))
