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

(in-package :program-events)

(define-constant +standard-event-priority+  10 :test #'=)

(define-constant +minimum-event-priority+   -1 :test #'=)

(define-constant +maximum-event-priority+   -2 :test #'=)

(defparameter *id-lock* (bt:make-recursive-lock))

(defparameter *event-id* 0)

;; used only in batch mode from the command line
(defparameter *process-events-immediately* nil
  "Used only in  batch mode from the command line.  Instead of pushing
  the  event on  a priority  queue  that will  be picked  by a  thread
  process the event immediately")

(defparameter *stop-event-dispatching* nil)

(defun stop-event-dispatching ()
  (setf *stop-event-dispatching* t))

(defun start-event-dispatching ()
  (setf *stop-event-dispatching* nil))

(defun stop-event-dispatching-p ()
  *stop-event-dispatching*)

(defmacro with-stop-event-dispatching (&body body)
  `(unwind-protect
        (progn
          (stop-event-dispatching)
          ,@body)
     (start-event-dispatching)))

;; keep  this  function  stricly  monotonic  otherwise  the  order  of
;; elements in priority queue is going to be messed up
(defun-w-lock next-id () *id-lock*
  (incf *event-id*)
  *event-id*)

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
    :accessor priority)
   (notes
    :initform nil
    :initarg  :notes
    :accessor notes
    :documentation "Someway useful for debugging")))

(defmethod print-object ((object program-event) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream
            "id ~a priority ~a notes ~a"
            (event-id object)
            (priority object)
            (notes object))))

(defgeneric process-event (object))

#+debug
(defmethod process-event :before (object)
  (misc:dbg "processing event ~a" object))

(defgeneric reinitialize-id (object))

(defmacro wrapped-in-lock ((queue) &body body)
  (with-gensyms (lock)
    `(with-accessors ((,lock lock)) ,queue
       (with-lock (,lock)
         ,@body))))

(defclass events-queue (priority-queue)
  ((lock
    :initform (bt:make-recursive-lock)
    :initarg  :lock
    :accessor lock)))

(defun queue-compare-predicate (a b)
  (let ((same-priority-p (= (priority a)
                            (priority b))))
    (cond
      ((= (priority a) +minimum-event-priority+)
       nil)
      ((= (priority b) +minimum-event-priority+)
       t)
      ((= (priority a) +maximum-event-priority+)
       t)
      ((= (priority b) +maximum-event-priority+)
       nil)
      (same-priority-p
       (< (event-id a)
          (event-id b)))
      (t
       (< (priority a)
          (priority b))))))

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

(defparameter *events-queue* (make-instance 'events-queue))

(defmethod  reinitialize-id ((object program-event))
  (wrapped-in-lock (*events-queue*)
    (setf (event-id object)
          (next-id))
    object))

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

(defun remove-event-if (predicate)
  (wrapped-in-lock (*events-queue*)
    (remove-element-if *events-queue* predicate)))

(defun map-events (fn)
  (wrapped-in-lock (*events-queue*)
    (map-elements *events-queue* fn)))

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
    (echo-character
     :initform nil
     :initarg  :echo-character
     :accessor echo-character)
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
                                       &key (forced-priority nil) &allow-other-keys)
  (if forced-priority
      (setf (priority object) forced-priority)
      (setf (priority object) (truncate (/ +standard-event-priority+ 2)))))

(defmethod process-event ((object ask-user-input-string-event))
  (with-accessors ((prompt         prompt)
                   (initial-value  initial-value)
                   (complete-fn    complete-fn)
                   (echo-character echo-character)) object
    (setf (command-window:event-to-answer specials:*command-window*)
          object)
    (setf (point-tracker:prompt specials:*command-window*)
          prompt)
    (command-window:remove-messages specials:*command-window*)
    (setf complete:*complete-function* complete-fn)
    (command-window:set-string-mode         specials:*command-window*)
    (command-window:set-history-most-recent specials:*command-window* prompt)
    (setf (command-window:command-line specials:*command-window*)
          initial-value)
    (point-tracker:move-point-to-end        specials:*command-window* initial-value)
    (setf (command-window:echo-character specials:*command-window*)
          echo-character)
    (windows:draw specials:*command-window*)))

(defclass user-input-string-event (ask-user-input-string-event)
  ()
  (:documentation  "When  user provided  a  string  as this  event  is
  generated. When processed it just will notify the condition variable
  of   the  slots   `command-window:event-to-answer'  in   the  object
  `specials:*command-window*' so  that the  callee thread  can restart
  the computation with the input."))

(defmethod initialize-instance :after ((object user-input-string-event)
                                       &key &allow-other-keys)
  (setf (priority object) (truncate (/ +standard-event-priority+ 2))))

(defmethod process-event ((object user-input-string-event))
  (with-accessors ((lock               lock)
                   (condition-variable condition-variable)) object
    (setf (command-window:echo-character specials:*command-window*)
          :completed)
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

(defclass change-window-title-event (program-event)
  ((window
    :initform nil
    :initarg  :window
    :accessor window)))

(defmethod process-event ((object change-window-title-event))
  (with-accessors ((title  payload)
                   (window window)) object
    (setf (message-window::title window) title)))

(define-constant +max-recover-count+ 3)

(defclass save-timeline-in-db-event (program-event)
  ((kind
    :initform nil
    :initarg  :kind
    :accessor kind)
   (timeline-type
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
    :accessor min-id)
   (max-id
    :initform nil
    :initarg  :max-id
    :accessor max-id)
   (recover-from-skipped-statuses
    :initform nil
    :initarg  :recover-from-skipped-statuses
    :reader   recover-from-skipped-statuses-p
    :writer   recover-from-skipped-statuses)
   (recover-count
    :initform 0
    :initarg  :recover-count
    :accessor recover-count)
   (force-saving-of-ignored-status
    :initform nil
    :initarg :force-saving-of-ignored-status
    :reader  force-saving-of-ignored-status-p
    :writer  (setf force-saving-of-ignored-status))))

(defmethod process-event ((object save-timeline-in-db-event))
  "Update a timeline, save messages, performs topological sorts"
  (let ((statuses      (payload object))
        (ignored-count 0))
    (with-accessors ((timeline-type                    timeline-type)
                     (folder                           folder)
                     (min-id                           min-id)
                     (max-id                           max-id)
                     (kind                             kind)
                     (recover-count                    recover-count)
                     (force-saving-of-ignored-status-p force-saving-of-ignored-status-p)) object
      #+debug-mode
      (let ((dump (with-output-to-string (stream)
                    (mapcar (lambda (toot) (tooter::present toot stream))
                            statuses))))
        (dbg "statuses ~a" dump))
      (loop for status in statuses do
        (let ((account-id (tooter:id (tooter:account status)))
              (status-id  (tooter:id status))
              (language   (tooter:language status))
              (skip-this-status nil))
          (when force-saving-of-ignored-status-p
            (db:remove-from-status-ignored status-id folder timeline-type))
          (when (or (and (db:user-ignored-p account-id)
                         (not (db:status-skipped-p status-id folder timeline-type)))
                    (and language
                         (cl-ppcre:scan (swconf:config-post-allowed-language)
                                        language))
                    (hooks:run-hook-until-success 'hooks:*skip-message-hook*
                                                  status
                                                  timeline-type
                                                  folder
                                                  kind
                                                  (localp object)))
            (db:add-to-status-skipped status-id folder timeline-type)
            (setf skip-this-status t)
            (incf ignored-count))
          (when (not skip-this-status)
            (db:update-db status
                          :timeline       timeline-type
                          :folder         folder
                          :skip-ignored-p t))))
      (db:renumber-timeline-message-index timeline-type
                                          folder
                                          :account-id nil)
      (when (and recover-count
                 (< recover-count +max-recover-count+)
                 (> ignored-count 0)
                 (recover-from-skipped-statuses-p object))
        (let ((going-backward  max-id)
              (going-forward   (or (and (null max-id)
                                        (null min-id))
                                   min-id)))
          (cond
            (going-forward
             (ui:update-current-timeline (1+ recover-count)))
            (going-backward
             (ui:update-current-timeline-backwards (1+ recover-count)))))))))

(defclass fetch-remote-status-event (program-event) ())

(defmethod process-event ((object fetch-remote-status-event))
  (let ((status (payload object)))
    #+debug-mode
    (let ((dump (with-output-to-string (stream)
                  (tooter::present status stream))))
      (dbg "fetch single status ~a" dump))
    (db:update-db status)))

(defparameter *search-next-saved-event* nil)

(defclass search-event (program-event) ())

(defun search-event-p (a)
  (typep a 'search-event))

(defmethod process-event :before ((object search-event))
  (setf *search-next-saved-event* object))

(defclass search-next-event (program-event) ())

(defmethod process-event ((object search-next-event))
  (when (search-event-p *search-next-saved-event*)
    (push-event *search-next-saved-event*)))

(defclass search-regex-message-content-event (search-event) ())

(defmethod process-event ((object search-regex-message-content-event))
  (let ((regexp (payload object))
        (win    specials:*message-window*))
    (when (text-utils:string-not-empty-p regexp)
      (handler-case
          (let ((scanner (cl-ppcre:create-scanner regexp :case-insensitive-mode t)))
            (message-window:search-regex win scanner))
        (cl-ppcre:ppcre-syntax-error ()
          (ui:error-message (_ "Invalid regular expression")))))))

(defclass search-message-gemini-fragment-event (search-event) ())

(defmethod process-event ((object search-message-gemini-fragment-event))
  (let ((fragment (payload object)))
    (message-window:search-gemini-fragment specials:*message-window* fragment)))

(defclass thread-search-event (search-event)
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

(defclass filesystem-tree-search-message-event (search-event) ())

(defmethod process-event ((object filesystem-tree-search-message-event))
  (let ((text-looking-for (payload          object)))
    (line-oriented-window::search-row  specials:*filesystem-explorer-window* text-looking-for)))

(defclass thread-goto-message (program-event) ())

(defmethod process-event ((object thread-goto-message))
  (let ((message-index (payload object)))
    (thread-window:goto-message specials:*thread-window* message-index)))

(defclass delete-all-status-event (program-event) ())

(defmethod process-event ((object delete-all-status-event))
  ;; do not change the order. Forget, then delete.
  (let ((timelines/folders-with-forgotten (db:forget-all-statuses-marked-deleted)))
    (db:delete-all-statuses-marked-deleted)
    (db:renumber-all-timelines timelines/folders-with-forgotten)))

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

(defclass event-with-message-status-id ()
  ((message-status-id
    :initform nil
    :initarg  :message-status-id
    :accessor message-status-id)))

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
                                        event-with-message-status-id
                                        event-with-timeline-and-folder)
  ())

(defmethod process-event ((object refresh-thread-windows-event))
  (with-accessors ((new-folder        new-folder)
                   (new-timeline      new-timeline)
                   (message-index     message-index)
                   (message-status-id message-status-id)) object
    (assert message-index)
    (when new-timeline
      (setf (thread-window:timeline-type specials:*thread-window*)
            new-timeline))
    (when new-folder
      (setf (thread-window:timeline-folder specials:*thread-window*)
            new-folder))
    (line-oriented-window:resync-rows-db specials:*thread-window*
                                         :suggested-status-id     message-status-id
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
  (tui:with-notify-errors
    (change-status-values object #'api-client:favourite-status)))

(defclass unfavourite-status-event (program-event event-with-message-index) ())

(defmethod process-event ((object unfavourite-status-event))
  (tui:with-notify-errors
    (change-status-values object #'api-client:unfavourite-status)))

(defclass reblog-status-event (program-event event-with-message-index) ())

(defmethod process-event ((object reblog-status-event))
  (tui:with-notify-errors
    (flet ((boost (status-id)
             (let* ((status             (db:find-status-id status-id))
                    (status-id-to-boost (db:row-message-reblog-id status)))
               (if status-id-to-boost
                   (api-client:reblog-status status-id-to-boost)
                   (api-client:reblog-status status-id)))))
      (change-status-values object #'boost))))

(defclass unreblog-status-event (program-event event-with-message-index) ())

(defmethod process-event ((object unreblog-status-event))
  (tui:with-notify-errors
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
    (ui:focus-to-send-message-window :print-message nil)
    (windows:draw specials:*send-message-window*)))

(defclass send-message-change-mentions-event (program-event) ())

(defmethod process-event ((object send-message-change-mentions-event))
  (let ((new-mentions   (payload object))
        (message-data   (sending-message:message-data specials:*send-message-window*)))
    (setf (sending-message:mentions message-data) new-mentions)
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
                                          :selected-fg   bg))
           (win            specials:*send-message-window*))
      (line-oriented-window:append-new-rows win line)
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
  (let ((send-win specials:*send-message-window*))
    (with-accessors ((message-data sending-message:message-data)) send-win
      (with-accessors ((body       sending-message:body)
                       (subject    sending-message:subject)
                       (reply-to   sending-message:reply-to)
                       (mentions   sending-message:mentions)
                       (visibility sending-message:visibility)) message-data
        (let* ((attachments (line-oriented-window:map-rows send-win
                                                           #'line-oriented-window:normal-text)))
          (hooks:run-hook 'hooks:*before-sending-message* object)
          (msg-utils:maybe-crypt-message send-win
                                         :notify-cant-crypt (use-ui-notification-p object))
          (let ((exceeding-characters (ui:message-exceeds-server-limit-p body)))
            (if exceeding-characters
                (ui:exceeding-characters-notify exceeding-characters)
                (let ((actual-message-body (if (text-utils:string-not-empty-p mentions)
                                               (format nil
                                                       "~a~a~%~a"
                                                       +mention-prefix+
                                                       mentions
                                                       body)
                                               body)))
                  (client:send-status actual-message-body
                                      reply-to
                                      attachments
                                      subject
                                      (make-keyword (string-upcase visibility)))
                  (ui:notify (_ "Message sent."))
                  (ui:close-send-message-window)))))))))

(defun find-user-id-from-exact-acct (username)
  (when-let* ((remote-accounts-matching (api-client:search-user username
                                                                :limit   100
                                                                :resolve   t))
              (matched-account          (find-if (lambda (a)
                                                   (string= (tooter:account-name a)
                                                            username))
                                                 remote-accounts-matching)))
    (values (tooter:id matched-account)
            username)))

(defmacro with-process-follower ((username user-id
                                  &optional
                                    (local-complete-username-fn #'db:all-unfollowed-usernames))
                                 &body body)
  `(tui:with-notify-errors
     (let ((,user-id nil))
       (if (find ,username (,local-complete-username-fn) :test #'string=)
           (setf ,user-id (db:acct->id ,username))
           (setf ,user-id (find-user-id-from-exact-acct ,username)))
       (if ,user-id
           (progn ,@body)
           (error (format nil (_ "Unable to find user ~a") ,username))))))

(defclass follow-user-event (program-event) ())

(defmethod process-event ((object follow-user-event))
  (with-accessors ((username payload)) object
    (with-process-follower (username user-id db:all-unfollowed-usernames)
      (client:follow-user  user-id)
      (db:add-to-followers user-id)
      (ui:notify (format nil (_ "Followed  ~a") username)))))

(defclass unfollow-user-event (program-event) ())

(defmethod process-event ((object unfollow-user-event))
  (with-accessors ((username payload)) object
    (with-process-follower (username user-id db:all-followed-usernames)
      (client:unfollow-user  user-id)
      (db:remove-from-followers user-id)
      (ui:notify (format nil (_ "Unfollowed  ~a") username)))))

(defclass open-follow-requests-window-event (program-event) ())

(defmethod process-event ((object open-follow-requests-window-event))
  (tui:with-notify-errors
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
         (event             (make-instance 'save-timeline-in-db-event
                                           :payload       conversation-tree
                                           :timeline-type db:+default-converation-timeline+
                                           :folder        conversation-folder
                                           :localp        nil)))
           (push-event event)
    conversation-tree))

(defmethod process-event ((object update-conversations-event))
  (with-accessors ((new-timeline new-timeline)
                   (new-folder  new-folder)) object
    (tui:with-notify-errors
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
    (tui:with-notify-errors
      (api-client:delete-conversation id)
      (db:delete-conversation folder))))

(defclass update-mentions-event (program-event) ())

(defmethod process-event ((object update-mentions-event))
  (when-let* ((mentions       (api-client:update-mentions-folder :delete-mentions-on-server t))
              (mentions-count (length mentions))
              (thread-window  specials:*thread-window*))
    (when command-line:*notify-mentions*
      (loop for mention in mentions do
        (thread-window:add-mention thread-window mention))
      (ui:notify (format nil
                         (n_ "Got ~a notification"
                             "Got ~a notifications"
                             mentions-count)
                         mentions-count)))))

(defclass expand-thread-event (program-event event-with-timeline-and-folder)
  ((status-id
    :initform nil
    :initarg :status-id
    :accessor status-id)
   (force-saving-of-ignored-status
    :initform nil
    :initarg :force-saving-of-ignored-status
    :reader  force-saving-of-ignored-status-p
    :writer  (setf force-saving-of-ignored-status))))

(defmethod process-event ((object expand-thread-event))
  (with-accessors ((new-folder                       new-folder)
                   (new-timeline                     new-timeline)
                   (status-id                        status-id)
                   (force-saving-of-ignored-status-p force-saving-of-ignored-status-p)) object
    (api-client:expand-status-thread status-id
                                     new-timeline
                                     new-folder
                                     force-saving-of-ignored-status-p)))

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
    (tui:with-notify-errors
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

(defclass add-pagination-status-event (program-event)
  ((status-id
    :initform nil
    :initarg  :status-id
    :accessor status-id)
   (timeline
    :initform nil
    :initarg  :timeline
    :accessor timeline)
   (folder
    :initform nil
    :initarg  :folder
    :accessor folder)))

(defmethod process-event ((object add-pagination-status-event))
  (with-accessors ((status-id status-id)
                   (timeline  timeline)
                   (folder    folder)) object
    (db:add-to-pagination-status status-id folder timeline :ensure-no-duplicates t)))

(defclass poll-vote-event (program-event)
  ((poll-id
    :initform nil
    :initarg  :poll-id
    :accessor poll-id)
   (choices
    :initform ()
    :initarg  :choices
    :accessor choices)))

(defmethod process-event ((object poll-vote-event))
  (with-accessors ((poll-id  poll-id)
                   (choices  choices)) object
    (tui:with-notify-errors
      (api-client:poll-vote poll-id choices))))

(defclass gemini-display-data-page (program-event)
  ((window
    :initform nil
    :initarg :window
    :accessor window)
   (local-path
    :initform ""
    :initarg :local-path
    :accessor local-path)))

(defmethod process-event ((object gemini-display-data-page))
  (with-accessors ((page-data  payload)
                   (window     window)
                   (local-path local-path)) object
    (tui:with-notify-errors
      (let* ((parsed       (gemini-parser:parse-gemini-file page-data))
             (local-path-p (text-utils:string-not-empty-p local-path))
             (links        (gemini-parser:sexp->links parsed
                                                      nil
                                                      nil
                                                      local-path
                                                      :comes-from-local-file local-path-p))
             (ir-text      (gemini-parser:sexp->text-rows parsed
                                                          gemini-client:*gemini-page-theme*)))
        (setf (windows:keybindings window)
              keybindings:*gemini-message-keymap*)
        (gemini-viewer:maybe-initialize-metadata window)
        (refresh-gemini-message-window links page-data ir-text nil)
        (ui:open-gemini-toc)
        (windows:draw window)))))

(defclass gemini-request-event (program-event)
  ((url
    :initform nil
    :initarg  :url
    :accessor url)
   (use-cached-file-if-exists
    :initform nil
    :initarg :use-cached-file-if-exists
    :accessor use-cached-file-if-exists)
   (give-focus-to-message-window
    :initform t
    :initarg :give-focus-to-message-window
    :reader  give-focus-to-message-window-p
    :writer  (setf give-focus-to-message-window))
   (opening-gempub-file
    :initform nil
    :initarg :opening-gempub-file
    :reader  opening-gempub-file-p
    :writer  (setf opening-gempub-file))
   (enqueue
    :initform nil
    :initarg :enqueue
    :accessor enqueue)))

(defun relative-path->absolute (path)
  (fs:normalize-path (fs:prepend-pwd path)))

(defun render-directory-as-gemini-text (root-directory)
  (let* ((index-path (relative-path->absolute root-directory))
         (all-paths  (mapcar #'fs:normalize-path
                             (fs:collect-children index-path)))
         (link-lines ())
         (raw-text   (with-output-to-string (stream)
                       (write-sequence (gemini-parser:geminize-h1
                                        (format nil
                                                (_ "Index of local directory ~a~2%")
                                                index-path))
                                       stream))))

    (loop for path in all-paths do
      (let* ((dirp                  (fs:dirp path))
             (dir-symbol            (swconf:directory-symbol))
             (link-label            (if dirp
                                        (text-utils:strcat path " " dir-symbol)
                                        path))
             (encoded-path          (gemini-client::percent-encode-path path))
             (link                  (gemini-parser:render-gemini-link encoded-path link-label)))
        (push link link-lines)))
    (setf link-lines (sort link-lines #'string<))
    (text-utils:join-with-strings (append (list raw-text) link-lines)
                                  (format nil "~%"))))

(defmethod process-event ((object gemini-request-event))
  (tui:with-notify-errors
    (with-accessors ((url                            url) ; if a local file *not* percent encoded
                     (give-focus-to-message-window-p give-focus-to-message-window-p)
                     (use-cached-file-if-exists      use-cached-file-if-exists)
                     (enqueue                        enqueue)) object
      (let ((window     specials:*message-window*)
            (local-path (if (text-utils:percent-encoded-p url)
                            (complete:tilde-expand-string (text-utils:percent-decode url))
                            (complete:tilde-expand-string url))))
        (setf (windows:keybindings window)
              keybindings:*gemini-message-keymap*)
        (when give-focus-to-message-window-p
          (ui:focus-to-message-window))
        (cond
          ((text-utils:string-empty-p url)
           (ui:error-message (_ "Empty address")))
          ((gemini-client:absolute-gemini-url-p url)
           (gemini-viewer:bury-download-stream)
           (gemini-viewer:ensure-just-one-stream-rendering)
           (gemini-viewer:request url
                                  :enqueue                   enqueue
                                  :use-cached-file-if-exists use-cached-file-if-exists))
          ((fs:dirp local-path)
           (ui:open-file-explorer local-path))
          ((gempub:gempub-file-p local-path :ignore-errors t)
           (let ((temp-directory (fs:temporary-directory)))
             (os-utils:unzip-file local-path temp-directory)
             (let* ((library-entry (db:gempub-metadata-find local-path))
                    (index-file    (and library-entry
                                        (db:row-index-file library-entry))))
               (if index-file
                   (setf (url object) (fs:cat-parent-dir temp-directory index-file))
                   (setf (url object) temp-directory))
               (setf (opening-gempub-file object) t)
               (push-event object))))
          ((opening-gempub-file-p object)
           (let* ((file-string (fs:slurp-file local-path))
                  (parsed      (gemini-parser:parse-gemini-file file-string))
                  (parent-dir  (fs:parent-dir-path local-path))
                  (links       (gemini-parser:sexp->links parsed
                                                      nil
                                                      nil
                                                      parent-dir
                                                      :comes-from-local-file t))
                  (local-links (remove-if (lambda (link)
                                            (let ((target      (gemini-parser:target link)))
                                              (if target
                                                  (uri:scheme (iri:iri-parse target))
                                                  t)))
                                          links))
                  (event       (make-instance 'gemini-display-data-page
                                              :local-path parent-dir
                                              :window     window
                                              :payload    file-string)))
             (let ((*process-events-immediately* t))
               (push-event event))
             (ui:clean-all-tour)
             (ui:add-links-to-tour local-links)
             (gemini-viewer:push-url-to-history window local-path)))
          (t
           (handler-case
               (let* ((file-string (fs:slurp-file local-path))
                      (parent-dir  (fs:parent-dir-path local-path))
                      (event       (make-instance 'gemini-display-data-page
                                                  :local-path parent-dir
                                                  :window     window
                                                  :payload    file-string)))
                 (let ((*process-events-immediately* t))
                   (push-event event))
                 (gemini-viewer:push-url-to-history window local-path))
             (error (e) (ui:error-message (format nil "~a" e))))))))))

(defclass gemini-back-event (program-event) ())

(defmethod process-event ((object gemini-back-event))
  (push-downloading-behind)
  (gemini-viewer:history-back specials:*message-window*))

(defclass gemini-got-line-event (program-event)
  ((wrapper-object
    :initform nil
    :initarg  :wrapper-object
    :accessor wrapper-object)
   (append-text
    :initform t
    :initarg  :append-text
    :accessor append-text)
   (skip-rendering
    :initform nil
    :initarg  :skip-rendering
    :reader   skip-rendering-p
    :writer   (setf skip-rendering))))

(defun refresh-gemini-message-window (links source ir-rows append-text)
  (let* ((win             specials:*message-window*)
         (window-metadata (message-window:metadata win)))
    (with-accessors ((rows message-window::rows)) win
      (let ((new-rows      (message-window:text->rendered-lines-rows win
                                                                     ir-rows)))
        (if append-text
            (progn
              (line-oriented-window:append-new-rows win new-rows)
              (gemini-viewer:append-metadata-link window-metadata links)
              (gemini-viewer:append-metadata-source window-metadata source)
              (funcall (message-window:adjust-rows-strategy win) win))

            (progn
              (setf (gemini-viewer:gemini-metadata-source-file window-metadata) source)
              (setf (gemini-viewer:gemini-metadata-links window-metadata) links)
              (line-oriented-window:update-all-rows win new-rows)
              (line-oriented-window:adjust-selected-rows specials:*message-window*
                                                         #'line-oriented-window:adjust-rows-select-first)))))))

(defmethod process-event ((object gemini-got-line-event))
  (with-accessors ((response       payload)
                   (append-text    append-text)
                   (wrapper-object wrapper-object)) object
    (with-accessors ((status-code          gemini-client:status-code)
                     (status-code-message  gemini-client:status-code-message)
                     (meta                 gemini-client:meta)
                     (parsed-file          gemini-client:parsed-file)
                     (source-url           gemini-client:source-url)
                     (source               gemini-client:source)
                     (links                gemini-client:links)
                     (text-rendering-theme gemini-client:text-rendering-theme)) response
      (let* ((win     specials:*message-window*)
             (ir-line (gemini-parser:sexp->text-rows parsed-file
                                                     text-rendering-theme)))
        (when (and (gemini-viewer:downloading-allowed-p wrapper-object)
                   (not (skip-rendering-p object))
                   (message-window:display-gemini-text-p win))
          (refresh-gemini-message-window links source ir-line append-text)
          (windows:draw win)
          (when append-text
            (message-window:draw-downloading-animation win)))))))

(defclass gemini-abort-downloading-event (program-event) ())

(defmethod process-event ((object gemini-abort-downloading-event))
  (with-accessors ((iri payload)) object
    (gemini-viewer:abort-download-stream iri
                                         :remove-wainting-stream-event t
                                         :redraw-stream-window         t)))

(defclass gemini-abort-all-downloading-event (program-event) ())

(defmethod process-event ((object gemini-abort-all-downloading-event))
  (gemini-viewer:remove-all-db-stream)
  (remove-event-if (lambda (a) (typep a 'gemini-got-line-event))))

(defclass gemini-push-behind-downloading-event (program-event) ())

(defun push-downloading-behind ()
  (map-events (lambda (a)
                (when (typep a 'gemini-got-line-event)
                  (setf (skip-rendering a) t)
                  (setf (priority a) +minimum-event-priority+))
                a)))

(defmethod process-event ((object gemini-push-behind-downloading-event))
  (push-downloading-behind))

(defclass gemini-enqueue-download-event (program-event) ())

(defmethod process-event ((object gemini-enqueue-download-event))
  (with-accessors ((stream-object payload)) object
    (gemini-viewer:push-db-stream stream-object)))

(defclass gemini-gemlog-subscribe-event (program-event) ())

(defmethod process-event ((object gemini-gemlog-subscribe-event))
  (with-accessors ((url payload)) object
    (let ((subscribedp (gemini-subscription:subscribe url)))
      (if subscribedp
          (gemini-subscription:refresh url)
          (ui:notify (format nil
                             (_ "Unable to subscribe to ~s")
                             url)
                     :as-error t)))))

(defclass gemlog-cancel-subscription-event (program-event) ())

(defmethod process-event ((object gemlog-cancel-subscription-event))
  (with-accessors ((gemlog-url payload)) object
    (db:gemini-cancel-subscription gemlog-url)
    (handler-bind ((conditions:out-of-bounds
                     (lambda (e)
                       (invoke-restart 'line-oriented-window:set-default-index e))))
      (line-oriented-window:resync-rows-db specials:*gemini-subscription-window*
                                           :suggested-message-index 0
                                           :redraw                  t))))

(defclass gemlog-show-event (program-event)
  ((title
    :initarg :title
    :accessor title)
   (subtitle
    :initarg :subtitle
    :accessor subtitle)
   (gemlog-url
    :initarg  :gemlog-url
    :accessor gemlog-url)
   (entries
    :initarg :entries
    :accessor entries)))

(defmethod process-event ((object gemlog-show-event))
  (with-accessors ((title      title)
                   (subtitle   subtitle)
                   (entries    entries)
                   (gemlog-url gemlog-url)) object
    (let* ((gemini-page (with-output-to-string (stream)
                          (format stream
                                  "~a~2%"
                                  (gemini-parser:geminize-h1 title))
                          (if subtitle
                              (format stream
                                      "~a~2%"
                                      (gemini-parser:geminize-h2 subtitle))
                              (format stream
                                      "~a~2%"
                                      (gemini-parser:geminize-h2 (_ "No subtitle"))))
                          (loop for entry in entries do
                            (let* ((link  (db:row-post-link  entry))
                                   (date-format  (swconf:date-fmt swconf:+key-message-window+))
                                   (date  (db:row-post-date entry))
                                   (encoded-date (db-utils:encode-datetime-string date))
                                   (title (text-utils:strcat (format-time encoded-date date-format)
                                                             " "
                                                             (db:row-post-title entry)))
                                   (seenp (db-utils:db-not-nil-p (db:row-post-seenp entry))))
                              (format stream
                                      (_ "~a ~:[(not opened)~;(opened)~]~%")
                                      (gemini-parser:render-gemini-link link
                                                                      title)
                                      seenp)))))
           (url      (iri:iri-parse gemlog-url))
           (parsed   (gemini-parser:parse-gemini-file gemini-page))
           (links    (gemini-parser:sexp->links parsed
                                                (uri:host url)
                                                (uri:port url)
                                                (uri:path url)))
           (theme   gemini-client:*gemini-page-theme*))
      (gemini-viewer:maybe-initialize-metadata specials:*message-window*)
      (refresh-gemini-message-window links
                                     gemini-page
                                     (gemini-parser:sexp->text-rows parsed theme)
                                     nil)
      (setf (windows:keybindings specials:*message-window*)
            keybindings:*gemini-message-keymap*)
      (windows:draw  specials:*message-window*))))

(defclass gemlog-refresh-thread  (program-event) ())

(defmethod process-event ((object gemlog-refresh-thread))
  (let* ((subscription         (payload object))
         (notification-message (format nil (_ "updating gemlog ~a") subscription)))
    (ui:notify-procedure (lambda ()
                           (db-utils:with-ready-database ()
                             (ignore-errors
                              (gemini-subscription:refresh subscription))))
                         notification-message
                         :ending-message nil)))

(defclass gemlog-refresh-all-event (program-event) ())

(defmethod process-event ((object gemlog-refresh-all-event))
  (let ((all-subscribed-gemlogs (mapcar #'db:row-url (db:gemini-all-subscriptions))))
    (loop for subscription in all-subscribed-gemlogs do
      (let ((event (make-instance 'gemlog-refresh-thread
                                  :payload  subscription
                                  :priority +minimum-event-priority+)))
        (push-event event)))))

(defclass gemini-toc-jump-to-section (program-event)
  ((toc-win
    :initform nil
    :initarg  :toc-win
    :accessor toc-win)
   (message-win
    :initform nil
    :initarg  :message-win
    :accessor message-win)
   (gid-looking-for
    :initform nil
    :initarg  :gid-looking-for
    :accessor gid-looking-for)))

(defmethod process-event ((object gemini-toc-jump-to-section))
  (with-accessors ((toc-win          toc-win)
                   (message-win      message-win)
                   (gid-looking-for  gid-looking-for)) object
    (let* ((selected-row    (line-oriented-window:selected-row-fields toc-win))
           (gid-looking-for (message-window:gemini-toc-group-id selected-row)))
      (message-window:jump-to-group-id message-win gid-looking-for))))

(defclass gemini-toc-open (program-event) ())

(defmethod process-event ((object gemini-toc-open))
  (let ((message-win specials:*message-window*)
        (toc-win     specials:*gemini-toc-window*))
    (cond
      ((not (message-window:gemini-window-p* message-win))
       (ui:error-message (_ "TOC can be shown for gemini windows only.")))
      ((and toc-win
            (windows:win-shown-p toc-win))
       (line-oriented-window:resync-rows-db toc-win :suggested-message-index 0))
      (t
       (gemini-page-toc:open-toc-window message-win)))))

;;;; pleroma

(defclass get-chat-messages-event (program-event)
  ((chat-id
    :initform nil
    :initarg  :chat-id
    :accessor chat-id)
   (min-message-id
    :initform nil
    :initarg  :min-message-id
    :accessor min-message-id)))

(defmethod process-event ((object get-chat-messages-event))
  (with-accessors ((chat-id        chat-id)
                   (min-message-id min-message-id)) object
    (let ((messages (api-pleroma:get-chat-messages chat-id min-message-id)))
      (dolist (message messages)
        (db:update-db message)
        (when (and specials:*chats-list-window*
                   (windows:win-shown-p specials:*chats-list-window*))
          (line-oriented-window:resync-rows-db specials:*chats-list-window*))))))

(defclass get-chats-event (program-event) ())

(defmethod process-event ((object get-chats-event))
  (with-accessors ((chat-id        chat-id)
                   (min-message-id min-message-id)) object
    (tui:with-notify-errors
      (let ((chats (api-pleroma:get-chats)))
        (dolist (chat chats)
          (db:update-db chat))))))

(defclass update-all-chat-messages-event (program-event) ())

(defmethod process-event ((object update-all-chat-messages-event))
  (tui:with-notify-errors
    (let ((all-chats (db:all-chats)))
      (dolist (chat all-chats)
        (let* ((chat-id (db:row-id chat))
               (min-id  (db:last-chat-message-id chat-id)))
          (process-event (make-instance 'program-events:get-chat-messages-event
                                        :chat-id        chat-id
                                        :min-message-id min-id)))))))

(defclass chat-show-event (program-event)
  ((chat
    :initform nil
    :initarg :chat
    :accessor chat)))

(defmethod process-event ((object chat-show-event))
  (with-accessors ((chat chat)) object
    (let* ((chat-id (db:row-id chat)))
      (db:mark-all-chat-messages-read chat-id)
      (setf (windows:keybindings specials:*message-window*)
            keybindings:*chat-message-keymap*)
      (message-window:prepare-for-rendering specials:*message-window*
                                            (chats-list-window:chat->text chat))
      (message-window:scroll-end specials:*message-window*)
      (setf (message-window:metadata specials:*message-window*)
            chat)
      (line-oriented-window:adjust-selected-rows specials:*message-window*
                                                 #'line-oriented-window:adjust-rows-select-last)
      (windows:draw specials:*message-window*))))

(defclass chat-post-message-event (program-event)
  ((message
    :initform nil
    :initarg :message
    :accessor message)
   (chat-id
    :initform nil
    :initarg  :chat-id
    :accessor chat-id)))

(defmethod process-event ((object chat-post-message-event))
  (with-accessors ((message message)
                   (chat-id chat-id)) object
    (api-pleroma:post-on-chat chat-id message)))

(defclass chat-change-label-event (program-event)
  ((label
    :initform nil
    :initarg :label
    :accessor label)
   (chat-id
    :initform nil
    :initarg  :chat-id
    :accessor chat-id)))

(defmethod process-event ((object chat-change-label-event))
  (with-accessors ((label label)
                   (chat-id chat-id)) object
    (db:chat-change-label chat-id label)
    (line-oriented-window:resync-rows-db specials:*chats-list-window*)))

(defclass chat-create-event (program-event)
  ((user-id
    :initform nil
    :initarg :user-id
    :accessor user-id)
   (chat-label
    :initform (_ "no label")
    :initarg  :chat-label
    :accessor chat-label)))

(defmethod process-event ((object chat-create-event))
  (with-accessors ((chat-label chat-label)
                   (user-id    user-id)) object
    (let ((chat (api-pleroma:create-new-chat user-id)))
      (db:update-db chat)
      (process-event (make-instance 'chat-change-label-event
                                    :chat-id (api-pleroma:chat-id chat)
                                    :label   chat-label)))))

(defclass search-link-event (search-event)
  ((window
    :initform nil
    :initarg :window
    :accessor window)
   (regex
    :initform nil
    :initarg :regex
    :accessor regex)))

(defmethod process-event ((object search-link-event))
  (with-accessors ((window window)
                   (regex  regex)) object
    (line-oriented-window:search-row window regex)))

(defclass search-toc-event (search-event)
  ((window
    :initform nil
    :initarg :window
    :accessor window)
   (regex
    :initform nil
    :initarg :regex
    :accessor regex)))

(defmethod process-event ((object search-toc-event))
  (with-accessors ((window window)
                   (regex  regex)) object
    (line-oriented-window:search-row window regex)
    (ui:gemini-toc-jump-to-entry)))

(defclass help-apropos-event (program-event)
  ((regex
    :initform nil
    :initarg :regex
    :accessor regex)
   (global
    :initform nil
    :initarg :globalp
    :reader  globalp
    :writer  (setf global))))

(defmethod process-event ((object help-apropos-event))
  (with-accessors ((regex regex)) object
    (keybindings:print-help specials:*main-window*
                            :regex regex
                            :global-search (globalp object))))

(defclass redraw-window-event (program-event) ())

(defmethod process-event ((object redraw-window-event))
  (with-accessors ((window payload)) object
    (windows:draw window)))

(defclass send-to-pipe-event (program-event)
  ((data
    :initform nil
    :initarg :data
    :accessor data)
   (command
    :initform nil
    :initarg :command
    :accessor command)))

(defmethod process-event ((object send-to-pipe-event))
  (with-accessors ((data    data)
                   (command command)) object
      (tui:with-print-error-message
        (os-utils:send-to-pipe data command))))

;;;; general usage

(defclass function-event (program-event) ())

(defmethod process-event ((object function-event))
  (with-accessors ((payload payload)) object
    (assert (functionp payload))
    (funcall payload)))

(defmacro with-enqueued-process ((&optional (priority +standard-event-priority+)) &body body)
  `(push-event (make-instance 'function-event
                              :payload  (lambda () ,@body)
                              :priority ,priority)))

;;;; end events

(defun dispatch-program-events ()
  (when (event-available-p)
    (let ((bypassable-event (pop-event)))
      (if (and (= (priority bypassable-event)
                  +minimum-event-priority+)
               (event-available-p))
          (let ((event (pop-event)))
            (reinitialize-id bypassable-event)
            (push-event bypassable-event)
            (process-event event))
          (process-event bypassable-event)))))
