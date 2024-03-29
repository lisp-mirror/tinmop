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

(in-package :scheduled-events)

(define-constant +refresh-all-chats-data-frequency+       10000 :test #'=)

(define-constant +refresh-all-chats-messages-frequency+      50 :test #'=)

(define-constant +refresh-gemlog-subscriptions-frequency+ 50000 :test #'=)

(define-constant +purge-gemlog-entries+                   30000 :test #'=)

(defun triggedp (ticks frequency)
  (= (rem ticks frequency)
     0))

(defmacro gen-scheduler-function ((name frequency) &body body-if-triggered)
  `(defun ,(format-fn-symbol t "~a" name) (ticks)
     (when (triggedp ticks ,frequency)
       ,@body-if-triggered)))

(defmacro gen-at-boot-function (name &body body)
  `(let ((triggedp nil))
     (defun ,(format-fn-symbol t "~a" name) ()
       (when (null triggedp)
         (setf triggedp t)
         ,@body))))

(gen-scheduler-function (refresh-all-chats-data
                         +refresh-all-chats-data-frequency+)
  (ui:notify (_ "Updating all chats."))
  (ui:update-all-chats-data))

(gen-scheduler-function (refresh-gemlog-subscriptions
                         +refresh-gemlog-subscriptions-frequency+)
  (ui:gemlog-refresh-all))

(gen-scheduler-function (purge-gemlog-entries +purge-gemlog-entries+)
  (ui:notify (_ "Removing old gemlog posts…"))
  (db:purge-seen-gemlog-entries)
  (ui:notify (_ "Removed")))

(gen-scheduler-function (refresh-all-chats-messages
                         +refresh-all-chats-messages-frequency+)
  (when (message-window:display-chat-p *message-window*)
    (ui:update-all-chats-messages)
    (let ((show-event (make-instance 'program-events:chat-show-event
                                     :chat (message-window:metadata *message-window*))))
      (program-events:push-event show-event))))

(gen-at-boot-function purge-history
  (db:purge-history))

(gen-at-boot-function refresh-gemlog-posts
  (when (swconf:gemini-update-gemlog-at-start-p)
    (ui:gemlog-refresh-all)))

(gen-at-boot-function sync-gempub-library
  (gempub:sync-library :notify t))

(defun run-scheduled-events (ticks)
  (refresh-all-chats-messages ticks)
  (refresh-all-chats-data ticks)
  (refresh-gemlog-subscriptions ticks)
  (purge-gemlog-entries ticks)
  (purge-history)
  (refresh-gemlog-posts)
  (sync-gempub-library))
