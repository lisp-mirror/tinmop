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

(in-package :chats-list-window)

(defclass chats-list-window (focus-marked-window
                                 simple-line-navigation-window
                                 title-window
                                 border-window)
  ())

(defmethod refresh-config :after ((object chats-list-window))
  (open-attach-window:refresh-view-links-window-config object
                                                       swconf:+key-open-gemini-stream-window+)
  (let* ((win-w (truncate (* (win-width  specials:*main-window*) 1/2)))
         (win-h (truncate (* (win-height specials:*main-window*) 1/2)))
         (x           (truncate (- (/ (win-width specials:*main-window*) 2)
                                   (/ win-w 2))))
         (y           (truncate (- (/ (win-height specials:*main-window*) 2)
                                   (/ win-h 2)))))
    (win-resize object win-w win-h)
    (win-move object x y)
    object))

(defun chat->list-item (chat-db-row)
  (format nil
          (_ "~@[~a~]~@[~a~] ~a unread: ~a")
          (db:row-id    chat-db-row)
          (db:row-label chat-db-row)
          (db:user-id->username (db:row-account-id chat-db-row))
          (db:count-unread-chat-messages (db:row-id chat-db-row))))

(defun chat->text (chat-db-row)
  (with-output-to-string (stream)
    (let ((all-messages (db:all-chat-messages (db:row-id chat-db-row))))
      (dolist (message all-messages)
        (let* ((date-fmt               (or (swconf:date-fmt swconf:+key-chat-window+)
                                           (swconf:date-fmt swconf:+key-thread-window+)))
               (created-date           (db:row-created-at message))
               (encoded-created-date   (db-utils:encode-datetime-string created-date))
               (formatted-created-date (format-time encoded-created-date date-fmt))
               (attachment             (db:attachment-to-chat-message (db:row-id message)))
               (attachment-type        (if attachment
                                           (format nil "~a attachment" (db:row-type attachment))
                                           ""))
               (content                (or (db:row-message-content message)
                                           ""))
               (username               (db:user-id->username (db:row-account-id message))))
          (format stream
                  (_ "~a ~a said:~%~a ~a~2%")
                  formatted-created-date
                  username
                  content
                  attachment-type))))))

(defmethod resync-rows-db ((object chats-list-window)
                           &key
                             (redraw t)
                             (suggested-message-index nil))
  (with-accessors ((rows             rows)
                   (selected-line-bg selected-line-bg)
                   (selected-line-fg selected-line-fg)) object
    (flet ((make-rows (chats bg fg)
             (mapcar (lambda (chat)
                       (make-instance 'line
                                      :normal-text   (chat->list-item chat)
                                      :selected-text (chat->list-item chat)
                                      :fields        chat
                                      :normal-bg     bg
                                      :normal-fg     fg
                                      :selected-bg   fg
                                      :selected-fg   bg))
                     chats)))
      (with-croatoan-window (croatoan-window object)
        (setf rows (make-rows (db:all-chats)
                              selected-line-bg
                              selected-line-fg))
        (when suggested-message-index
          (select-row object suggested-message-index))
        (when redraw
          (draw object))))))

(defun open-chats-list-window ()
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf  *chats-list-window*
          (make-instance 'chats-list-window
                         :top-row-padding   0
                         :title             (_ "Current chats")
                         :single-row-height 1
                         :uses-border-p     t
                         :keybindings       keybindings:*chats-list-keymap*
                         :croatoan-window   low-level-window))
    (refresh-config  *chats-list-window*)
    (resync-rows-db  *chats-list-window* :redraw nil)
    (when (rows  *chats-list-window*)
      (select-row  *chats-list-window* 0))
    (draw  *chats-list-window*)
     *chats-list-window*))
