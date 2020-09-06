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

(in-package :api-pleroma)

(defmacro gen-translate-entity-field-fn (class-name)
  `(defun ,(misc:format-fn-symbol t "decode-~a" class-name) (a)
     (tooter:decode-entity ',class-name a)))

(gen-translate-entity-field-fn tooter:account)

(gen-translate-entity-field-fn tooter:emoji)

(gen-translate-entity-field-fn tooter:attachment)

(gen-translate-entity-field-fn tooter:card)

(tooter:define-entity chat-message
  (message-id :field "id")
  (unreadp    :field "unread")
  (emojis     :translate-with #'decode-emoji)
  (updated-at :translate-with #'tooter:convert-timestamp)
  (created-at :translate-with #'tooter:convert-timestamp)
  (content)
  (chat-id    :field "chat_id")
  (card       :translate-with #'decode-card)
  (attachment :translate-with #'decode-attachment)
  (account-id :field "account_id"))

(defmethod print-object ((object chat-message) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((message-id message-id)
                     (chat-id    chat-id)
                     (unreadp    unreadp)
                     (content    content)
                     (account-id account-id)
                     (attachment attachment)) object
     (format stream
              "chat ~a id ~a unread ~a content ~s sender account ~a attachment ~a"
              chat-id
              message-id
              unreadp
              content
              account-id
              attachment))))

(tooter:define-entity chat
  (chat-id :field "id")
  (updated-at :translate-with #'tooter:convert-timestamp)
  (created-at :translate-with #'tooter:convert-timestamp)
  (unread-count :field "unread")
  (last-message :field "last_message" :translate-with #'decode-chat-message)
  (account :translate-with #'decode-account))

(defmethod print-object ((object chat) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((chat-id      chat-id)
                     (updated-at   updated-at)
                     (unread-count unread-count)
                     (last-message last-message)
                     (account      account)) object
      (format stream
              "id ~a updated-at ~a unread ~a last-message ~a account ~a"
              chat-id
              updated-at
              unread-count
              last-message
              account))))
