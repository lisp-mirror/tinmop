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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :api-pleroma)

(defgeneric delete-notification (object id))

(defmethod delete-notification ((object tooter:client) (id string))
  (tooter:submit object
                 "/api/v1/notifications/dismiss"
                 :id id))

(defmethod delete-notification ((object tooter:client) (notification tooter:notification))
  (delete-notification object (tooter:id notification)))

(defmacro gen-translate-fn (class-name)
  `(defun ,(misc:format-fn-symbol t "decode-~a" class-name) (a)
     (tooter:decode-entity ',class-name a)))

(gen-translate-fn tooter:account)

(gen-translate-fn tooter:emoji)

(gen-translate-fn tooter:attachment)

(gen-translate-fn tooter:card)

(tooter:define-entity chat-message
  (message-id :field "id")
  (unreadp :field "unread")
  (emojis :translate-with #'decode-emoji)
  (updated-at :translate-with #'tooter:convert-timestamp)
  (content)
  (chat-id :field "chat_id")
  (card :translate-with #'decode-card)
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

(defgeneric create-chat (object user-id))

(defmethod create-chat ((object tooter:client) (account-id string))
  "Connect with `client' and create a new chat with user identified by `account-id'"
  (decode-chat (tooter:submit object
                              (format nil
                                      "/api/v1/pleroma/chats/by-account-id/~a"
                                      account-id))))

(defgeneric get-all-chats (object))

(defmethod get-all-chats ((object tooter:client))
  "Geat a list o all chats, ordered from the more recent updated."
  (decode-chat (tooter:query object "/api/v1/pleroma/chats")))

(defgeneric post-chat-message (object chat-id content media))

(defun post-chat-path (chat-id)
  (format nil "/api/v1/pleroma/chats/~a/messages" chat-id))

(defmethod post-chat-message ((object tooter:client) (chat-id string) (content string) (media null))
  "Post a message to chat identified by `chat-id' with text `content` or
media `media'."
  (decode-chat-message (tooter:submit object
                                      (post-chat-path chat-id)
                                      :content  content
                                      :media-id media)))

(defmethod post-chat-message ((object tooter:client) (chat-id string) (content null) (media string))
  "Post a message to chat identified by `chat-id' with text `content` or
media `media'. Returns a `chat-message' instance"
  (let ((path-media (fs:namestring->pathname media)))
    (decode-chat-message (tooter:submit object
                                        (post-chat-path chat-id)
                                        :content  content
                                        :media-id (tooter:id (tooter:make-media object
                                                                                path-media))))))

(defgeneric fetch-chat-messages (object chat-id &key min-id))

(defmethod fetch-chat-messages ((object tooter:client) chat-id &key (min-id nil))
  (decode-chat-message (tooter:query object
                                     (format nil
                                             "/api/v1/pleroma/chats/~a/messages"
                                             chat-id)
                                     :min-id min-id)))

(defgeneric delete-chat-message (object chat-id message-id))

(defmethod delete-chat-message ((object tooter:client) (chat-id string) (message-id string))
  (decode-chat-message (tooter:submit object
                                      (format nil
                                              "/api/v1/pleroma/chats/~a/messages/~a"
                                              chat-id
                                              message-id)
                                      :http-method :delete)))
