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

(defgeneric create-chat (object user-id))

(defmethod create-chat ((object tooter:client) (account-id string))
  "Connect with `client' and create a new chat with user identified by `account-id'"
  (decode-chat (tooter:submit object
                              (format nil
                                      "/api/v1/pleroma/chats/by-account-id/~a"
                                      account-id))))


(defgeneric get-chats-list (object &key with-muted-p max-id min-id since-id offset limits))

(defmethod get-chats-list ((object tooter:client)
                          &key
                            (with-muted-p t)
                            (max-id nil)
                            (min-id nil)
                            (since-id nil)
                            (offset 0)
                            (limits 200))
  "Get a list of all chats, ordered from the more recent updated. Note: uses version 2 of the API."
  (decode-chat (tooter:query object
                             "/api/v2/pleroma/chats"
                             :with-muted with-muted-p
                             :max-id     max-id
                             :min-id     min-id
                             :since-id   since-id
                             :offset     offset
                             :limits     limits)))

(defgeneric get-all-chats-v2 (object min-id &key &allow-other-keys))

(defmethod get-all-chats-v2 ((object tooter:client) min-id &key (accum ()))
  "Get a list of all chats, ordered from the more recent updated."
  (let ((chats (api-client:sort-id< (get-chats-list object))))
    (if chats
        (let ((new-min-id (tooter:id (last-elt chats))))
          (get-all-chats-v2 object object new-min-id (append chats accum)))
        (api-client:sort-id< accum))))

(defgeneric get-all-chats (object))

(defmethod get-all-chats ((object tooter:client))
  "Get a list of all chats, ordered from the more recent updated."
  (decode-chat (tooter:query object "/api/v1/pleroma/chats")))

(defgeneric post-chat-message (object chat-id content media))

(defun post-chat-path (chat-id)
  (format nil "/api/v1/pleroma/chats/~a/messages" chat-id))

(defmethod post-chat-message ((object tooter:client)
                              (chat-id string)
                              (content string)
                              (media null))
  "Post a message to chat identified by `chat-id' with text `content` or
media `media'."
  (decode-chat-message (tooter:submit object
                                      (post-chat-path chat-id)
                                      :content  content
                                      :media-id media)))

(defmethod post-chat-message ((object tooter:client)
                              (chat-id string)
                              (content null)
                              (media string))
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

(api-client:defun-api-call get-chat-messages (chat-id min-id)
  (fetch-chat-messages api-client:*client* chat-id :min-id min-id))

(api-client:defun-api-call get-chats ()
  (get-all-chats api-client:*client*))

(api-client:defun-api-call post-on-chat (chat-id message)
  (if (cl-ppcre:scan "^/" message)
      (api-pleroma:post-chat-message api-client:*client* chat-id nil message)
      (api-pleroma:post-chat-message api-client:*client* chat-id message nil)))

(api-client:defun-api-call create-new-chat (user-id)
  (create-chat api-client:*client* user-id))
