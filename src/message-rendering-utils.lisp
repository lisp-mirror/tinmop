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

(in-package :message-rendering-utils)

(defun mention-p (maybe-mention)
  (scan (strcat "^" +mention-prefix+)
        maybe-mention))

(defun add-mention-prefix (username)
  (if (mention-p username)
      username
      (strcat +mention-prefix+ username)))

(defun strip-mention-prefix (maybe-mention)
  (if (not (mention-p maybe-mention))
      maybe-mention
      (subseq maybe-mention (length +mention-prefix+))))

(defun find-first-mention-in-message (message-body)
  (when message-body
    (with-input-from-string (body-stream message-body)
      (when-let* ((first-line    (read-line body-stream nil ""))
                  (mentions      (split-words first-line))
                  (first-mention (first mentions)))
        (when (mention-p first-mention)
          first-mention)))))

(defun local-mention->acct (text-line usernames-table)
  "Substitute in  `text-line' '@user' with '@user@server',  if '@user'
  is found as key in the alist `usernames-table'"
  (flet ((find-all-username (key)
           (let ((found (mapcar #'cdr
                                (remove-if-not (lambda (a) (string= (car a) key))
                                               usernames-table))))
             (join-with-strings found ", "))))
    (let ((results text-line))
      (loop for pair in usernames-table do
           (when-let* ((local-mention    (car pair))
                       (local-mention-re (strcat " " local-mention))
                       (actual-mention   (find-all-username local-mention)))
             (setf results (regex-replace-all local-mention-re results actual-mention))))
      results)))

(defun crypto-message-destination-user (message-data)
  (with-accessors ((body       sending-message:body)
                   (subject    sending-message:subject)
                   (reply-to   sending-message:reply-to)
                   (visibility sending-message:visibility)) message-data
    (when (string= visibility
                   +status-direct-visibility+)
      (if reply-to
          (let ((reply-username (status-id->username reply-to)))
            (db:username->id reply-username))
          (when-let* ((mention  (find-first-mention-in-message body))
                      (user     (db:user-exists-p (msg-utils:strip-mention-prefix mention)))
                      (username (db:row-user-username user)))
            (db:username->id username))))))

(defun maybe-crypt-message (send-message-window &key (notify-cant-crypt nil))
  (with-accessors ((message-data sending-message:message-data)
                   (rows         line-oriented-window:rows)) send-message-window
    (with-accessors ((body       sending-message:body)
                     (subject    sending-message:subject)
                     (reply-to   sending-message:reply-to)
                     (visibility sending-message:visibility)) message-data
      (when (string= visibility
                     +status-direct-visibility+)
        (let ((destination-user-id (crypto-message-destination-user message-data)))
          (if (null destination-user-id)
              (when notify-cant-crypt
                (ui:notify (_ "This message will *not* be crypted")))
              (let* ((destination-username (db:user-id->username destination-user-id))
                     (destination-mention  (msg-utils:add-mention-prefix destination-username))
                     (new-body             (misc:make-fresh-array 0 #\Space 'character nil))
                     (crypto-key           (db:crypto-user-key destination-username)))
                (if (null crypto-key)
                    (when notify-cant-crypt
                      (ui:notify (format nil
                                         (_ "No key to crypt message for ~s found")
                                         destination-username)))
                    (with-output-to-string (body-stream new-body)
                      ;; add username on top
                      (format body-stream "~a~%" destination-mention)
                      (format body-stream
                              "~a~%"
                              (crypto-utils:encrypt-message body crypto-key))
                      (setf body new-body))))))))))

(defun find-crypto-data (text)
  (with-input-from-string (stream text)
    (loop for line = (read-line stream nil nil)
       while line
       do
         (when (crypto-utils:crypto-text-p line)
           (return-from find-crypto-data line))))
  nil)

(defun maybe-decrypt-message (message-row message-text &key (notify-cant-decrypt nil))
  (let* ((username      (db:row-message-username message-row))
         (html-stripped (html-utils:html->text (db:row-message-content message-row)
                                               :add-link-footnotes nil))
         (mention       (find-first-mention-in-message html-stripped))
         (reply-p       (db:row-message-reply-to-id message-row))
         (crypto-key    (cond
                          (reply-p
                           (db:crypto-user-key username))
                          (mention
                           (db:crypto-user-key (msg-utils:strip-mention-prefix mention)))
                          (t
                           nil)))
         (crypto-data   (find-crypto-data message-text)))
    (if crypto-data
        (if (and (null crypto-key)
                 notify-cant-decrypt)
            (ui:notify (format nil
                               (_ "Unable to find the crypto key for user ~s.")
                               username)
                       :as-error t)
            (crypto-utils:decrypt-message crypto-data crypto-key))
        nil)))

(defun attachment-type->description (type)
  (macrolet ((gen-cond (key types descriptions)
             `(cond
                ,@(append
                   (loop
                      for type in types
                      for description in descriptions collect
                        `((string-equal ,type ,key) ,description))
                  `((t
                     (_ "invalid type")))))))
    (gen-cond type
              ("unknown"        "image"     "gifv"     "video"     "audio")
              ((_ "unknown") (_ "image") (_ "gifv") (_ "video") (_ "audio")))))

(defun attachment-type->metadata (type row)
  (let ((data (misc:make-fresh-array 0 #\a 'character nil)))
    (with-output-to-string (stream data)
      (format stream
              (_ "description: ~a~%")
              (db-utils:db-getf row :description))
      (format stream
              (_ "size: ~aX~a pixels~%")
              (db-utils:db-getf row :width)
              (db-utils:db-getf row :height))
      (when (or (string-equal type "gifv")
                (string-equal type "video")
                (string-equal type "audio"))
        (format stream
                (_"duration: ~a~%")
                (db-utils:db-getf row :duration))))
    data))

(defun status-attachments->text (status-id)
  (let ((text (misc:make-fresh-array 0 #\Space 'character nil)))
    (when-let* ((all-attachments (db:all-attachments-to-status status-id)))
      (with-output-to-string (stream text)
        (multiple-value-bind (header-prefix header-postfix header-value)
            (swconf:message-window-attachments-header)
          (let ((actual-header-value (or header-value
                                         (_ "Attachments"))))
            (format stream (strcat header-prefix
                                   actual-header-value
                                   header-postfix))))
        (loop for attachment in all-attachments do
             (let ((type (db-utils:db-getf attachment
                                           :type
                                           (_ "unknown"))))
               (format stream
                       (_"type: ~a~%metadata~%~a~%address: ~a~2%")
                       (attachment-type->description type)
                       (attachment-type->metadata    type attachment)
                       (db-utils:db-getf attachment :url  (_ "unknown")))))))
    text))

(defgeneric message-original->text-body (object &key &allow-other-keys))

(defmethod message-original->text-body ((object string) &key &allow-other-keys)
  (let* ((raw-body (html-utils:html->text object)))
    (emoji-shortcodes:emojify raw-body)))

(defun prepend-crypto-marker (decrypted-text)
  (format nil "~a~2%~a" (swconf:crypted-mark-value) decrypted-text))

(defmethod message-original->text-body ((object null) &key &allow-other-keys)
  (declare (ignore object))
  "")

(defmethod message-original->text-body ((object list)
                                        &key
                                          (notify-cant-decrypt nil)
                                          (try-decrypt         nil)
                                          &allow-other-keys)
  (let ((as-text (message-original->text-body (db:row-message-content object))))
    (if try-decrypt
        (let ((decrypted (maybe-decrypt-message object
                                                as-text
                                                :notify-cant-decrypt notify-cant-decrypt)))
          (if decrypted
              (message-original->text-body (prepend-crypto-marker decrypted))
              as-text))
        as-text)))

(defun message-original->text-header (message-row)
  (let* ((date-format      (swconf:date-fmt swconf:+key-message-window+))
         (username         (db:row-message-username          message-row))
         (display-name     (db:row-message-user-display-name message-row))
         (creation-time    (db:row-message-creation-time     message-row))
         (lockedp          (db-utils:db-not-nil-p (db:row-lockedp message-row)))
         (locked-mark      (swconf:message-window-account-locking-status-mark lockedp))
         (encoded-date     (db-utils:encode-datetime-string creation-time))
         (from-label       (_ "From: "))
         (boosted-label    (_ "Boosted: "))
         (boosted-id       (db:row-message-reblog-id message-row))
         (boosted-username (and boosted-id
                                (db:status-id->username boosted-id)))
         (date-label     (_ "Date: "))
         (padding-length (max (length from-label)
                              (length date-label)
                              (length boosted-label)))
         (text           (misc:make-fresh-array 0 #\Space 'character nil)))
    (with-output-to-string (stream text)
      (format stream
              "~a(~a) ~a~a~%"
              (right-padding from-label padding-length)
              display-name username locked-mark)
      (format stream "~a~a~2%"
              (right-padding date-label padding-length)
              (format-time encoded-date date-format))
      (when boosted-id
        (format stream "~a~a~%"
                (right-padding boosted-label padding-length)
                boosted-username)))
    text))

(defun poll->text (poll-id width)
  (when poll-id
    (when-let* ((poll                 (db:find-poll poll-id))
                (options              (db:all-poll-options poll-id))
                (all-titles           (loop for option in options collect
                                         (db:row-title option)))
                (all-rendered-indices (loop for idx from 0 below (length all-titles) collect
                                          (format nil "[~a] " idx)))
                (vote-sum             (reduce #'+
                                       (mapcar #'db:row-votes-count options)))
                (max-title-w          (find-max-line-length all-titles))
                (max-index-w          (find-max-line-length all-rendered-indices))
                (max-bar-width        (- width
                                         max-title-w
                                         max-index-w
                                         6))
                (bar-char             (swconf:vote-vertical-bar)))
      (let ((expiredp              (db:row-poll-expired-p       poll))
            (multiple-vote-allowed (db:row-poll-multiple-vote-p poll)))
        (with-output-to-string (stream)
          (format stream "~3%")
          (loop
             for title  in all-titles
             for index  in all-rendered-indices
             for option in options
             do
               (let* ((padded-title (left-padding title max-title-w))
                      (padded-index (left-padding index max-index-w))
                      (rate         (handler-case
                                        (/ (db:row-votes-count option)
                                           vote-sum)
                                      (error () 0)))
                      (vote         (left-padding (format nil "~f%" (* 100 rate)) 4))
                      (bar-w        (truncate (* rate max-bar-width))))
                 (format stream "~a~a " padded-index padded-title)
                 (loop for i from 0 below bar-w do
                      (princ bar-char stream))
                 (format stream " ~a~%" (left-padding vote (+ 4 ; size of vote percent: ' nnn%'
                                                              (- max-bar-width bar-w))))))
          (if multiple-vote-allowed
              (format stream "~%~a~%" (_ "Multiple choices allowed"))
              (format stream "~%~a~%" (_ "A single choice allowed")))
          (when expiredp
            (format stream "~%~a~%" (_ "The poll has expired"))))))))

(defun signature ()
  (when-let ((signature-file (swconf:signature-file-path)))
    (format nil "~%-- ~%~a" (fs:slurp-file signature-file))))
