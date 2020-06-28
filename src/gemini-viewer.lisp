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

(in-package :gemini-viewer)

(defstruct gemini-metadata
  (links)
  (history))

(defun add-url-to-history (window url)
  (let* ((metadata (message-window:metadata window))
         (history  (reverse (gemini-metadata-history metadata))))
    (setf (gemini-metadata-history metadata)
          (reverse (push url history)))))

(defun maybe-initialize-metadata (window)
  (when (not (gemini-metadata-p (message-window:metadata window)))
    (setf (message-window:metadata window)
          (make-gemini-metadata)))
  (message-window:metadata window))

(defun request (url)
  (let ((parsed-uri (puri:parse-uri url)))
    (maybe-initialize-metadata specials:*message-window*)
    (if (null parsed-uri)
        (ui:error-message (format nil
                                  (_ "Could not understand the address ~s")
                                  url))
        (let ((host  (puri:uri-host  parsed-uri))
              (path  (puri:uri-path  parsed-uri))
              (query (puri:uri-query parsed-uri))
              (port  (or (puri:uri-port  parsed-uri)
                         gemini-client:+gemini-default-port+)))
          (handler-case
              (progn
                (multiple-value-bind (status x meta body gemini-text gemini-links)
                    (gemini-client:request host
                                           path
                                           :query query
                                           :port  port)
                  (declare (ignore x))
                  (add-url-to-history specials:*message-window* url)
                  (cond
                    ((gemini-client:response-redirect-p status)
                     (flet ((on-input-complete (maybe-accepted)
                              (when (ui::boolean-input-accepted-p maybe-accepted)
                                (let ((new-url (gemini-parser:absolutize-link meta
                                                                              (puri:uri-host parsed-uri)
                                                                              (puri:uri-port parsed-uri)
                                                                              (puri:uri-path parsed-uri))))
                                  (db-utils:with-ready-database (:connect nil)
                                    (request new-url))))))
                       (ui:ask-string-input #'on-input-complete
                                            :priority program-events:+minimum-event-priority+
                                            :prompt
                                            (format nil
                                                    (_ "Redirects to ~s, follows redirect? [y/N] ")
                                                    meta))))
                    ((gemini-client:response-input-p status)
                     (flet ((on-input-complete (input)
                              (when (string-not-empty-p input)
                                (db-utils:with-ready-database (:connect nil)
                                  (request (gemini-parser:make-gemini-uri host
                                                                          path
                                                                          input
                                                                          port))))))
                       (ui:ask-string-input #'on-input-complete
                                            :prompt
                                            (format nil
                                                    (_ "Server ~s asks: ~s ")
                                                    host
                                                    meta))))
                    ((gemini-client:response-sensitive-input-p status)
                     (error 'conditions:not-implemented-error
                            :text "Sensitive input not implemented"))
                    (gemini-text
                     (setf (message-window:source-text *message-window*)
                           gemini-text)
                     (setf (gemini-metadata-links (message-window:metadata *message-window*))
                           gemini-links)
                     (setf (keybindings *message-window*)
                           keybindings:*gemini-message-keymap*)
                     (draw *message-window*))
                    (t
                     (fs:with-anaphoric-temp-file (stream)
                       (write-sequence body stream)
                       (force-output stream)
                       (os-utils:xdg-open fs:temp-file))))))
            (gemini-client:gemini-tofu-error (e)
              (let ((host (gemini-client:host e)))
                (flet ((on-input-complete (maybe-accepted)
                         (when (ui::boolean-input-accepted-p maybe-accepted)
                           (db-utils:with-ready-database (:connect nil)
                             (db:tofu-delete host)
                             (request url)))))
                  (ui:ask-string-input #'on-input-complete
                                       :prompt
                                       (format nil
                                               (_ "Host ~s signature changed! This is a potential security risk! Ignore this warning? [y/N] ")
                                               host)))))
            (conditions:not-implemented-error (e)
              (ui:notify (format nil (_ "Error: ~a") e)
                         :as-error t))
            (gemini-client:gemini-protocol-error (e)
              (ui:notify (format nil "~a" e)
                         :as-error t))
            (error (e)
              (ui:notify (format nil
                                 (_ "Error getting ~s: ~a")
                                 url
                                 e)
                         :as-error t)))))))

(defun history-back (window)
  (when-let* ((metadata (message-window:metadata window))
              (history  (misc:safe-all-but-last-elt (gemini-metadata-history metadata)))
              (last     (last-elt history)))
    (setf (gemini-metadata-history metadata)
          (misc:all-but-last-elt history))
    (request last)))
