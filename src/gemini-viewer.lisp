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
  (history)
  (source-file))

(defgeneric append-metadata-link (object link))

(defgeneric append-metadata-source (object source-text))

(defmethod append-metadata-link ((object gemini-metadata) link)
  (setf (gemini-metadata-links object)
        (append (gemini-metadata-links object)
                link)))

(defmethod append-metadata-source ((object gemini-metadata) source-file)
  (setf (gemini-metadata-source-file object)
        (strcat (gemini-metadata-source-file object)
                source-file)))

(defun add-url-to-history (window url)
  (let* ((metadata   (message-window:metadata window))
         (history    (reverse (gemini-metadata-history metadata)))
         (last-entry (safe-last-elt (gemini-metadata-history metadata))))
    (when (string/= last-entry
                    url)
      (setf (gemini-metadata-history metadata)
            (reverse (push url history))))))

(defun maybe-initialize-metadata (window)
  (when (not (gemini-metadata-p (message-window:metadata window)))
    (setf (message-window:metadata window)
          (make-gemini-metadata)))
  (message-window:metadata window))


(defun request-stream-gemini-document-thread (socket stream host
                              port path query
                              status-code status-code-description meta)
  (lambda ()
    (let* ((url          (gemini-parser:make-gemini-uri host path query))
           (parsed-url   (gemini-parser:parse-gemini-file (format nil "-> ~a~%" url)))
           (url-response (gemini-client:make-gemini-file-response nil
                                                                  nil
                                                                  nil
                                                                  parsed-url
                                                                  nil
                                                                  ""
                                                                  nil))
           (url-event    (make-instance 'program-events:gemini-got-line-event
                                        :payload     url-response
                                        :append-text nil)))
      (program-events:push-event url-event)
      (loop
         for line-as-array = (read-line-into-array stream)
         while line-as-array do
           (let* ((line     (babel:octets-to-string line-as-array :errorp nil))
                  (parsed   (gemini-parser:parse-gemini-file line))
                  (links    (gemini-parser:sexp->links parsed host port path))
                  (response (gemini-client:make-gemini-file-response status-code
                                                                     status-code-description
                                                                     meta
                                                                     parsed
                                                                     url
                                                                     line
                                                                     links))
                  (event    (make-instance 'program-events:gemini-got-line-event
                                           :payload response)))
             (program-events:push-event event)))
      (ui:notify (_ "Gemini document downloading completed"))
      (gemini-client:close-ssl-socket socket))))

(defun request-stream-other-document-thread (socket stream host
                                             port path query
                                             status-code status-code-description meta)
  (declare (ignorable host
                      port path query
                      status-code status-code-description meta))
  (lambda ()
    (fs:with-anaphoric-temp-file (out-stream)
      (let* ((buffer (misc:read-all stream)))
        (gemini-client:close-ssl-socket socket)
        (write-sequence buffer out-stream)
        (force-output out-stream)
        (os-utils:xdg-open fs:temp-file)))))

(defun request (url)
  (let ((parsed-uri (quri:uri url)))
    (maybe-initialize-metadata specials:*message-window*)
    (if (null parsed-uri)
        (ui:error-message (format nil
                                  (_ "Could not understand the address ~s")
                                  url))
        (let ((host  (quri:uri-host  parsed-uri))
              (path  (quri:uri-path  parsed-uri))
              (query (quri:uri-query parsed-uri))
              (port  (or (quri:uri-port  parsed-uri)
                         gemini-client:+gemini-default-port+)))
          (handler-case
              (progn
                (multiple-value-bind (status code-description meta response socket)
                    (gemini-client:request host
                                           path
                                           :query query
                                           :port  port)
                  (add-url-to-history specials:*message-window* url)
                  (cond
                    ((gemini-client:response-redirect-p status)
                     (flet ((on-input-complete (maybe-accepted)
                              (when (ui::boolean-input-accepted-p maybe-accepted)
                                (let ((new-url (gemini-parser:absolutize-link meta
                                                                              (quri:uri-host parsed-uri)
                                                                              (quri:uri-port parsed-uri)
                                                                              (quri:uri-path parsed-uri))))
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
                    ((streamp response)
                     (let ((stream response))
                       (if (gemini-client:mime-gemini-p meta)
                           (bt:make-thread (request-stream-gemini-document-thread socket
                                                                                  stream
                                                                                  host
                                                                                  port
                                                                                  path
                                                                                  query
                                                                                  status
                                                                                  code-description
                                                                                  meta))

                           (bt:make-thread (request-stream-other-document-thread socket
                                                                                 stream
                                                                                 host
                                                                                 port
                                                                                 path
                                                                                 query
                                                                                 status
                                                                                 code-description
                                                                                 meta))))))))
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
            #-debug-mode
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
          history)
    (ui:info-message (format nil (_ "Going back to: ~a") last))
    (request last)))

(defun view-source (window)
  (when-let* ((metadata (message-window:metadata window))
              (source   (gemini-metadata-source-file metadata))
              (last     (misc:safe-last-elt (gemini-metadata-history metadata))))
    (setf (message-window:source-text window) source)
    (draw window)
    (ui:info-message (format nil (_ "Viewing source of: ~a") last))))
