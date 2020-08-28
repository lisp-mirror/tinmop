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

(defparameter *download-thread-lock* (bt:make-recursive-lock "download-gemini"))

(defparameter *download-thread-blocked* nil)

(defclass gemini-stream ()
  ((download-thread-lock
    :initform (bt:make-recursive-lock "download-gemini")
    :initarg  :download-thread-lock
    :accessor download-thread-lock)
   (download-thread-blocked
    :initform nil
    :initarg  :download-thread-blocked
    :reader   download-thread-blocked-p
    :writer   (setf download-thread-blocked))
   (download-uri
    :initform nil
    :initarg  :download-uri
    :accessor download-uri)
   (start-time
    :initform (db-utils:local-time-obj-now)
    :initarg  :start-time
    :accessor start-time)
   (download-stream
    :initform nil
    :initarg  :download-stream
    :accessor download-stream)
   (download-socket
    :initform nil
    :initarg  :download-socket
    :accessor download-socket)
   (support-file
    :initform (fs:temporary-file)
    :initarg  :support-file
    :accessor support-file)
   (octect-count
    :initform 0
    :initarg  :octect-count
    :accessor octect-count)
   (thread
    :initform nil
    :initarg  :thread
    :accessor thread)))

(defgeneric abort-downloading (object))

(defgeneric allow-downloading (object))

(defgeneric downloading-allowed-p (object))

(defgeneric downloading-start-thread (object function host port path query))

(defmethod abort-downloading ((object gemini-stream))
  (with-accessors ((download-thread-lock download-thread-lock)) object
    (with-lock (download-thread-lock)
      (setf (download-thread-blocked object) t))))

(defmethod allow-downloading ((object gemini-stream))
  (with-accessors ((download-thread-lock download-thread-lock)) object
    (with-lock (download-thread-lock)
      (setf (download-thread-blocked object) nil))))

(defmethod downloading-allowed-p ((object gemini-stream))
  (with-accessors ((download-thread-lock download-thread-lock)) object
    (with-lock (download-thread-lock)
      (not (download-thread-blocked-p object)))))

(defmethod downloading-start-thread ((object gemini-stream)
                                     function
                                     host port path query)
  (with-accessors ((start-time   start-time)
                   (thread       thread)
                   (download-uri download-uri)) object
    (setf thread
          (bt:make-thread function))
    (setf start-time (db-utils:local-time-obj-now))
    (setf download-uri (gemini-parser:make-gemini-uri host path query port))
    object))

(defclass gemini-file-stream (gemini-stream) ())

(defclass gemini-others-data-stream (gemini-stream) ())

(defmacro with-open-support-file ((stream file &optional (element-type '(unsigned-byte 8)))
                                  &body body)
  `(with-open-file (,stream ,file
                            :element-type      ',element-type
                            :direction         :output
                            :element-type      'character
                            :if-exists         :supersede
                            :if-does-not-exist :create)
     ,@body))

(defgeneric increment-bytes-count (object data &key &allow-other-keys))

(defmethod increment-bytes-count ((object gemini-stream) data
                                  &key (convert-to-octects nil))
  (with-accessors ((octect-count octect-count)) object
    (if convert-to-octects
        (incf octect-count (babel:string-size-in-octets data
                                                        :errorp nil))
        (incf octect-count (length data)))))

(defmethod increment-bytes-count ((object gemini-stream) (data number)
                                  &key &allow-other-keys)
  (with-accessors ((octect-count octect-count)) object
    (incf octect-count data)))

(defun request-stream-gemini-document-thread (wrapper-object host
                                              port path query
                                              status-code status-code-description meta)
  (with-accessors ((download-socket download-socket)
                   (download-stream download-stream)
                   (octect-count    octect-count)
                   (support-file    support-file)) wrapper-object
    (lambda ()
      (with-open-support-file (file-stream support-file character)
        (let* ((url          (gemini-parser:make-gemini-uri host path query port))
               (parsed-url   (gemini-parser:parse-gemini-file (format nil "-> ~a~%" url)))
               (url-response (gemini-client:make-gemini-file-response nil
                                                                      nil
                                                                      nil
                                                                      parsed-url
                                                                      nil
                                                                      ""
                                                                      nil))
               (url-event    (make-instance 'program-events:gemini-got-line-event
                                            :wrapper-object  wrapper-object
                                            :payload         url-response
                                            :append-text     nil)))
          (program-events:push-event url-event)
          (loop
             named download-loop
             for line-as-array = (read-line-into-array download-stream)
             while line-as-array do
               (if (downloading-allowed-p wrapper-object)
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
                                                   :wrapper-object  wrapper-object
                                                   :payload         response)))
                     (write-sequence line file-stream)
                     (increment-bytes-count wrapper-object line :convert-to-octects t)
                     (program-events:push-event event))
                   (progn
                     (return-from download-loop nil))))
          (if (not (downloading-allowed-p wrapper-object))
              (ui:notify (_ "Gemini document downloading aborted"))
              (ui:notify (_ "Gemini document downloading completed")))
          (allow-downloading wrapper-object)
          (gemini-client:close-ssl-socket download-socket)))
      (fs:delete-file-if-exists support-file))))

(defun request-stream-other-document-thread (wrapper-object
                                             socket
                                             host
                                             port
                                             path
                                             query
                                             status-code status-code-description meta)
  (declare (ignorable host
                      port path query
                      status-code status-code-description meta))
  (with-accessors ((download-socket download-socket)
                   (download-stream download-stream)
                   (octect-count    octect-count)
                   (support-file    support-file)) wrapper-object

    (lambda ()
      (with-open-support-file (file-stream support-file)
        (labels ((%fill-buffer ()
                   (multiple-value-bind (buffer read-so-far)
                       (read-array download-stream 1024)
                     (increment-bytes-count wrapper-object read-so-far)
                     (if (< read-so-far (length buffer))
                         (progn
                           (write-sequence buffer file-stream :start 0 :end read-so-far)
                           (force-output file-stream)
                           (gemini-client:close-ssl-socket socket)
                           (os-utils:xdg-open support-file))
                         (progn
                           (write-sequence buffer file-stream)
                           (%fill-buffer))))))
          (%fill-buffer))))))

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
              (flet ((get-user-input (hide-input host prompt)
                       (flet ((on-input-complete (input)
                                (when (string-not-empty-p input)
                                  (db-utils:with-ready-database (:connect nil)
                                    (request (gemini-parser:make-gemini-uri host
                                                                            path
                                                                            input
                                                                            port))))))
                         (ui:ask-string-input #'on-input-complete
                                              :hide-input hide-input
                                              :prompt (format nil
                                                              (_ "Server ~s asks: ~s ")
                                                              host
                                                              prompt)))))
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
                     (get-user-input nil host meta))
                    ((gemini-client:response-sensitive-input-p status)
                     (get-user-input t host meta))
                    ((streamp response)
                     (if (gemini-client:mime-gemini-p meta)
                         (let* ((gemini-stream (make-instance 'gemini-file-stream
                                                              :download-stream response
                                                              :download-socket socket))
                                (thread-fn
                                 (request-stream-gemini-document-thread gemini-stream
                                                                        host
                                                                        port
                                                                        path
                                                                        query
                                                                        status
                                                                        code-description
                                                                        meta)))
                           (downloading-start-thread gemini-stream
                                                     thread-fn
                                                     host
                                                     port
                                                     path
                                                     query))
                         (let* ((gemini-stream (make-instance 'gemini-others-data-stream
                                                              :download-stream response
                                                              :download-socket socket))
                                (thread-fn
                                 (request-stream-other-document-thread gemini-stream
                                                                       socket
                                                                       host
                                                                       port
                                                                       path
                                                                       query
                                                                       status
                                                                       code-description
                                                                       meta)))
                           (downloading-start-thread gemini-stream
                                                     thread-fn
                                                     host
                                                     port
                                                     path
                                                     query)))))))
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
