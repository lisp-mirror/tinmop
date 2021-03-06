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

(in-package :gemini-viewer)

(defparameter *gemini-db-streams-lock* (bt:make-recursive-lock))

(define-constant +read-buffer-size+ 1024
  :documentation "Chunk's size of the buffer when reading non gemini contents from stream")

(defparameter *gemini-streams-db* ())

(defun push-db-stream (stream-object)
  (pushnew stream-object
           *gemini-streams-db*
           :test (lambda (a b)
                   (string= (download-uri a)
                            (download-uri b))))
  *gemini-streams-db*)

(defun remove-db-stream (stream-object)
  (setf *gemini-streams-db*
        (remove stream-object *gemini-streams-db*))
  *gemini-streams-db*)

(defun remove-all-db-stream ()
  (map nil
       (lambda (a) (abort-downloading a))
       *gemini-streams-window*)
  (setf *gemini-streams-db* ())
  *gemini-streams-db*)

(defun find-db-stream-if (predicate)
  (find-if predicate *gemini-streams-db*))

(defun find-db-stream-url (url)
  (find-db-stream-if (lambda (a) (string= (download-uri a) url))))

(defun ensure-just-one-stream-rendering ()
  (with-lock (*gemini-db-streams-lock*)
    (when-let ((current-rendering (find-db-stream-if (lambda (a)
                                                       (eq (stream-status a)
                                                           :rendering)))))
      (setf (stream-status current-rendering) :streaming))))

(defun db-entry-to-foreground (uri)
  (when-let* ((stream-object (find-db-stream-url uri)))
    (with-accessors ((support-file support-file)
                     (meta         meta)) stream-object
      (if (gemini-client:mime-gemini-p meta)
          (progn
            (ensure-just-one-stream-rendering)
            (setf (stream-status stream-object) :rendering))
          (os-utils:xdg-open support-file)))))

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
   (stream-status
    :initform nil
    :initarg  :stream-status)
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
   (port
    :initform nil
    :initarg  :port
    :accessor port)
   (status-code
    :initform nil
    :initarg  :status-code
    :accessor status-code)
   (status-code-description
    :initform nil
    :initarg  :status-code-description
    :accessor status-code-description)
   (meta
    :initform nil
    :initarg  :meta
    :accessor meta)
   (path
    :initform nil
    :initarg  :path
    :accessor path)
   (host
    :initform nil
    :initarg  :host
    :accessor host)
   (thread
    :initform nil
    :initarg  :thread
    :accessor thread)))

(defmethod print-object ((object gemini-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
            "~a ~d ~a ~a"
            (download-uri  object)
            (octect-count  object)
            (meta           object)
            (stream-status  object))))

(defmethod to-tui-string ((object gemini-stream) &key (window nil))
  (flet ((pad (string width)
           (right-padding (ellipsize string width) width)))
    (let* ((window-width   (win-width window))
           (url-w          (truncate (* window-width 2/3)))
           (octect-count-w (truncate (* window-width 1/9)))
           (meta-w         (truncate (* window-width 1/9)))
           (status-w       (truncate (* window-width 1/9)))
           (color-re       (swconf:color-regexps))
           (fitted-line    (format nil
                                   "~a ~d ~a ~a"
                                   (pad (download-uri object) url-w)
                                   (pad (to-s (octect-count object))
                                        octect-count-w)
                                   (pad (meta object) meta-w)
                                   (ellipsize (string-downcase (format nil
                                                                       "~s"
                                                                       (stream-status object)))
                                              status-w))))
      (loop for re in color-re do
           (setf fitted-line (colorize-line fitted-line re)))
      (colorized-line->tui-string fitted-line))))

(defgeneric abort-downloading (object))

(defgeneric allow-downloading (object))

(defgeneric downloading-allowed-p (object))

(defgeneric downloading-start-thread (object function host port path query))

(defmethod abort-downloading ((object gemini-stream))
  (with-accessors ((download-thread-lock download-thread-lock)) object
    (setf (stream-status object) :aborted)
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

(defmethod (setf stream-status) (val (object gemini-stream))
  (with-accessors ((download-thread-lock download-thread-lock)
                   (stream-status        stream-status)) object
    (with-lock (download-thread-lock)
      (setf (slot-value object 'stream-status) val))))

(defmethod stream-status ((object gemini-stream))
  (with-accessors ((download-thread-lock download-thread-lock)) object
    (with-lock (download-thread-lock)
      (slot-value object 'stream-status))))

(defmethod downloading-start-thread ((object gemini-stream)
                                     function
                                     host port path query)
  (with-accessors ((start-time    start-time)
                   (thread        thread)
                   (stream-status stream-status)
                   (download-uri  download-uri)) object
    (setf thread
          (bt:make-thread function))
    (setf start-time (db-utils:local-time-obj-now))
    (setf download-uri (gemini-parser:make-gemini-uri host path query port))
    object))

(defclass gemini-file-stream (gemini-stream) ())

(defmethod (setf stream-status) :after ((val (eql :rendering)) (object gemini-file-stream))
  (with-accessors ((download-thread-lock download-thread-lock)
                   (support-file         support-file)) object
    (with-lock (download-thread-lock)
      (let ((event (make-gemini-download-event (fs:slurp-file support-file)
                                               object
                                               nil)))
        (program-events:push-event event)))))

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

(defun make-gemini-download-event (src-data stream-object append-text)
  (with-accessors ((download-uri            download-uri)
                   (host                    host)
                   (port                    port)
                   (path                    path)
                   (meta                    meta)
                   (status-code             status-code)
                   (status-code-description status-code-description)) stream-object
    (let* ((parsed   (gemini-parser:parse-gemini-file src-data))
           (links    (gemini-parser:sexp->links parsed host port path))
           (response (gemini-client:make-gemini-file-response status-code
                                                              status-code-description
                                                              meta
                                                              parsed
                                                              download-uri
                                                              src-data
                                                              links)))
      (make-instance 'program-events:gemini-got-line-event
                     :wrapper-object  stream-object
                     :payload         response
                     :append-text     append-text))))

(defun request-stream-gemini-document-thread (wrapper-object host
                                              port path query)
  (with-accessors ((download-socket download-socket)
                   (download-stream download-stream)
                   (octect-count    octect-count)
                   (support-file    support-file)) wrapper-object
    (flet ((maybe-render-line (line-event)
             (when (eq (stream-status wrapper-object) :rendering)
               (program-events:push-event line-event))))
      (lambda ()
        (with-open-support-file (file-stream support-file character)
          (let* ((url          (gemini-parser:make-gemini-uri host path query port))
                 (url-header   (format nil "-> ~a~%" url))
                 (parsed-url   (gemini-parser:parse-gemini-file url-header))
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
            (write-sequence url-header file-stream)
            (increment-bytes-count wrapper-object url-header :convert-to-octects t)
            (maybe-render-line url-event)
            (loop
               named download-loop
               for line-as-array = (read-line-into-array download-stream)
               while line-as-array do
                 (if (downloading-allowed-p wrapper-object)
                     (let* ((line   (babel:octets-to-string line-as-array :errorp nil))
                            (event  (make-gemini-download-event line wrapper-object t)))
                       (write-sequence line file-stream)
                       (increment-bytes-count wrapper-object line :convert-to-octects t)
                       (maybe-render-line event))
                     (progn
                       (return-from download-loop nil))))
            (if (not (downloading-allowed-p wrapper-object))
                (ui:notify (_ "Gemini document downloading aborted"))
                (let ((compact-event (make-instance 'program-events:gemini-compact-lines-event
                                                    :download-uri (download-uri wrapper-object)
                                                    :priority
                                                    program-events:+maximum-event-priority+)))
                  (program-events:push-event compact-event)
                  (ui:notify (_ "Gemini document downloading completed"))
                  (setf (stream-status wrapper-object) :completed)))
            ;; (allow-downloading wrapper-object)
            (gemini-client:close-ssl-socket download-socket)))))))
;;        (fs:delete-file-if-exists support-file)))))

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
                   (when (downloading-allowed-p wrapper-object)
                   (multiple-value-bind (buffer read-so-far)
                       (read-array download-stream +read-buffer-size+)
                     (increment-bytes-count wrapper-object read-so-far)
                     (if (< read-so-far (length buffer))
                         (progn
                           (write-sequence buffer file-stream :start 0 :end read-so-far)
                           (force-output file-stream)
                           (setf (stream-status wrapper-object) :completed)
                           (gemini-client:close-ssl-socket socket)
                           (os-utils:xdg-open support-file))
                         (progn
                           (write-sequence buffer file-stream)
                           (%fill-buffer)))))))
          (%fill-buffer))))))

(defun request (url &key
                      (enqueue                    nil)
                      (certificate                nil)
                      (certificate-key            nil)
                      (do-nothing-if-exists-in-db t))
  (let ((parsed-uri (ignore-errors (uri:uri-parse url))))
    (maybe-initialize-metadata specials:*message-window*)
    (if (null parsed-uri)
        (ui:error-message (format nil
                                  (_ "Could not understand the address ~s")
                                  url))
        (let* ((host       (uri:uri-host  parsed-uri))
               (path       (uri:uri-path  parsed-uri))
               (query      (uri:uri-query parsed-uri))
               (port       (or (uri:uri-port  parsed-uri)
                               gemini-client:+gemini-default-port+))
               (actual-uri (gemini-parser:make-gemini-uri host
                                                          path
                                                          query
                                                          port)))
          (when (not (and do-nothing-if-exists-in-db
                          (find-db-stream-url actual-uri)))
            (when (null enqueue)
              (ensure-just-one-stream-rendering))
            (handler-case
                (labels ((gemini-file-stream-p (meta)
                           (gemini-client:mime-gemini-p meta))
                         (starting-status (meta)
                           (if (gemini-file-stream-p meta)
                               (if enqueue
                                   :streaming
                                   :rendering)
                               (if enqueue
                                   :streaming
                                   :running)))
                         (fetch-cached-certificate (actual-uri)
                           (let* ((certificate-and-key
                                   (or (multiple-value-list
                                        (db:ssl-cert-find actual-uri))
                                       (multiple-value-list
                                        (gemini-client:make-client-certificate actual-uri))))
                                  (certificate (first  certificate-and-key))
                                  (key         (second certificate-and-key)))
                             (assert certificate)
                             (assert key)
                             (values certificate key)))
                         (get-user-input (hide-input host prompt)
                           (flet ((on-input-complete (input)
                                    (when (string-not-empty-p input)
                                      (db-utils:with-ready-database (:connect nil)
                                        (request (gemini-parser:make-gemini-uri host
                                                                                path
                                                                                input
                                                                                port)
                                                 :certificate-key    certificate-key
                                                 :certificate        certificate)))))
                             (ui:ask-string-input #'on-input-complete
                                                  :hide-input hide-input
                                                  :prompt (format nil
                                                                  (_ "Server ~s asks: ~s ")
                                                                  host
                                                                  prompt)))))
                  (multiple-value-bind (status code-description meta response socket)
                      (gemini-client:request host
                                             path
                                             :certificate-key    certificate-key
                                             :client-certificate certificate
                                             :query              query
                                             :port               port)
                    (add-url-to-history specials:*message-window* actual-uri)
                    (cond
                      ((gemini-client:response-redirect-p status)
                       (flet ((on-input-complete (maybe-accepted)
                                (when (ui::boolean-input-accepted-p maybe-accepted)
                                  (let ((new-url (gemini-parser:absolutize-link meta
                                                                                (uri:uri-host parsed-uri)
                                                                                (uri:uri-port parsed-uri)
                                                                                (uri:uri-path parsed-uri))))
                                    (db-utils:with-ready-database (:connect nil)
                                      (request new-url
                                               :certificate-key certificate-key
                                               :certificate     certificate))))))
                         (ui:ask-string-input #'on-input-complete
                                              :priority program-events:+minimum-event-priority+
                                              :prompt
                                              (format nil
                                                      (_ "Redirects to ~s, follows redirect? [y/N] ")
                                                      meta))))
                      ((gemini-client:response-certificate-requested-p status)
                       (multiple-value-bind (cached-certificate cached-key)
                           (fetch-cached-certificate actual-uri)
                         (request actual-uri
                                  :enqueue                    enqueue
                                  :do-nothing-if-exists-in-db do-nothing-if-exists-in-db
                                  :certificate-key            cached-key
                                  :certificate                cached-certificate)))
                      ((gemini-client:response-input-p status)
                       (get-user-input nil host meta))
                      ((gemini-client:response-sensitive-input-p status)
                       (get-user-input t host meta))
                      ((streamp response)
                       (if (gemini-file-stream-p meta)
                           (let* ((starting-status (starting-status meta))
                                  (gemini-stream   (make-instance 'gemini-file-stream
                                                                  :host            host
                                                                  :port            port
                                                                  :path            path
                                                                  :meta            meta
                                                                  :status-code     status
                                                                  :status-code-description
                                                                  code-description
                                                                  :stream-status   starting-status
                                                                  :download-stream response
                                                                  :download-socket socket))
                                  (thread-fn
                                   (request-stream-gemini-document-thread gemini-stream
                                                                          host
                                                                          port
                                                                          path
                                                                          query))
                                  (enqueue-event (make-instance 'program-events:gemini-enqueue-download-event
                                                                :payload gemini-stream)))
                             (program-events:push-event enqueue-event)
                             (downloading-start-thread gemini-stream
                                                       thread-fn
                                                       host
                                                       port
                                                       path
                                                       query))
                           (let* ((starting-status (starting-status meta))
                                  (gemini-stream   (make-instance 'gemini-others-data-stream
                                                                  :stream-status   starting-status
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
                                                                         meta))
                                  (enqueue-event (make-instance 'program-events:gemini-enqueue-download-event
                                                                :payload gemini-stream)))
                             (program-events:push-event enqueue-event)
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
                               (request url
                                        :enqueue         enqueue
                                        :certificate     certificate
                                        :certificate-key certificate-key
                                        :do-nothing-if-exists-in-db
                                        do-nothing-if-exists-in-db)))))
                    (ui:ask-string-input #'on-input-complete
                                         :prompt
                                         (format nil
                                                 (_ "Host ~s signature changed! This is a potential security risk! Ignore this warning? [y/N] ")
                                                 host)
                                         :priority program-events:+standard-event-priority+))))
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
                           :as-error t))))))))

(defun history-back (window)
  (when-let* ((metadata (message-window:metadata window))
              (history  (misc:safe-all-but-last-elt (gemini-metadata-history metadata)))
              (last     (last-elt history)))
    (setf (gemini-metadata-history metadata)
          history)
    (ui:info-message (format nil (_ "Going back to: ~a") last))
    (let ((found (find-db-stream-url last)))
      (if found
          (gemini-viewer:db-entry-to-foreground last)
          (request last))))) ; this should never happens

(defun view-source (window)
  (when-let* ((metadata (message-window:metadata window))
              (source   (gemini-metadata-source-file metadata))
              (last     (misc:safe-last-elt (gemini-metadata-history metadata))))
    (setf (message-window:source-text window) source)
    (draw window)
    (ui:info-message (format nil (_ "Viewing source of: ~a") last))))

(defclass gemini-streams-window (focus-marked-window
                                 simple-line-navigation-window
                                 title-window
                                 border-window)
  ())

(defmethod refresh-config :after ((object gemini-streams-window))
  (open-attach-window:refresh-view-links-window-config object
                                                       swconf:+key-open-gemini-stream-window+)
  (let* ((win-w (truncate (* (win-width  specials:*main-window*) 3/4)))
         (win-h (truncate (* (win-height specials:*main-window*) 3/4)))
         (x           (truncate (- (/ (win-width specials:*main-window*) 2)
                                   (/ win-w 2))))
         (y           (truncate (- (/ (win-height specials:*main-window*) 2)
                                   (/ win-h 2)))))
    (win-resize object win-w win-h)
    (win-move object x y)
    object))

(defmethod resync-rows-db ((object gemini-streams-window)
                           &key
                             (redraw t)
                             (suggested-message-index nil))
  (with-accessors ((rows             rows)
                   (selected-line-bg selected-line-bg)
                   (selected-line-fg selected-line-fg)) object
    (flet ((make-rows (streams bg fg)
             (mapcar (lambda (stream-object)
                       (let ((unselected-line (to-tui-string stream-object :window object)))
                         (make-instance 'line
                                        :normal-text   unselected-line
                                        :selected-text (tui-string->chars-string unselected-line)
                                        :fields        stream-object
                                        :normal-bg     bg
                                        :normal-fg     fg
                                        :selected-bg   fg
                                        :selected-fg   bg)))
                     streams)))
      (with-croatoan-window (croatoan-window object)
        (setf rows (make-rows *gemini-streams-db*
                              selected-line-bg
                              selected-line-fg))
        (when suggested-message-index
          (select-row object suggested-message-index))
        (when redraw
          (win-clear object)
          (draw object))))))

;; (defmethod draw :before ((object gemini-streams-window))
;;   (with-accessors ((rows              rows)
;;                    (uses-border-p     uses-border-p)
;;                    (single-row-height single-row-height)
;;                    (top-row-padding   top-row-padding)
;;                    (new-messages-mark new-messages-mark)
;;                    (top-rows-slice    top-rows-slice)
;;                    (bottom-rows-slice bottom-rows-slice)) object
;;     (let ((y-start (if uses-border-p
;;                        1
;;                        0)))
;;       (renderizable-rows-data object) ; set top and bottom slice
;;       (win-clear object)
;;       (with-croatoan-window (croatoan-window object)
;;         (loop
;;            for gemini-stream in (safe-subseq rows top-rows-slice bottom-rows-slice)
;;            for y from (+ y-start top-row-padding) by single-row-height do
;;              (print-text object
;;                          gemini-stream
;;                          1 y
;;                          :bgcolor (bgcolor croatoan-window)
;;                          :fgcolor (fgcolor croatoan-window)))))))

(defun open-gemini-stream-window ()
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf  *gemini-streams-window*
          (make-instance 'gemini-streams-window
                         :top-row-padding   0
                         :title             (_ "Current gemini streams")
                         :single-row-height 1
                         :uses-border-p     t
                         :keybindings       keybindings:*gemini-downloads-keymap*
                         :croatoan-window   low-level-window))
    (refresh-config  *gemini-streams-window*)
    (resync-rows-db  *gemini-streams-window* :redraw nil)
    (when (rows  *gemini-streams-window*)
      (select-row  *gemini-streams-window* 0))
    (draw  *gemini-streams-window*)
     *gemini-streams-window*))
