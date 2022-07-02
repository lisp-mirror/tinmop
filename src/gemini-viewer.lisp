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

(define-constant +read-buffer-size+ 2048 :test #'=
  :documentation "Chunk's size of the buffer when reading non gemini contents from stream")

(defparameter *gemini-streams-db* ())

(defun push-db-stream (stream-object)
  (pushnew stream-object
           *gemini-streams-db*
           :test (lambda (a b)
                   (string= (download-iri a)
                            (download-iri b))))
  *gemini-streams-db*)

(defun remove-db-stream (stream-object)
  (setf *gemini-streams-db*
        (remove stream-object *gemini-streams-db*))
  *gemini-streams-db*)

(defun remove-all-db-stream ()
  (map nil
       (lambda (a) (abort-downloading a))
       *gemini-streams-db*)
  (setf *gemini-streams-db* ())
  *gemini-streams-db*)

(defun find-db-stream-if (predicate)
  (find-if predicate *gemini-streams-db*))

(defun find-db-stream-url (url)
  (find-db-stream-if (lambda (a) (string= (download-iri a) url))))

(defun ensure-just-one-stream-rendering ()
  (with-lock (*gemini-db-streams-lock*)
    (when-let ((current-rendering (find-db-stream-if (lambda (a)
                                                       (eq (stream-status a)
                                                           :rendering)))))
      (setf (stream-status current-rendering) :streaming))))

(defun abort-download-stream (url &key
                                    (remove-wainting-stream-event t)
                                    (redraw-stream-window         t))
  (when-let ((stream-object (find-db-stream-url url)))
    (abort-downloading stream-object)
    (remove-db-stream stream-object)
    (when remove-wainting-stream-event
      (program-events:remove-event-if (lambda (a)
                                        (and (typep a
                                                    'program-events:gemini-got-line-event)
                                             (string= url
                                                      (download-iri stream-object))))))
    (when (and redraw-stream-window
               specials:*gemini-streams-window*)
      (line-oriented-window:resync-rows-db specials:*gemini-streams-window*))))

(defun bury-download-stream ()
  (let ((program-events:*process-events-immediately* t)
        (event (make-instance 'program-events:gemini-push-behind-downloading-event
                              :priority program-events:+maximum-event-priority+)))
    (program-events:push-event event)))

(defun force-rendering-of-cached-file (stream-object)
  ;; this is more than a mere setter
  ;; and is 'eql' specialized on rendering
  ;; it will force displaying of gemini cached file on the screen
  (setf (stream-status stream-object) :rendering))

(defun db-entry-to-foreground (iri)
  (when-let* ((stream-object (find-db-stream-url iri)))
    (with-accessors ((support-file support-file)
                     (meta         meta)) stream-object
      (cond
        ((gemini-client:mime-gemini-p meta)
         (ensure-just-one-stream-rendering)
         (force-rendering-of-cached-file stream-object)
         (setf (stream-status stream-object) :completed)
         (ui:open-gemini-toc)
         (program-events:with-enqueued-process ()
           (ui:open-gemini-message-link-window :give-focus nil)))
        ((gemini-client:text-file-stream-p meta)
          (ensure-just-one-stream-rendering)
         (force-rendering-of-cached-file stream-object)
         (setf (stream-status stream-object) :completed))
        (t
         (os-utils:open-resource-with-external-program support-file t))))))

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
   (download-iri
    :initform nil
    :initarg  :download-iri
    :accessor download-iri)
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
   (query
    :initform nil
    :initarg  :query
    :accessor query)
   (fragment
    :initform nil
    :initarg  :fragment
    :accessor fragment)
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
            (download-iri  object)
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
                                   (pad (download-iri object) url-w)
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

(defgeneric downloading-start-thread (object function host port path query fragment))

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
                                     host
                                     port
                                     path
                                     query
                                     fragment)
  (with-accessors ((start-time    start-time)
                   (thread        thread)
                   (stream-status stream-status)
                   (download-iri  download-iri)) object
    (setf thread (bt:make-thread function))
    (setf start-time (db-utils:local-time-obj-now))
    (setf download-iri (gemini-parser:make-gemini-iri host
                                                      path
                                                      :query    query
                                                      :port     port
                                                      :fragment fragment))
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
  `(handler-case
       (with-open-file (,stream ,file
                                :element-type      ',element-type
                                :direction         :output
                                :element-type      'character
                                :if-exists         :supersede
                                :if-does-not-exist :create)
         ,@body)
     (file-error (condition)
       (declare (ignore condition))
       nil)))

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
  (with-accessors ((download-iri            download-iri)
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
                                                              download-iri
                                                              src-data
                                                              links)))
      (make-instance 'program-events:gemini-got-line-event
                     :wrapper-object  stream-object
                     :payload         response
                     :append-text     append-text))))

(let ((cache ()))
  (defun fetch-favicon (parsed-url)
    (if (not (swconf:gemini-fetch-favicon-p))
        (swconf:gemini-default-favicon)
        (flet ((fetch-from-cache (key)
                 (assoc-value cache key :test #'string=)))
          (multiple-value-bind (actual-iri host path query port fragment)
              (gemini-client:displace-iri parsed-url)
            (declare (ignore actual-iri path query fragment))
            (or (fetch-from-cache host)
                (ignore-errors
                 (let* ((favicon-url   (gemini-parser:make-gemini-iri host
                                                                      "/favicon.txt"
                                                                      :port port))
                        (response-body (gemini-client:slurp-gemini-url favicon-url))
                        (favicon-list  (coerce (text-utils:to-s response-body :errorp t)
                                               'list))
                        (emoji         (starting-emoji favicon-list))
                        (favicon       (if emoji
                                           (coerce emoji 'string)
                                           (swconf:gemini-default-favicon))))
                   (setf cache (acons host favicon cache))
                   (fetch-favicon parsed-url)))
                (swconf:gemini-default-favicon)))))))

(defun request-stream-gemini-document-thread (wrapper-object host
                                              port path query fragment favicon
                                              gemini-format-p)
  (with-accessors ((download-socket download-socket)
                   (download-stream download-stream)
                   (octect-count    octect-count)
                   (support-file    support-file)) wrapper-object
    (labels ((maybe-render-line (line-event)
               (when (eq (stream-status wrapper-object) :rendering)
                 (program-events:push-event line-event)))
             (maybe-change-title (title-event)
               (when (eq (stream-status wrapper-object) :rendering)
                 (program-events:push-event title-event)))
             (maybe-render-toc ()
               (when (eq (stream-status wrapper-object) :rendering)
                 (ui:open-gemini-toc)))
             (maybe-render-focus-mark ()
               (when (eq (stream-status wrapper-object) :rendering)
                 (program-events:with-enqueued-process ()
                   (windows:draw-focus-mark *message-window*))))
             (maybe-render-links ()
               (when (eq (stream-status wrapper-object) :rendering)
                 (program-events:with-enqueued-process ()
                   (ui:open-gemini-message-link-window :give-focus nil))))
             (maybe-render-preformat-wrapper (file-stream wrapper-object)
               (when (not gemini-format-p)
                 (let* ((preformat-line (format nil "~a~%" gemini-parser:+preformatted-prefix+))
                        (preformat-wrapper-event (make-gemini-download-event preformat-line
                                                                             wrapper-object
                                                                             t)))
                   (maybe-render-line preformat-wrapper-event)
                   (write-sequence preformat-line file-stream))))
             (array->string (array remove-bom)
               (let ((res (text-utils:to-s array :errorp nil)))
                 (if (and (string-not-empty-p res)
                          remove-bom
                          (char= (first-elt res)
                                 #\ZERO_WIDTH_NO-BREAK_SPACE))
                     (subseq res 1)
                     res))))
      (lambda ()
        (gemini-parser:with-initialized-parser
          (when-let ((extension (fs:get-extension path)))
            (setf support-file (fs:temporary-file :extension extension)))
          (with-open-support-file (file-stream support-file character)
            (let* ((url                    (gemini-parser:make-gemini-iri host
                                                                          path
                                                                          :query    query
                                                                          :port     port
                                                                          :fragment fragment))
                   (url-header             (format nil "~a ~a~2%" favicon url))
                   (parsed-url             (gemini-parser:parse-gemini-file url-header))
                   (url-response           (gemini-client:make-gemini-file-response nil
                                                                                    nil
                                                                                    nil
                                                                                    parsed-url
                                                                                    nil
                                                                                    ""
                                                                                    nil))
                   (url-event               (make-instance 'program-events:gemini-got-line-event
                                                           :wrapper-object  wrapper-object
                                                           :payload         url-response
                                                           :append-text     nil))
                   (new-title-event         (make-instance 'program-events:change-window-title-event
                                                           :payload url-header
                                                           :window *message-window*)))
              (write-sequence url-header file-stream)
              (increment-bytes-count wrapper-object url-header :convert-to-octects t)
              (maybe-change-title new-title-event)
              (maybe-render-line url-event)
              (maybe-render-preformat-wrapper file-stream wrapper-object)
              (loop
                named download-loop
                for ct from 0
                for line-as-array = (read-line-into-array download-stream)
                while line-as-array do
                  (gemini-client:debug-gemini "[stream] gemini file stream raw data line : ~a"
                                              line-as-array)
                  (if (downloading-allowed-p wrapper-object)
                      (let* ((line  (if (= ct 0)
                                        (array->string line-as-array t)
                                        (array->string line-as-array nil)))
                             (event (make-gemini-download-event line
                                                                wrapper-object
                                                                t)))
                        (gemini-client:debug-gemini "[stream] gemini file stream got data line : ~a"
                                                    line)
                        (write-sequence line file-stream)
                        (increment-bytes-count wrapper-object line :convert-to-octects t)
                        (maybe-render-line event))
                      (progn
                        (return-from download-loop nil))))
              (maybe-render-preformat-wrapper file-stream wrapper-object)
              (if (not (downloading-allowed-p wrapper-object))
                  (ui:notify (_ "Gemini document downloading aborted"))
                  (progn
                    (maybe-render-toc)
                    (maybe-render-links)
                    (maybe-render-focus-mark)
                    (ui:notify (_ "Gemini document downloading completed"))
                    (setf (stream-status wrapper-object) :completed)
                    (when (and fragment
                               (swconf:config-gemini-fragment-as-regex-p))
                      (let* ((regex    (if (text-utils:percent-encoded-p fragment)
                                           (text-utils:percent-decode    fragment)
                                           fragment))
                             (priority program-events:+standard-event-priority+)
                             (event    (make-instance 'program-events:search-message-gemini-fragment-event
                                                      :priority priority
                                                      :payload  regex)))
                        (program-events:push-event event)))))
              ;; (allow-downloading wrapper-object)
              (gemini-client:close-ssl-socket download-socket))))))))
;;        (fs:delete-file-if-exists support-file)))))

(defun request-stream-other-document-thread (wrapper-object
                                             socket
                                             host
                                             port
                                             path
                                             query
                                             fragment
                                             status-code
                                             status-code-description
                                             meta)
  (declare (ignorable host
                      port path query fragment
                      status-code status-code-description meta))
  (with-accessors ((download-socket download-socket)
                   (download-stream download-stream)
                   (octect-count    octect-count)
                   (support-file    support-file)) wrapper-object

    (lambda ()
      (when-let ((extension (fs:get-extension path)))
        (setf support-file (fs:temporary-file :extension extension)))
      (with-open-support-file (file-stream support-file)
        (let ((partial-content-not-opened t))
        (labels ((download-completed-p (buffer read-so-far)
                   (< read-so-far (length buffer)))
                 (opening-partial-contents-p (read-so-far)
                   (let ((buffer-size (swconf:link-regex->program-to-use-buffer-size path)))
                     (if buffer-size
                         (> read-so-far buffer-size)
                         (> read-so-far swconf:+buffer-minimum-size-to-open+))))
                 (%fill-buffer ()
                   (declare (optimize (debug 0) (speed 3)))
                   (when (downloading-allowed-p wrapper-object)
                     (multiple-value-bind (program-exists y wait-for-download)
                         (swconf:link-regex->program-to-use support-file)
                       (declare (ignore y))
                       (multiple-value-bind (buffer read-so-far)
                           (read-array download-stream +read-buffer-size+)
                         (declare ((vector (unsigned-byte 8)) buffer))
                         (declare (fixnum read-so-far))
                         (increment-bytes-count wrapper-object read-so-far)
                         (if (download-completed-p buffer read-so-far)
                             (progn
                               (write-sequence buffer file-stream :start 0 :end read-so-far)
                               (force-output file-stream)
                               (setf (stream-status wrapper-object) :completed)
                               (gemini-client:close-ssl-socket socket)
                               (when (or wait-for-download
                                         partial-content-not-opened)
                                 (os-utils:open-resource-with-external-program support-file
                                                                               nil)))
                             (progn
                               (write-sequence buffer file-stream)
                               (when (and partial-content-not-opened
                                          program-exists
                                          (not wait-for-download)
                                          (opening-partial-contents-p (octect-count wrapper-object)))
                                 (setf partial-content-not-opened nil)
                                 (os-utils:open-resource-with-external-program support-file
                                                                               nil))
                               (%fill-buffer))))))))
          (%fill-buffer)))))))

(defun request-success-dispatched-clrs (enqueue)
  (lambda (status code-description meta response socket iri parsed-iri)
    (declare (ignore iri))
    (labels ((starting-status (meta)
               (if (or (gemini-client:gemini-file-stream-p meta)
                       (gemini-client:text-file-stream-p   meta))
                   (if enqueue
                       :streaming
                       :rendering)
                   (if enqueue
                       :streaming
                       :running))))
      (multiple-value-bind (actual-iri host path query port fragment)
          (gemini-client:displace-iri parsed-iri)
        (declare (ignore actual-iri))
        (gemini-client:debug-gemini "response is a stream")
        (labels ((make-text-based-stream (gemini-format-p)
                   (let* ((starting-status (starting-status meta))
                          (gemini-stream   (make-instance 'gemini-file-stream
                                                          :host            host
                                                          :port            port
                                                          :path            path
                                                          :query           query
                                                          :fragment        fragment
                                                          :meta            meta
                                                          :status-code     status
                                                          :status-code-description
                                                          code-description
                                                          :stream-status   starting-status
                                                          :download-stream response
                                                          :download-socket socket))
                          (favicon      (fetch-favicon parsed-iri))
                          (thread-fn    (request-stream-gemini-document-thread gemini-stream
                                                                               host
                                                                               port
                                                                               path
                                                                               query
                                                                               fragment
                                                                               favicon
                                                                               gemini-format-p))
                          (enqueue-event (make-instance 'program-events:gemini-enqueue-download-event
                                                        :payload gemini-stream)))
                     (program-events:push-event enqueue-event)
                     (downloading-start-thread gemini-stream
                                               thread-fn
                                               host
                                               port
                                               path
                                               query
                                               fragment))))
          (cond
            ((gemini-client:gemini-file-stream-p meta)
             (gemini-client:debug-gemini "response is a gemini document stream")
             (make-text-based-stream t))
            ((gemini-client:text-file-stream-p meta)
             (gemini-client:debug-gemini "response is a text stream")
             (make-text-based-stream nil))
            (t
             (let* ((starting-status (starting-status meta))
                    (gemini-stream   (make-instance 'gemini-others-data-stream
                                                    :stream-status   starting-status
                                                    :download-stream response
                                                    :download-socket socket))
                    (thread-fn       (request-stream-other-document-thread gemini-stream
                                                                           socket
                                                                           host
                                                                           port
                                                                           path
                                                                           query
                                                                           fragment
                                                                           status
                                                                           code-description
                                                                           meta))
                    (enqueue-event (make-instance 'program-events:gemini-enqueue-download-event
                                                  :payload gemini-stream)))
               (gemini-client:debug-gemini "response is *not* a gemini file stream")
               (program-events:push-event enqueue-event)
               (downloading-start-thread gemini-stream
                                         thread-fn
                                         host
                                         port
                                         path
                                         query
                                         fragment)))))))))

(defun request (url &key
                      (enqueue                    nil)
                      (certificate                nil)
                      (certificate-key            nil)
                      (use-cached-file-if-exists  nil)
                      (do-nothing-if-exists-in-db nil))
  (labels ((get-user-input (hide-input url prompt)
             (multiple-value-bind (actual-iri host path query port fragment)
                 (gemini-client:displace-iri (iri:iri-parse url))
               (declare (ignore actual-iri query fragment))
               (flet ((on-input-complete (input)
                        (when (string-not-empty-p input)
                          (db-utils:with-ready-database (:connect nil)
                            (let ((encoded-input (maybe-percent-encode input)))
                              (request (gemini-parser:make-gemini-iri host
                                                                      path
                                                                      :query
                                                                      encoded-input
                                                                      :port     port)
                                       :certificate-key    certificate-key
                                       :certificate        certificate
                                       :do-nothing-if-exists-in-db nil))))))
                 (ui:ask-string-input #'on-input-complete
                                      :priority
                                      program-events:+minimum-event-priority+
                                      :hide-input hide-input
                                      :prompt (format nil
                                                      (_ "Server ~s asks: ~s ")
                                                      host
                                                      prompt)))))
           (redirect-dispatch (status code-description meta response socket iri parsed-iri)
             (declare (ignore status code-description response socket iri))
             (gemini-client:debug-gemini "response redirect to: ~s" meta)
             (flet ((on-input-complete (maybe-accepted)
                      (when (ui::boolean-input-accepted-p maybe-accepted)
                        (pop-url-from-history specials:*message-window*)
                        (when-let ((new-url (gemini-client:build-redirect-iri meta
                                                                              parsed-iri)))
                          (db-utils:with-ready-database (:connect nil)
                            (request new-url
                                     :enqueue         enqueue
                                     :certificate-key certificate-key
                                     :certificate     certificate))))))
               (ui:ask-string-input #'on-input-complete
                                    :priority program-events:+minimum-event-priority+
                                    :prompt
                                    (format nil
                                            (_ "Redirects to ~s, follows redirect? [y/N] ")
                                            meta))))
           (input-dispatch (status code-description meta response socket iri parsed-iri)
             (declare (ignore status code-description response socket parsed-iri))
             (gemini-client:debug-gemini "response requested input: ~s" meta)
             (get-user-input nil iri meta))
           (sensitive-input-dispatch (status code-description meta response socket iri parsed-iri)
             (declare (ignore status code-description response socket parsed-iri))
             (gemini-client:debug-gemini "response requested sensitive input: ~s"
                                         meta)
             (get-user-input t iri meta))
           (certificate-request-dispatch (status
                                          code-description
                                          meta
                                          response
                                          socket iri
                                          parsed-iri)
             (declare (ignore status code-description response socket meta parsed-iri))
             (gemini-client:debug-gemini "response requested certificate")
             (multiple-value-bind (cached-certificate cached-key)
                 (gemini-client:fetch-cached-certificate iri)
               (request iri
                        :enqueue                    enqueue
                        :do-nothing-if-exists-in-db do-nothing-if-exists-in-db
                        :certificate-key            cached-key
                        :certificate                cached-certificate))))
    (handler-case
        (gemini-client:with-request-dispatch-table ((:certificate-requested
                                                     #'certificate-request-dispatch
                                                     :input-requested
                                                     #'input-dispatch
                                                     :sensitive-input-requested
                                                     #'sensitive-input-dispatch
                                                     :redirect
                                                     #'redirect-dispatch
                                                     :success
                                                     (request-success-dispatched-clrs enqueue))
                                                     :ignore-warning nil)
          (gemini-client:debug-gemini "viewer requesting iri ~s" url)
          (maybe-initialize-metadata specials:*message-window*)
          (let ((actual-iri (gemini-client:displace-iri (iri:iri-parse url))))
            (if use-cached-file-if-exists
                (progn
                  (gemini-client:debug-gemini "checking cache")
                  (if (find-db-stream-url actual-iri)
                      (progn
                        (gemini-client:debug-gemini  "caching found for ~a" actual-iri)
                        (push-url-to-history specials:*message-window* actual-iri)
                        (db-entry-to-foreground actual-iri))
                      (progn
                        (gemini-client:debug-gemini "caching *not* found for ~a" actual-iri)
                        (request actual-iri
                                 :enqueue            enqueue
                                 :certificate-key    certificate-key
                                 :certificate        certificate
                                 :use-cached-file-if-exists nil
                                 :do-nothing-if-exists-in-db
                                 do-nothing-if-exists-in-db))))
                (when (not (and do-nothing-if-exists-in-db
                                (find-db-stream-url actual-iri)))
                  (when (null enqueue)
                    (ensure-just-one-stream-rendering))
                  (push-url-to-history specials:*message-window* actual-iri)
                  (gemini-client:request-dispatch url
                                                  gemini-client::dispatch-table
                                                  :certificate     certificate
                                                  :certificate-key certificate-key)))))
      (gemini-client:gemini-tofu-error (e)
        (ui:ask-input-on-tofu-error e
                                    (lambda ()
                                      (request url
                                               :enqueue         enqueue
                                               :certificate     certificate
                                               :certificate-key certificate-key
                                               :do-nothing-if-exists-in-db
                                               do-nothing-if-exists-in-db))))
      (conditions:not-implemented-error (e)
        (ui:notify (format nil (_ "Error: ~a") e)
                   :as-error t))
      (gemini-client:gemini-protocol-error (e)
        (ui:notify (format nil "~a" e)
                   :as-error t))
      #-debug-mode
      (error (e)
        (ui:notify (format nil (_ "Error getting ~s: ~a") url e)
                   :as-error t))
      #-debug-mode
      (condition (c)
        (ui:notify (format nil (_ "Error getting ~s: ~a") url c)
                   :as-error t)))))

(defun history-back (window)
  (when-let* ((metadata (message-window:metadata window))
              (history  (misc:safe-all-but-last-elt (gemini-metadata-history metadata)))
              (last     (last-elt history)))
    (setf (gemini-metadata-history metadata)
          history)
    (ui:info-message (format nil (_ "Going back to: ~a") last))
    (let ((found (find-db-stream-url last)))
      (if found
          (db-entry-to-foreground last)
          (load-gemini-url last))))) ; this happens if navigating in a local tree

(defun view-source (window)
  (when-let* ((metadata (message-window:metadata window))
              (source   (gemini-metadata-source-file metadata))
              (last     (misc:safe-last-elt (gemini-metadata-history metadata))))
    (message-window:prepare-for-rendering window source)
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
  (let* ((win-w       (truncate (* (win-width  specials:*main-window*) 3/4)))
         (win-h       (truncate (* (win-height specials:*main-window*) 3/4)))
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
        (line-oriented-window:update-all-rows object
                                              (make-rows *gemini-streams-db*
                                                         selected-line-bg
                                                         selected-line-fg))
        (when suggested-message-index
          (select-row object suggested-message-index))
        (when redraw
          (win-clear object)
          (draw object))))))

(defun open-gemini-stream-window ()
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *gemini-streams-window*
          (make-instance 'gemini-streams-window
                         :top-row-padding   0
                         :title             (_ "Current gemini streams")
                         :single-row-height 1
                         :uses-border-p     t
                         :keybindings       keybindings:*gemini-downloads-keymap*
                         :croatoan-window   low-level-window))
    (refresh-config  *gemini-streams-window*)
    (resync-rows-db  *gemini-streams-window* :redraw nil)
    (when (not (line-oriented-window:rows-empty-p *gemini-streams-window*))
      (select-row  *gemini-streams-window* 0))
    (draw  *gemini-streams-window*)
     *gemini-streams-window*))

(defun load-gemini-url (url &key
                              (priority program-events:+standard-event-priority+)
                              (give-focus-to-message-window t)
                              (use-cached-file-if-exists nil)
                              (enqueue                   nil))
  "Load `url', that  is a web resource or a  local file. This function
can be  used only when  the event polling  is enabled (e.g.  from user
command) otherwise  te actual code to  get the resource will  never be
executed."
  (let* ((event (make-instance 'program-events:gemini-request-event
                               :give-focus-to-message-window give-focus-to-message-window
                               :priority                     priority
                               :use-cached-file-if-exists    use-cached-file-if-exists
                               :enqueue                      enqueue
                               :url                          url)))
    (program-events:push-event event)))
