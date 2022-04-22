(in-package :kami)

(a:define-constant +kami-scheme+     "np"               :test #'string=)

(a:define-constant +download-buffer+ (expt 2 24)        :test #'=)

(a:define-constant +octect-type+     '(unsigned-byte 8) :test #'equalp)

(defparameter *stream*   nil)

(defparameter *root-fid* nil)

(defstruct 9p-parameters
  (message-sent)
  (fid)
  (tag))

(defmacro with-open-ssl-stream ((ssl-stream socket host port
                                 client-certificate
                                 certificate-key)
                                &body body)
  (alexandria:with-gensyms (tls-context socket-stream ssl-hostname)
    `(let ((,tls-context (cl+ssl:make-context :verify-mode cl+ssl:+ssl-verify-none+)))
       (cl+ssl:with-global-context (,tls-context :auto-free-p t)
         (let* ((,socket        (gemini-client:open-tls-socket ,host ,port))
                (,socket-stream (usocket:socket-stream ,socket))
                (,ssl-hostname  ,host)
                (,ssl-stream
                  (cl+ssl:make-ssl-client-stream ,socket-stream
                                                 :certificate     ,client-certificate
                                                 :key             ,certificate-key
                                                 :external-format nil ; unsigned byte 8
                                                 :unwrap-stream-p t
                                                 :verify          nil
                                                 :hostname        ,ssl-hostname)))
           ,@body)))))

(defmacro with-cloned-root-fid ((stream cloned-fid &key (clunk-cloned-fid t)) &body body)
  `(let ((,cloned-fid (9p:clone-fid ,stream *root-fid*)))
     (prog1
         (progn ,@body)
       (when ,clunk-cloned-fid
         (9p:9p-clunk ,stream ,cloned-fid)))))

(defmacro with-9p-params ((params) &body body)
  `(let* ((9p:*tag*           (9p-parameters-tag          ,params))
          (9p:*fid*           (9p-parameters-fid          ,params))
          (9p:*messages-sent* (9p-parameters-message-sent ,params)))
     (unwind-protect
          (progn ,@body)
       (setf (9p-parameters-tag          ,params) 9p:*tag*)
       (setf (9p-parameters-fid          ,params) 9p:*fid*)
       (setf (9p-parameters-message-sent ,params) 9p:*messages-sent*))))

(defun expand-node (stream root-fid params)
  (lambda (node)
    (with-9p-params (params)
      (let* ((*stream*   stream)
             (*root-fid* root-fid)
             (path (tree-path (data node))))
        (with-cloned-root-fid  (*stream* cloned-root-fid)
          (let* ((entries     (9p:collect-directory-children *stream* cloned-root-fid path))
                 (files       (remove-if-not (lambda (a) (or (eq (9p:stat-entry-type a)
                                                                 :file)
                                                             (eq (9p:stat-entry-type a)
                                                                 :executable)))
                                             entries))
                 (directories (remove-if-not (lambda (a) (eq (9p:stat-entry-type a)
                                                             :directory))
                                             entries)))
            (remove-all-children node)
            (loop for directory in directories do
              (let ((absolute-path (text-utils:strcat path (9p:stat-name directory))))
                (add-child node
                           (make-instance 'm-tree
                                          :data (make-node-data absolute-path t)))))
            (loop for file in files do
              (let ((absolute-path (text-utils:strcat path (9p:stat-name file))))
                (add-child node
                           (make-instance 'm-tree
                                          :data (make-node-data absolute-path nil))))))
          node)))))

(defun rename-node (stream root-fid params)
  (lambda (node new-path)
    (with-9p-params (params)
      (let* ((*stream*   stream)
             (*root-fid* root-fid)
             (path (tree-path (data node))))
        (assert path)
        (with-cloned-root-fid  (*stream* cloned-root-fid)
          (9p:move-file *stream* cloned-root-fid path new-path))))))

(defun delete-node (stream root-fid params)
  (lambda (node)
    (with-9p-params (params)
      (let* ((*stream*   stream)
             (*root-fid* root-fid)
             (path (tree-path (data node))))
        (assert path)
        (with-cloned-root-fid (*stream* cloned-root-fid)
          (9p:remove-path *stream* cloned-root-fid path))))))

(defun create-node (stream root-fid params)
  (lambda (path dirp)
    (with-9p-params (params)
      (let* ((*stream*   stream)
             (*root-fid* root-fid))
        (assert path)
        (with-cloned-root-fid (*stream* cloned-root-fid :clunk-cloned-fid nil)
          (let ((created-fid nil))
            (if dirp
                (setf created-fid
                      (9p:create-path *stream*
                                      cloned-root-fid
                                      (if (fs:path-referencing-dir-p path)
                                          path
                                          (text-utils:strcat path "/"))))
                (setf created-fid
                      (9p:create-path *stream* cloned-root-fid path)))
            (9p:9p-clunk *stream* created-fid)))))))

(defun download-node (stream root-fid params)
  (lambda (node
           &optional
             (destination-file
              (make-temporary-file-from-node node)))
    (with-9p-params (params)
      (let* ((*stream*   stream)
             (*root-fid* root-fid)
             (path       (tree-path (data node))))
        (with-open-file (output-stream destination-file
                                       :direction         :output
                                       :if-exists         :supersede
                                       :if-does-not-exist :create
                                       :element-type      +octect-type+)
          (with-cloned-root-fid (*stream* cloned-root-fid)
            (9p:read-entire-file-apply-function stream
                                                cloned-root-fid
                                                path
                                                (lambda (data offset count)
                                                  (declare (ignore offset count))
                                                  (write-sequence data output-stream)))))
        (with-cloned-root-fid (*stream* cloned-root-fid)
          (let* ((info-source-node (9p:path-info *stream* cloned-root-fid path))
                 (permissions (9p:permissions-original-value (9p:stat-mode info-source-node)))
                 (destination-file-mode (logand permissions  #x7ff)))
            (fs:change-path-permissions destination-file destination-file-mode)))
        destination-file))))

(defun upload-node (stream root-fid params)
  (lambda (source-path destination-path)
    (with-9p-params (params)
      (let* ((*stream*   stream)
             (*root-fid* root-fid))
        (let ((source-permissions (fs:get-stat-mode source-path)))
          (with-open-file (input-stream source-path
                                        :direction    :input
                                        :element-type +octect-type+)
            (with-cloned-root-fid (*stream* cloned-root-fid)
              (9p:remove-path *stream* cloned-root-fid destination-path))
            (with-cloned-root-fid (*stream* cloned-root-fid :clunk-cloned-fid nil)
              (let* ((buffer (misc:make-array-frame +download-buffer+ 0 +octect-type+ t))
                     (fid    (9p:create-path *stream* cloned-root-fid destination-path)))
                (loop named write-loop
                      for read-so-far = (read-sequence buffer input-stream)
                        then (read-sequence buffer input-stream)
                      for offset = 0 then (+ offset read-so-far)
                      do
                         (9p:9p-write *stream* fid offset (subseq buffer 0 read-so-far))
                         (when (< read-so-far +download-buffer+)
                           (return-from write-loop t)))
                (9p:9p-clunk *stream* fid)))
            (with-cloned-root-fid (*stream* cloned-root-fid)
              (9p:change-mode *stream*
                              cloned-root-fid
                              destination-path source-permissions)
              (9p:read-all-pending-messages stream))))))))

(defun query-path (stream root-fid params)
  (lambda (path what)
    (with-9p-params (params)
      (let* ((*stream*   stream)
             (*root-fid* root-fid))
        (with-cloned-root-fid (*stream* cloned-root-fid)
          (a:when-let ((stat-entry (9p:path-info *stream* cloned-root-fid path)))
            (ecase what
              (:type
               (9p:stat-entry-type stat-entry))
              (:size
               (9p:stat-entry-type stat-entry))
              (:size-string
               (fs:octects->units-string (9p:stat-size stat-entry)))
              (:permissions-string
               (let ((mode (9p:stat-mode stat-entry)))
                 (format nil
                         (_ "User: ~a Group: ~a Others ~a")
                         (9p:permissions-user-string      mode)
                         (9p:permissions-group-string     mode)
                         (9p:permissions-others-string    mode)))))))))))

(defun collect-tree (stream root-fid params)
  (lambda (path)
    (with-9p-params (params)
      (let* ((*stream*   stream)
             (*root-fid* root-fid)
             (all-files  nil)
             (all-dirs   nil))
        (with-cloned-root-fid (*stream* cloned-root-fid :clunk-cloned-fid t)
          (multiple-value-bind (files directories)
              (9p:collect-tree *stream* cloned-root-fid path)
            (setf all-files files)
            (setf all-dirs  directories)))
        (values all-files all-dirs)))))

(defun generate-filesystem-window-handlers (path host port
                                            query fragment
                                            client-certificate client-key)
  (with-open-ssl-stream (stream socket host port client-certificate client-key)
    (let* ((9p:*tag*           10)
           (9p:*fid*           1)
           (9p:*messages-sent* '())
           (*stream*   stream)
           (*root-fid* (9p:mount *stream* "/"))
           (parameters (make-9p-parameters :message-sent 9p:*messages-sent*
                                           :fid          9p:*fid*
                                           :tag          9p:*tag*)))
      (list :query                                query
            :fragment                             fragment
            :socket                               socket
            :path                                 path
            :filesystem-expand-function           (expand-node   *stream* *root-fid* parameters)
            :filesystem-rename-function           (rename-node   *stream* *root-fid* parameters)
            :filesystem-delete-function           (delete-node   *stream* *root-fid* parameters)
            :filesystem-create-function           (create-node   *stream* *root-fid* parameters)
            :filesystem-download-function         (download-node *stream* *root-fid* parameters)
            :filesystem-upload-function           (upload-node   *stream* *root-fid* parameters)
            :filesystem-query-path-function       (query-path    *stream* *root-fid* parameters)
            :filesystem-collect-tree              (collect-tree  *stream* *root-fid* parameters)
            :filesystem-close-connection-function (lambda ()
                                                    (9p:close-client socket))))))

(defun iri->filesystem-window-handlers (kami-iri)
  (a:when-let ((parsed-iri (iri:iri-parse kami-iri :null-on-error t)))
    (multiple-value-bind (cached-certificate cached-key)
        (gemini-client:fetch-cached-certificate kami-iri)
      (multiple-value-bind (actual-iri host path query port fragment scheme)
          (gemini-client:displace-iri parsed-iri)
        (declare (ignore actual-iri scheme))
        (kami:generate-filesystem-window-handlers path
                                                  host
                                                  port
                                                  query
                                                  fragment
                                                  cached-certificate
                                                  cached-key)))))
