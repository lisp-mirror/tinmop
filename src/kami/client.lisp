(in-package :kami)

(a:define-constant +download-buffer+ (expt 2 24)        :test #'=)

(a:define-constant +octect-type+     '(unsigned-byte 8) :test #'equalp)

(defparameter *stream*   nil)

(defparameter *root-fid* nil)

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

(defmacro with-cloned-root-fid ((stream cloned-fid) &body body)
  `(let ((,cloned-fid (9p:clone-fid ,stream *root-fid*)))
     ,@body))

(defun expand-node (stream root-fid)
  (lambda (node)
    (let* ((*stream*   stream)
           (*root-fid* root-fid)
           (path (tree-path (data node))))
      (with-cloned-root-fid  (*stream* root-fid)
        (let* ((entries     (9p:collect-directory-children *stream* root-fid path))
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
                                        :data (make-node-data absolute-path nil))))))))
    node))

(defun rename-node (stream root-fid)
  (lambda (node new-path)
    (let* ((*stream*   stream)
           (*root-fid* root-fid)
           (path (tree-path (data node))))
      (assert path)
      (with-cloned-root-fid  (*stream* root-fid)
        (9p:move-file *stream* root-fid path new-path)))))

(defun delete-node (stream root-fid)
  (lambda (node)
    (let* ((*stream*   stream)
           (*root-fid* root-fid)
           (path (tree-path (data node))))
      (assert path)
      (with-cloned-root-fid (*stream* root-fid)
        (9p:remove-path *stream* root-fid path)))))

(defun create-node (stream root-fid)
  (lambda (path dirp)
    (let* ((*stream*   stream)
           (*root-fid* root-fid))
      (assert path)
      (with-cloned-root-fid (*stream* root-fid)
        (if dirp
            (9p:create-path *stream* root-fid (if (cl-ppcre:scan "/$" path)
                                                  path
                                                  (text-utils:strcat path "/")))
            (9p:create-path *stream* root-fid path))))))

(defun download-node (stream root-fid)
  (lambda (node
           &optional
             (destination-file
              (make-temporary-file-from-node node)))
    (let* ((*stream*   stream)
           (*root-fid* root-fid)
           (path       (tree-path (data node))))
      (with-open-file (output-stream destination-file
                                     :direction         :output
                                     :if-exists         :supersede
                                     :if-does-not-exist :create
                                     :element-type      +octect-type+)
        (with-cloned-root-fid (*stream* root-fid)
          (9p:read-entire-file-apply-function stream
                                              root-fid
                                              path
                                              (lambda (data offset count)
                                                (declare (ignore offset count))
                                                (write-sequence data output-stream))))))
    destination-file))

(defun upload-node (stream root-fid)
  (lambda (source-path destination-path)
    (let* ((*stream*   stream)
           (*root-fid* root-fid))
      (with-open-file (input-stream source-path
                                    :direction    :input
                                    :element-type +octect-type+)
        (with-cloned-root-fid (*stream* root-fid)
          (9p:remove-path *stream* root-fid destination-path))
        (with-cloned-root-fid (*stream* root-fid)
          (let* ((buffer (misc:make-array-frame +download-buffer+ 0 +octect-type+ t))
                 (fid    (9p:create-path *stream* root-fid destination-path)))
            (loop named write-loop
                  for read-so-far = (read-sequence buffer input-stream)
                    then (read-sequence buffer input-stream)
                  for offset = 0 then (+ offset read-so-far)
                  do
                     (9p:9p-write *stream* fid offset (subseq buffer 0 read-so-far))
                     (when (< read-so-far +download-buffer+)
                       (return-from write-loop t)))
            (9p:9p-clunk *stream* fid)
            (9p:read-all-pending-message stream)))))))

(defun query-path (stream root-fid)
  (lambda (path what)
    (let* ((*stream*   stream)
           (*root-fid* root-fid))
      (ecase what
        (:size
         (with-cloned-root-fid (*stream* root-fid)
           (a:when-let ((stat-entry (9p:path-exists-p *stream* root-fid path)))
             (9p:stat-size stat-entry))))))))

(defun generate-filesystem-window-handlers (root host port client-certificate client-key)
  (with-open-ssl-stream (stream socket host port client-certificate client-key)
    (let* ((*stream*   stream)
           (*root-fid* (9p:mount *stream* root)))
      (list :filesystem-expand-function            (expand-node   *stream* *root-fid*)
            :filesystem-rename-function            (rename-node   *stream* *root-fid*)
            :filesystem-delete-function            (delete-node   *stream* *root-fid*)
            :filesystem-create-function            (create-node   *stream* *root-fid*)
            :filesystem-download-function          (download-node *stream* *root-fid*)
            :filesystem-upload-function            (upload-node   *stream* *root-fid*)
            :filesystem-query-path-function        (query-path    *stream* *root-fid*)
            :filesystem-close-connection-function  (lambda (stream)
                                                     (declare (ignore stream))
                                                     (9p:close-client socket))))))

(defun iri->filesystem-window-handlers (kami-iri)
  (a:when-let ((parsed-iri (iri:iri-parse kami-iri :null-on-error t)))
    (multiple-value-bind (cached-certificate cached-key)
        (gemini-client:fetch-cached-certificate kami-iri)
      (multiple-value-bind (actual-iri host path query port fragment scheme)
          (gemini-client:displace-iri parsed-iri)
        (declare (ignore actual-iri query fragment scheme))
        (kami:generate-filesystem-window-handlers path
                                                  host
                                                  port
                                                  cached-certificate
                                                  cached-key)))))
