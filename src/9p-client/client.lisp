(in-package :9p-client)

(define-constant +byte-type+            '(unsigned-byte 8) :test #'equalp)

(define-constant +version+               "9P2000"          :test #'string=)

(define-constant +message-length-size+          4          :test #'=)

(define-constant +message-type-size+            1          :test #'=)

(define-constant +message-tag-size+             2          :test #'=)

(define-constant +message-string-length-size+   2          :test #'=)

(define-constant +nofid+                        #xffffffff :test #'=)

(define-constant +create-for-read+       #x0               :test #'=)

(define-constant +create-for-write+      #x1               :test #'=)

(define-constant +create-for-read-write+ #x2               :test #'=)

(define-constant +create-for-exec+       #x3               :test #'=)

(define-constant +create-dir+            #x80000000        :test #'=)

(define-constant +open-truncate+         #x10              :test #'=)

(define-constant +open-remove-on-clunk+  #x40              :test #'=)

(define-constant +standard-socket-port+   564              :test #'=)

(define-constant +nwname-clone+             0              :test #'=)

(defparameter *buffer-size* 252)

(defparameter *tag* 8)

(defparameter *fid* #x00000001)

(defparameter *messages-sent* '())

(defun tags-exists-p-clsr (tag-looking-for)
  (lambda (a) (octects= tag-looking-for (car a))))

(defun fire-response (tag message-type data)
  (let ((found (find-if (tags-exists-p-clsr tag) *messages-sent*)))
    (if found
        (let ((fn (cdr found)))
          (setf *messages-sent* (remove-if (tags-exists-p-clsr tag) *messages-sent*))
          (funcall fn message-type data))
        (warn (format nil "received unknown response message tag ~a" tag)))))

(defun append-tag-callback (tag function)
  (setf *messages-sent* (push (cons tag function) *messages-sent*)))

(defun read-all-pending-message (socket)
  (when *messages-sent*
    (multiple-value-bind (message-type rtag data)
        (restart-case
            (read-message socket)
          (ignore-error (e)
            (values (message-type e)
                    (tag          e)
                    #())))
      (fire-response rtag message-type data)
      (read-all-pending-message socket))))

(defun next-tag ()
  (prog1
      (make-octects *tag* 2)
    (incf *tag*)))

(defun next-fid ()
  (prog1
      (int32->bytes *fid*)
    (incf *fid*)))

(defun bytes->int (bytes)
  (let ((res #x0000000000000000)
        (ct  0))
    (map nil
         (lambda (a)
           (setf res (boole boole-ior
                            (ash a ct)
                            res))
           (incf ct 8))
         bytes)
    res))

(defmacro gen-intn->bytes (bits)
  (let ((function-name (alexandria:format-symbol t "~:@(int~a->bytes~)" bits)))
    `(defun ,function-name (val &optional (count 0) (res '()))
       (if (>= count ,(/ bits 8))
           (reverse res) ; little endian
           (,function-name (ash val -8)
                           (1+ count)
                           (push (boole boole-and val #x00ff)
                                 res))))))

(gen-intn->bytes  8)

(gen-intn->bytes  16)

(gen-intn->bytes  32)

(gen-intn->bytes  64)

(gen-intn->bytes 512)

(gen-intn->bytes 416)

(defun big-endian->little-endian (bytes)
  (reverse bytes))

(defun vcat (a b)
  (concatenate 'vector a b))

(defclass octects ()
  ((value
    :initform 0
    :initarg :value
    :accessor value)
   (size
    :initform 0
    :initarg :size
    :accessor size)))

(defgeneric octects= (a b))

(defgeneric encode (object))

(defgeneric decode (object))

(defmethod encode ((object octects))
  (with-accessors ((value value)
                   (size size)) object
    (let ((bytes (ecase size
                   (1  (int8->bytes  value))
                   (2  (int16->bytes  value))
                   (4  (int32->bytes  value))
                   (8  (int64->bytes  value))
                   (13 (int416->bytes value))
                   (32 (int512->bytes value))))
          (res   (make-array size :element-type +byte-type+)))
      (loop for i from 0 below size do
        (setf (elt res i) (elt bytes i)))
      res)))

(defmethod octects= ((a octects) b)
  (= (value a) b))

(defmethod octects= ((a number) (b octects))
  (octects= b a))

(defmethod octects= ((a number) (b number))
  (= b a))

(defun add-size (msg)
  (let ((length (int32->bytes (+ +message-length-size+ (length msg)))))
    (vcat length msg)))

(defun start-client (host port)
  (usocket:socket-connect host
                          port
                          :protocol     :stream
                          :element-type +byte-type+))

(defun close-client (socket)
  (usocket:socket-close socket))

(defun send-message (socket message)
  (with-accessors ((stream usocket:socket-stream)) socket
    (write-sequence message stream)
    (finish-output stream)))

(defun encode-string (string)
  (let* ((bytes (babel:string-to-octets string))
         (size  (int16->bytes (length bytes))))
    (vcat size bytes)))

(defmethod encode ((object string))
  (encode-string object))

(defmethod encode ((object list))
  (let ((buffer (make-message-buffer (length object))))
    (loop for i from 0 below (length object) do
      (setf (elt buffer i) (elt object i)))
    buffer))

(defmethod encode (object)
  object)

(defmethod decode-string (data)
  (let ((size (bytes->int (subseq data 0 +message-string-length-size+))))
    (babel:octets-to-string (subseq data
                                    +message-string-length-size+
                                    (+ +message-string-length-size+ size))
                            :errorp nil)))

(defun compose-message (message-type tag &rest params)
  (let ((actual-params (reduce #'vcat (mapcar #'encode params))))
    (add-size (reduce #'vcat (list (encode message-type) (encode tag) actual-params)))))

(defun displace-response (response)
  (let ((message-type (subseq response 0 +message-type-size+))
        (message-tag  (subseq response
                              +message-type-size+
                              (+ +message-type-size+
                                 +message-tag-size+)))
        (data         (subseq response
                              (+ +message-type-size+
                                 +message-tag-size+))))
    (values (bytes->int message-type)
            (bytes->int message-tag)
            data)))

(defun make-message-buffer (size)
  (make-array size :element-type +byte-type+))

(defun error-response-p (response)
  (multiple-value-bind (message-type x y)
      (displace-response response)
    (declare (ignore x y))
    (= message-type *rerror*)))

(defun read-message (socket)
  (with-accessors ((stream usocket:socket-stream)) socket
    (let ((message-length-buffer  (make-message-buffer +message-length-size+)))
      (read-sequence message-length-buffer stream)
      (let* ((message-length (bytes->int message-length-buffer))
             (buffer         (make-message-buffer (- message-length +message-length-size+))))
        (read-sequence buffer stream)
        (multiple-value-bind (message-type tag data)
            (displace-response buffer)
          (if (error-response-p buffer)
              (error '9p-error
                     :message-type message-type
                     :tag          tag
                     :error-value (decode-string data))
              (values message-type tag data)))))))

(defun make-octects (number size)
  (make-instance 'octects :value number :size size))

(defun send-version (socket tag)
  (let ((message (compose-message (make-octects *tversion* 1)
                                  tag
                                  (make-octects *buffer-size* 4)
                                  +version+)))
    (send-message socket message)
    (multiple-value-bind (message-type rtag data)
        (read-message socket)
      (assert (= message-type *rversion*))
      (if (octects= rtag tag)
          (let ((message-size     (bytes->int    (subseq data 0 4)))
                (protocol-version (decode-string (subseq data 4))))
            (setf *buffer-size* message-size)
            (values message-size protocol-version))
          (error '9p-initialization-error :tag tag :rtag rtag)))))

(defmacro with-new-tag ((tag) &body body)
  `(let ((,tag (next-tag)))
     ,@body))

(defmacro with-new-fid ((fid) &body body)
  `(let ((,fid (next-fid)))
     ,@body))

(defun initialize-session (host port)
  (with-new-tag (tag)
    (let* ((socket (start-client host port)))
      (multiple-value-bind (buffer-size protocol-version)
          (send-version socket tag)
      (values socket protocol-version buffer-size)))))

(defun decode-quid (data)
  (let ((file-type    (first-elt data))
        (file-version (subseq data 1 4))
        (file-path    (subseq data 1 5)))
    (values file-type
            (bytes->int file-version)
            (bytes->int file-path))))

(defun dummy-callback (message-type data)
  (declare (ignore message-type data)))

(defun dump-callback (message-type data)
  (format t "reply mtype  ~a ~a~%" message-type data))

(defun 9p-attach (socket root
                  &key
                    (username "nobody")
                    (callback #'dummy-callback))
  (with-new-tag (tag)
    (with-new-fid (root-fid)
      (let* ((message (compose-message (make-octects *tattach* 1)
                                       tag
                                       root-fid
                                       (make-octects +nofid+ 4)
                                       username
                                       root)))
        (append-tag-callback tag callback)
        (send-message socket message)
        root-fid))))

(defun 9p-create (socket parent-dir-fid path
                  &key
                    (callback    #'dummy-callback)
                    (permissions #o640)
                    (mode        +create-for-read-write+))
  "Note: path is relative to root, see attach,
   Also note that successfully creating a file will open it."
  (with-new-tag (tag)
    (let* ((message (compose-message (make-octects *tcreate* 1)
                                     tag
                                     parent-dir-fid
                                     path
                                     (make-octects permissions 4)
                                     (make-octects mode 1))))
      (append-tag-callback tag callback)
      (send-message socket message))))

(defun 9p-open (socket fid
                &key
                  (callback #'dummy-callback)
                  (mode     +create-for-read+))
  "Note before opening you have to 'walk' the file to get the corresponding fid."
  (with-new-tag (tag)
    (let* ((message (compose-message (make-octects *topen* 1)
                                     tag
                                     fid
                                     (make-octects mode 1))))
      (append-tag-callback tag callback)
      (send-message socket message))))

(defgeneric 9p-write (socket fid offset data &key callback))

(defmethod 9p-write (socket fid offset (data vector)
                     &key
                       (callback #'dummy-callback))
  (let* ((data-chunk-num    (floor (/ (length data) *buffer-size*)))
         (data-chunk-length (if (> (length data) *buffer-size*)
                                (* data-chunk-num *buffer-size*)
                                (length data)))
         (remainder         (if (> (length data) *buffer-size*)
                                (- (length data)
                                   (* data-chunk-num *buffer-size*))
                                0)))
    (flet ((write-chunk (chunk chunk-offset)
             (with-new-tag (tag)
               (let* ((message (compose-message (make-octects *twrite* 1)
                                                tag
                                                fid
                                                (make-octects chunk-offset 8)
                                                (make-octects (length chunk) 4)
                                                chunk)))
                 (append-tag-callback tag callback)
                 (send-message socket message)))))
      (loop for i from 0 below (- (length data) remainder) by data-chunk-length do
        (let ((chunk (subseq data i (+ i data-chunk-length))))
          (write-chunk chunk (+ offset i))))
      (when (> remainder 0)
        (write-chunk (subseq data (- (length data) remainder))
                     (+ offset (- (length data) remainder)))))))

(defmethod 9p-write (socket fid offset (data string)
                     &key
                       (callback #'dummy-callback))
  (9p-write socket fid offset (babel:string-to-octets data) :callback callback))

(defun 9p-walk (socket root-fid new-fid new-name &key (callback #'dummy-callback))
  (if (and (numberp new-name)
           (= 0 new-name))
      (%9p-walk-self socket root-fid new-fid :callback callback)
      (with-new-tag (tag)
        (let* ((message (compose-message (make-octects *twalk* 1)
                                         tag
                                         root-fid
                                         new-fid
                                         (make-octects 1 2)
                                         new-name)))
          (append-tag-callback tag callback)
          (send-message socket message)))))

(defun %9p-walk-self (socket root-fid new-fid &key (callback #'dummy-callback))
  (with-new-tag (tag)
    (let* ((message (compose-message (make-octects *twalk* 1)
                                     tag
                                     root-fid
                                     new-fid
                                     (make-octects 0 2))))
      (append-tag-callback tag callback)
      (send-message socket message))))

(defun 9p-remove (socket fid &key (callback #'dummy-callback))
  (with-new-tag (tag)
    (let* ((message (compose-message (make-octects *tremove* 1)
                                     tag
                                     fid)))
      (append-tag-callback tag callback)
      (send-message socket message))))

(defun 9p-clunk (socket fid &key (callback #'dummy-callback))
  (with-new-tag (tag)
    (let* ((message (compose-message (make-octects *tclunk* 1)
                                     tag
                                     fid)))
      (append-tag-callback tag callback)
      (send-message socket message))))

(defun 9p-stat (socket fid &key (callback #'dummy-callback))
  (with-new-tag (tag)
    (let* ((message (compose-message (make-octects *tstat* 1)
                                     tag
                                     fid)))
      (append-tag-callback tag callback)
      (send-message socket message))))

(defun 9p-read (socket fid offset chunk-length &key (callback #'dummy-callback))
  (with-new-tag (tag)
    (let* ((message (compose-message (make-octects *tread* 1)
                                     tag
                                     fid
                                     (make-octects offset 8)
                                     (make-octects chunk-length 4))))
      (append-tag-callback tag callback)
      (send-message socket message))))

(defun decode-read-reply (data &optional (as-string nil))
  (let ((count    (bytes->int (subseq data 0 4)))
        (raw-data (subseq data 4)))
    (values (if as-string
                (babel:octets-to-string raw-data :errorp nil)
                raw-data)
            count)))

(defun encoded-string-offset (decoded-string)
  (+  (length decoded-string)
      +message-string-length-size+))

(defun decode-rstat (data)
  (flet ((->int (start end)
           (bytes->int (subseq data start end))))
    (let* ((entry-size1           (->int  0  2))
           (entry-size2           (->int  2  4))
           (ktype                 (->int  4  6))
           (kdev                  (->int  6 10))
           (entry-type            (->int 10 11))
           (version               (->int 11 15))
           (path                  (->int 15 23))
           (mode                  (->int 23 27))
           (atime                 (->int 27 31))
           (mtime                 (->int 31 35))
           (size                  (->int 35 43))
           (strings-start         43)
           (name                  (decode-string (subseq data strings-start)))
           (name-offset           (encoded-string-offset name))
           (user-id               (decode-string (subseq data
                                                         (+ strings-start
                                                            name-offset))))
           (user-id-offset        (+ strings-start
                                     (encoded-string-offset user-id)
                                     name-offset))
           (group-id              (decode-string (subseq data user-id-offset)))
           (group-id-offset       (+ user-id-offset
                                     (encoded-string-offset group-id)))
           (last-modified-from-id (decode-string (subseq data group-id-offset))))
      (values entry-size1
              entry-size2
              ktype
              kdev
              entry-type
              version
              path
              mode
              atime
              mtime
              size
              name
              user-id
              group-id
              last-modified-from-id))))

;;; high level routines

(defun read-all-pending-messages-ignoring-errors (socket)
  (handler-bind ((9p-error
                   (lambda (e)
                     (invoke-restart 'ignore-error e))))
    (read-all-pending-message socket)))

(defun create-directory (socket parent-fid directory-name &key (permissions #o760))
  (9p-create socket
             parent-fid
             directory-name
             :permissions (logior +create-dir+ permissions)
             :mode        +create-for-read+)
  (read-all-pending-messages-ignoring-errors socket))

(defun create-path (socket parent-fid path &key (file-permissions #o640))
  (let ((fs:*directory-sep-regexp* "\\/")
        (path-elements             (fs:split-path-elements path))
        (last-is-dir-p             (cl-ppcre:scan "\\/$" path)))
    (labels ((%create-dirs (path-elements)
               (when path-elements
                 (create-directory socket parent-fid (first path-elements))
                 (read-all-pending-messages-ignoring-errors socket)
                 (%create-dirs (rest path-elements)))))
      (%create-dirs (misc:safe-all-but-last-elt path-elements))
      (if last-is-dir-p
          (create-directory socket parent-fid (last-elt path-elements))
          (9p-create        socket parent-fid (last-elt path-elements)
                            :permissions file-permissions))
      (read-all-pending-messages-ignoring-errors socket))))

(defun mount (host root-path &optional (port +standard-socket-port+))
  (multiple-value-bind (socket version)
      (initialize-session host port)
    (let* ((root-fid (9p-attach socket root-path)))
      (read-all-pending-message socket)
      (values socket root-fid version))))

(defun open-path (socket root-fid path
                         &key
                           (walk-callback #'dummy-callback)
                           (open-callback #'dummy-callback)
                           (mode          +create-for-read+))
  (let ((fs:*directory-sep-regexp* "\\/")
        (path-elements             (fs:split-path-elements path)))
    (labels ((walk-dirs (path-elements parent-fid)
               (with-new-fid (fid)
               (if path-elements
                   (progn
                     (9p-walk socket
                              parent-fid
                              fid
                              (first path-elements)
                              :callback walk-callback)
                     (read-all-pending-message socket)
                     (walk-dirs (rest path-elements) fid))
                   parent-fid))))
      (let ((fid (walk-dirs path-elements root-fid)))
        (9p-open socket fid :callback open-callback :mode mode)
        (read-all-pending-message socket)
        fid))))

(defun cat-reply-vector (a b)
  (concatenate '(vector (unsigned-byte 8)) a b))

(defun slurp-file (socket root-fid path &key (buffer-size *buffer-size*))
  (let ((res (make-array 0 :element-type +byte-type+ :adjustable nil))
        (fid (open-path socket root-fid path)))
    (labels ((slurp (offset)
               (9p-read socket
                        fid
                        offset
                        buffer-size
                        :callback (lambda (x reply)
                                    (declare (ignore x))
                                    (multiple-value-bind (data count)
                                        (decode-read-reply reply nil)
                                      (setf res (cat-reply-vector res data))
                                      (when (or (= count buffer-size)
                                                (= count *buffer-size*))
                                          (slurp (+ offset count))))))))
      (slurp 0)
      (read-all-pending-message socket)
      res)))

(defun example (path &optional (root (os-utils:getenv "HOME")) (host "localhost") (port 10564))
  (multiple-value-bind (socket root-fid)
      (mount host root port)
    (with-new-fid (saved-root-fid)
      (9p-walk socket root-fid saved-root-fid +nwname-clone+)
      (let ((fid (open-path socket root-fid path)))
        (9p-read socket fid 0 10
                 :callback (lambda (x data)
                             (declare (ignore x))
                             (format t "read: ~a~%" (decode-read-reply data t))))
        (read-all-pending-message socket)))))

(defun example-slurp (path
                      &optional (root (os-utils:getenv "HOME")) (host "localhost") (port 10564))
  (let ((*buffer-size* 10))
    (multiple-value-bind (socket root-fid)
        (mount host root port)
      (babel:octets-to-string (slurp-file socket root-fid path :buffer-size 3) :errorp nil))))

(defun example-stat (&optional (root (os-utils:getenv "HOME")) (host "localhost") (port 10564))
  (multiple-value-bind (socket root-fid)
      (mount host root port)
    (9p-stat socket root-fid
             :callback (lambda (x data)
                         (declare (ignore x))
                         (format t "raw ~a~%stat ~a~%"  data (multiple-value-list (decode-rstat data)))))
    (read-all-pending-message socket)))

(defun pad-read-dir-reply (data)
  (cat-reply-vector #(0 0) data))

(defun read-directory (&optional (root (os-utils:getenv "HOME")) (host "localhost") (port 10564))
  (multiple-value-bind (socket root-fid)
      (mount host root port)
      (9p-open socket root-fid :mode +create-for-read+)
      (9p-read socket
               root-fid
               0 4
               :callback (lambda (x data)
                           (declare (ignore x))
                           (format t
                                   "raw ~a~%" data)))
      (read-all-pending-message socket)))
