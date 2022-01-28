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

(in-package :filesystem-utils)

(define-constant +preprocess-include+ "^%include"              :test #'string=)

(define-constant +file-path-regex+ "[\\p{L},\\/,\\\\,\\.]+"    :test 'string=)

(defparameter *directory-sep-regexp*
  #+windows "\\"
  #-windows "\\/")

(defparameter *directory-sep*
  #+windows "\\"
  #-windows "/")

(defun copy-a-file (in out &key (overwrite nil))
  (if (and in
           (file-exists-p in)
           out
           (or (not (file-exists-p out))
               overwrite))
      (progn
        (uiop:copy-file in out)
        out)
      nil))

(defun create-a-file (path)
  (open path :direction :probe :if-does-not-exist :create))

(defun rename-a-file (old new)
  (nix:rename old new))

(defun file-size (filename)
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8)
                          :if-does-not-exist nil)
    (if (null stream)
        0
        (file-length stream))))

(defun slurp-file (filename &key (convert-to-string t))
  "A simple way to slurp a file."
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence seq stream)
      (if convert-to-string
          (babel:octets-to-string seq)
          seq))))

(defun dump-sequence-to-file (seq file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-sequence seq stream)))

(defun create-file (file &key (skip-if-exists nil))
  "create file and parent dir, if necessary"
  (when (not (and skip-if-exists
                  (file-exists-p file)))
    (let ((path-splitted (fs:split-path-elements file)))
      (when (and path-splitted
                 (> (length path-splitted) 1))
        (do* ((path-rest (subseq path-splitted 0
                                 (1- (length path-splitted)))
                         (rest path-rest))
              (path-so-far "" (if (and path-rest
                                       (not (string= "" (first-elt path-rest))))
                                  (concatenate 'string
                                               path-so-far
                                               *directory-sep*
                                               (first-elt path-rest)
                                               *directory-sep*)
                                  path-so-far)))
             ((null path-rest))
          (when (not (directory-exists-p path-so-far))
            (make-directory path-so-far)))
        (with-open-file (stream file
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create))))))

(defun has-extension (path ext)
  (let ((re (concatenate 'string ext "$")))
    (cl-ppcre:scan re path)))

(defun extension-dir-p (path)
  (let ((re (concatenate 'string *directory-sep-regexp* "$")))
    (cl-ppcre:scan re path)))

(defun strip-extension (file &key (strip-all nil))
  (let ((new (cl-ppcre:regex-replace "(?i)[a-z0-9]\\.[^.]+$" file "")))
    (if (string= file new)
        new
        (if strip-all
            (strip-extension new :strip-all t)
            new))))

(defun get-extension (file)
  (multiple-value-bind (matchedp res)
      (cl-ppcre:scan-to-strings "(?i)[a-z0-9]\(\\.[^.\/]+$\)" file)
    (when matchedp
      (first-elt res))))

(defun add-extension (file extension)
  (text-utils:strcat file "." extension))

(defun cat-parent-dir (parent direntry)
  (if (or (backreference-dir-p direntry)
          (loopback-reference-dir-p direntry))
      (format nil "~a~a" parent direntry)
      (format nil "~a~a~a" parent *directory-sep* direntry)))

(defmacro do-directory ((var) root &body body)
  (with-gensyms (dir)
    `(let ((,dir (nix:opendir ,root)))
       (unwind-protect
            (handler-case
                (do ((,var (cat-parent-dir ,root (nix:readdir ,dir))
                           (cat-parent-dir ,root (nix:readdir ,dir))))
                    ((cl-ppcre:scan "NIL$" ,var))
                  ,@body)
              (nix::enotdir () 0)
              (nix:eacces () 0)
              (nix:eloop () 0))
       (nix:closedir ,dir)))))

(defun collect-children (parent-dir)
  (let ((all-paths ()))
    (fs:do-directory (path) parent-dir
      (push path all-paths))
    (setf all-paths (sort all-paths #'string<))
    all-paths))

(defun collect-tree (unvisited-dirs &optional (accum '()))
  (declare (optimize (debug 0) (speed 3)))
  (cond
    ((null unvisited-dirs)
     accum)
    (t
     (let* ((children (collect-children (first unvisited-dirs)))
            (files        (remove-if #'directory-exists-p children))
            (directories  (mapcar (lambda (a) (text-utils:strcat a "/"))
                                  (remove-if (lambda (a)
                                               (or (file-exists-p a)
                                                   (backreference-dir-p a)
                                                   (loopback-reference-dir-p a)))
                                             children))))
       (collect-tree (append (rest unvisited-dirs) directories)
                     (append files accum))))))

(defun backreference-dir-p (path)
  (string= (path-last-element path) ".."))

(defun loopback-reference-dir-p (path)
  (string= (path-last-element path) "."))

(defun path-referencing-dir-p (path)
  (cl-ppcre:scan "/$" path))

(defun collect-files/dirs (root)
  (let ((all-files '())
        (all-dirs  '()))
    (labels ((collect (dir)
               (when (not (member dir all-files :test #'string=))
                 (let* ((all-children (collect-children dir))
                        (files        (remove-if #'directory-exists-p all-children))
                        (directories  (remove-if (lambda (a)
                                                   (or (file-exists-p a)
                                                       (backreference-dir-p a)
                                                       (loopback-reference-dir-p a)))
                                                 all-children)))
                   (setf all-files (append all-files files))
                   (setf all-dirs  (append all-dirs directories))
                   (loop for new-dir in directories do
                      (collect new-dir))))))
    (collect root)
      (values all-files
              all-dirs))))

(defgeneric prepend-pwd (object))

(defmethod prepend-pwd ((object string))
  (if (cl-ppcre:scan "^\\." object)
      (text-utils:strcat (os-utils:pwd) (subseq object 1))
      object))

(defmethod prepend-pwd ((object sequence))
  (map 'list #'prepend-pwd object))

(defun search-matching-file (root-directory &key (name ".*"))
  "Scan a filesystem saving files that match the provided criteria,
   does not follow symlinks."
  (let ((matched '())
        (scanner (cl-ppcre:create-scanner name)))
    (labels ((match (dir)
               (do-directory (path) dir
                   (let ((filename (path-last-element path)))
                     (cond
                       ((regular-file-p path)
                        (when (cl-ppcre:scan scanner filename)
                          (push path matched)))
                       ((and (not (cl-ppcre:scan "^\\.\\." filename))
                             (not (cl-ppcre:scan "^\\."   filename))
                             (dirp path))
                        (match path)))))))
      (match root-directory)
      matched)))

(defun regular-file-p (path)
  (nix:s-isreg (nix:stat-mode (nix:stat path))))

(defun dirp (path)
  (ignore-errors
   (and (nix:stat path)
        (nix:s-isdir (nix:stat-mode (nix:stat path))))))

(defun split-path-elements (path)
  (cl-ppcre:split *directory-sep-regexp* path))

(defun path-last-element (path)
  (let ((elements (cl-ppcre:split *directory-sep-regexp* path)))
    (and elements
         (last-elt elements))))

(defun path-first-element (path)
  (let ((elements (cl-ppcre:split *directory-sep-regexp* path)))
    (and elements
         (first-elt elements))))

(defun path-to-hidden-file-p (path)
  "unix-like only"
  (let ((last-element (path-last-element path)))
    (and path (cl-ppcre:scan "^\\." last-element))))

(defun strip-dirs-from-path (p)
  (multiple-value-bind (all registers)
      (cl-ppcre:scan-to-strings (concatenate 'string
                                             *directory-sep*
                                             "([^"
                                             *directory-sep*
                                             "]+)$")
                                p)
    (declare (ignore all))
    (and (> (length registers) 0)
         (elt registers 0))))

(defun parent-dir-path (path)
  (let ((splitted (remove-if #'(lambda (a) (string= "" a))
                             (split-path-elements path))))
    (cond
      ((> (length splitted) 1)
       (let ((res (if (string= (string (elt path 0)) *directory-sep*)
                      (concatenate 'string *directory-sep* (first splitted))
                      (first splitted))))
         (loop for i in (subseq splitted 1 (1- (length splitted))) do
              (setf res (concatenate 'string res *directory-sep* i)))
         (setf res (concatenate 'string res *directory-sep*))
         res))
      ((or (= (length splitted) 1)
           (null splitted))
       *directory-sep*)
      (t
       path))))

(defun append-file-to-path (dir filename)
  (let ((actual-dir (if (cl-ppcre:scan (concatenate 'string *directory-sep* "$")
                                       dir)
                        dir
                        (concatenate 'string dir *directory-sep*))))
    (concatenate 'string actual-dir filename)))

(defmacro define-stat-time (slot-name)
  (with-gensyms (stat)
    `(defun ,(format-symbol t "~:@(get-stat-~a~)" slot-name) (file)
       (restart-case
           (let ((,stat (nix:stat file)))
             (when ,stat
               (misc:time-unix->universal (,(format-symbol :nix "~:@(stat-~a~)" slot-name)
                                            ,stat))))
         (use-value (value) value)))))

(define-stat-time mtime)

(define-stat-time ctime)

(define-stat-time atime)

(defun file-hash (file)
  (num:fnv-hash-32 (slurp-file file :convert-to-string nil)))

(defun file-outdated-p (file &rest dependencies)
  (handler-bind ((nix:enoent #'(lambda (c)
                                 (declare (ignore c))
                                 (invoke-restart 'use-value nil))))
    (let ((mtime-file  (get-stat-mtime file))
          (mtimes-deps (remove-if #'null (mapcar #'get-stat-mtime dependencies))))
      (if mtime-file
          (remove-if #'(lambda (mtime) (<= mtime mtime-file)) mtimes-deps)
          t))))

(defun file-exists-p (f)
  (uiop:file-exists-p f))

(defun directory-exists-p (d)
  (uiop:directory-exists-p d))

(defun delete-file-if-exists (f)
  (uiop:delete-file-if-exists f))

(defun delete-directory-if-empty (d)
  (uiop:delete-empty-directory d))

(defun file-length-if-exists (f)
  (when (file-exists-p f)
    (with-open-file (stream f :element-type '(unsigned-byte 8))
      (file-length stream))))

(defun home-dir (&key (add-separator-ends nil))
  (let ((home (os-utils:getenv "HOME")))
    (if add-separator-ends
        (text-utils:strcat home *directory-sep*)
        home)))

(cffi:defcfun (ffi-mkstemp "mkstemps") :int (template :pointer) (suffix-length :int))

(defun %mkstemp (prefix suffix)
  (let ((template (text-utils:strcat prefix "XXXXXX" suffix)))
    (cffi:with-foreign-string (ptr-template template)
      (ffi-mkstemp ptr-template (length suffix))
      (cffi:foreign-string-to-lisp ptr-template))))

(defparameter *temporary-files-created* ())

(defun temporary-file (&key (temp-directory nil) (extension ""))
  (let ((tmpdir (or temp-directory
                    (os-utils:default-temp-dir))))
    (let ((filepath (if tmpdir
                    (%mkstemp (format nil "~a~a~a" tmpdir *directory-sep*
                                      config:+program-name+)
                              extension)
                    (%mkstemp (format nil "~atmp~a~a" *directory-sep* *directory-sep*
                                      config:+program-name+)
                              extension))))
      (push filepath *temporary-files-created*)
      filepath)))

(defun clean-temporary-files ()
  (dolist (temporary-file *temporary-files-created*)
    (delete-file-if-exists temporary-file)))

(defmacro with-anaphoric-temp-file ((stream &key (prefix nil) (unlink nil)) &body body)
  `(let ((temp-file (temporary-file ,prefix))) ; anaphora
       (unwind-protect
            (with-open-file (,stream temp-file
                                     :element-type      '(unsigned-byte 8)
                                     :direction         :output
                                     :if-exists         :supersede
                                     :if-does-not-exist :create)
              ,@body)
         ,(if unlink
              `(delete-file-if-exists temp-file)
              nil))))

(defparameter *temporary-directories-created* ())

(defun temporary-directory (&optional (temp-parent-directory nil))
  (let ((tmpdir (or temp-parent-directory
                    (os-utils:default-temp-dir))))
    (let ((directory-path (if tmpdir
                              (nix:mkdtemp (format nil "~a~a"
                                                   tmpdir
                                                   config:+program-name+))
                              (nix:mkdtemp (format nil "~atmp~a"
                                                   *directory-sep*
                                                   config:+program-name+)))))
      (push directory-path *temporary-directories-created*)
      directory-path)))

(cffi:defcfun (ffi-fnmatch "fnmatch")
  :int
  (pattern       :pointer)
  (string        :pointer)
  (flags         :int))

(defun filename-pattern-match (pattern string)
  (cffi:with-foreign-string (ptr-pattern pattern)
    (cffi:with-foreign-string (ptr-string string)
      (zerop (ffi-fnmatch ptr-pattern ptr-string 0)))))

(defun children-matching-path (pattern)
  (let* ((parent   (parent-dir-path pattern))
         (children (collect-children parent)))
    (remove-if-not (lambda (a) (filename-pattern-match pattern a))
                   children)))

(defun recursive-delete (path)
  (if (regular-file-p path)
      (delete-file-if-exists path)
      (let ((children (collect-children path)))
        (dolist (file-or-dir children)
          (cond
            ((file-exists-p file-or-dir)
             (delete-file-if-exists file-or-dir))
            ((and (directory-exists-p file-or-dir)
                  (not (or (loopback-reference-dir-p file-or-dir)
                           (backreference-dir-p      file-or-dir))))
             (recursive-delete file-or-dir))))
        (delete-directory-if-empty path))))

(defun clean-temporary-directories ()
  (dolist (temporary-directory *temporary-directories-created*)
    (recursive-delete temporary-directory)))

(defun has-file-permission-p (file permission)
  (find permission (osicat:file-permissions file) :test #'eq))

(defun file-can-write-p (file)
  (has-file-permission-p file :user-write))

(defmacro gen-permission-files (&rest modes)
  `(progn
     ,@(loop for mode in modes collect
            `(define-constant ,(misc:format-fn-symbol t "+~a+" mode)
                 ,mode
               :test #'eql))))

(gen-permission-files
   nix:s-irwxu nix:s-irusr nix:s-iwusr nix:s-ixusr nix:s-irwxg nix:s-irgrp nix:s-iwgrp
   nix:s-ixgrp nix:s-irwxo nix:s-iroth nix:s-iwoth nix:s-ixoth nix:s-isuid nix:s-isgid)

(defun set-file-permissions (file mode)
  (nix:chmod file mode))

(misc:defcached cached-directory-files ((path) :test equal)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (if (gethash path cache)
      (gethash path cache)
      (progn
        (setf (gethash path cache) (uiop:directory-files path))
        (cached-directory-files path))))

(defun directory-files (path)
  (and path
       (uiop:directory-files path)))

(defun make-directory (path)
  (if (not (cl-ppcre:scan (concatenate 'string *directory-sep* "$") path))
      (make-directory (concatenate 'string path *directory-sep*))
      (ensure-directories-exist path)))

(defun package-path ()
  (uiop:pathname-parent-directory-pathname
   (asdf:component-pathname
    (asdf:find-component (symbolicate (string-upcase config:+program-name+))
                         nil))))

(defun file-in-package (name)
  (concatenate 'string (namestring (package-path)) name))

(defparameter *file-link-to* nil)

(define-constant +rel-link+ :rel)

(define-constant +abs-link+ :abs)

(defmacro see-file (&body forms)
  (if (> (length forms) 1)
      (warn "see-file: too many elements in forms, must be exactly 2"))
  (let ((path (first-elt forms)))
    (when (not (stringp path))
      (error (format nil "see-file: the path ~a is not a string" path)))
    (when (= (length path) 0)
      (error (format nil "see-file: the path ~a is to short" path)))
    (if (string= *directory-sep* (string (first-elt path)))
        `(setf *file-link-to* (cons ,path +abs-link+))
        `(setf *file-link-to* (cons ,path +rel-link+)))))

(defun link-file-path (file)
  (misc:with-load-forms-in-var (*file-link-to* link-file file)
    (if link-file
        (destructuring-bind (path . type)
            link-file
          (if (eq type +rel-link+)
              (cat-parent-dir (parent-dir-path file) path)
              path))
        nil)))

(defmacro file-is-link-if-else ((file link-file-pointed) is-link-forms is-not-link-forms)
  `(let ((,link-file-pointed (link-file-path ,file)))
     (if ,link-file-pointed
         ,is-link-forms
         ,is-not-link-forms)))

(defun pathname->namestring (p)
  (uiop:native-namestring p))

(defun namestring->pathname (p)
 (uiop:parse-native-namestring p))

(defun read-single-form (file)
  (with-open-file (stream file :direction :input :if-does-not-exist nil)
    (when stream
      (read stream))))

(defun eq-filename (a b)
  (flet ((strip (a) (strip-dirs-from-path (pathname->namestring a))))
    (string= (strip a)
             (strip b))))

(define-constant +file-size-units+ '("KiB" "MiB" "GiB") :test #'equalp)

(defun octects->units (octects units)
  (let* ((exponent (case units
                     (:kib 1)
                     (:mib 2)
                     (:gib 3)
                     (otherwise 1)))
         (scaled (/ octects (expt 1024 exponent))))
      (values scaled
              (elt +file-size-units+ (1- exponent)))))

(defgeneric octects->units-string (object))

(defmethod octects->units-string (object)
  (format nil (config:_ "invalid value: ~a") object))

(defmethod octects->units-string ((object number))
  (let ((decimals (loop
                    for number = object then (truncate (/ number 10)) while (> number 0)
                    for results = 0 then (1+ results)
                    finally (return results))))
    (cond
      ((or (null decimals)
           (<= decimals 3))
       (format nil (config:_ "~a bytes") object))
      ((<= 4 decimals 6)
       (format nil (config:_ "~a Kib") (truncate (octects->units object :kib))))
      ((<= 7 decimals 9)
       (format nil (config:_ "~a Mib") (truncate (octects->units object :mib))))
      (t
       (format nil (config:_ "~a Gib") (truncate (octects->units object :gib)))))))

(defgeneric normalize-path (object))

(defmethod normalize-path ((object null))
  nil)

(defmethod normalize-path ((object string))
  (flet ((make-stack ()
           (make-instance 'stack:stack
                          :test-fn #'string=))
         (fill-input-stack (stack)
           (loop
              for segment in (remove-if #'text-utils:string-empty-p
                                        (reverse (cl-ppcre:split "/" object)))
              do
                (stack:stack-push stack segment))))
    (let* ((ends-with-separator-p (text-utils:string-ends-with-p "/" object))
           (ends-with-dots        nil)
           (input-stack  (make-stack))
           (output-stack (make-stack)))
      (fill-input-stack input-stack)
      (labels ((fill-output-buffer ()
                 (when (not (stack:stack-empty-p input-stack))
                   (let ((popped (stack:stack-pop input-stack)))
                     (cond
                       ((and (string= popped "..")
                             (not (stack:stack-empty-p output-stack)))
                        (stack:stack-pop output-stack)
                        (when (stack:stack-empty-p input-stack)
                          (setf ends-with-dots t)))
                       ((and (or (string= popped "..")
                                 (string= popped "."))
                             (stack:stack-empty-p input-stack))
                        (setf ends-with-dots t)
                        (stack:stack-push output-stack "/"))
                       ((and (string/= popped ".")
                             (string/= popped ".."))
                        (stack:stack-push output-stack popped))))
                   (fill-output-buffer)))
               (output-stack->list ()
                 (reverse (loop
                             for segment = (stack:stack-pop output-stack)
                             while segment
                             collect segment))))
        (fill-output-buffer)
        (let* ((joinable (output-stack->list))
               (merged   (if joinable
                             (if (or ends-with-separator-p
                                     ends-with-dots)
                                 (text-utils:wrap-with (text-utils:join-with-strings joinable
                                                                                     "/")
                                                       "/")
                                 (text-utils:strcat "/" (text-utils:join-with-strings joinable
                                                                                      "/")))
                             "/")))
          (cl-ppcre:regex-replace-all "//" merged ""))))))
