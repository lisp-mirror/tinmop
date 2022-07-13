;; tinmop module for utility move command in thread window
;; Copyright © 2022 cage

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

(defpackage :generate-gemlog
  (:use :cl
        :misc
        :fs
        :html-utils
        :gemini-parser
        :text-utils)
  (:local-nicknames (:a :alexandria)))

(in-package :generate-gemlog)

(a:define-constant +meta-node-attribute+ "META" :test #'string=)

(a:define-constant +meta-topic-key+ :topic :test #'eq)

(a:define-constant +meta-topic-values-delimiters+ "," :test #'string=)

(a:define-constant +meta-date-key+ :date :test #'eq)

(a:define-constant +meta-key-value-delimiter+ ": *" :test #'string=)

(a:define-constant +archive-dir+ "archive" :test #'string=)

(a:define-constant +archive-gemlog-file+ "gemlog.gmi" :test #'string=)

(a:define-constant +archive-topic-file+  "topics.gmi" :test #'string=)

(defparameter *gemlog-header*  (format nil "# Posts~2%## Il gemlog di cage~2%"))

(defparameter *topic-index-header*  (format nil "# Topics archive~2%"))

(defparameter *uri-prefix-path*  "/")

(defparameter *post-home-backlink*  "../index.gmi")

(defparameter *post-home-backlink-name*  "home")

(defparameter *indices-home-backlink*  "./index.gmi")

(defparameter *indices-home-backlink-name*  "home")

(defun parse-date (timestring)
  (local-time:parse-timestring timestring))

(defun format-date-to-string (date)
  (with-output-to-string (stream)
    (local-time:format-timestring stream date
                                  :format '((:year 2) "-" (:month 2) "-" (:day 2)))))

(defun meta-node-p (node)
  (a:when-let ((attribute (find-attribute :alt node)))
    (string= (trim-blanks (attribute-value attribute))
             "META")))

(defun extract-meta (parsed)
  (let ((lines (mapcar (lambda (node)
                         (trim-blanks (first (children node))))
                       (remove-if-not #'meta-node-p parsed))))
    (loop for line in (rest lines)
          when line
            collect
            (let* ((key-value (mapcar #'trim-blanks
                                      (cl-ppcre:split +meta-key-value-delimiter+ line)))
                   (key       (a:make-keyword (string-upcase (first key-value))))
                   (raw-value (second key-value)))
              (cons key
                    (ecase key
                      (:topic
                       (list (mapcar #'trim-blanks
                                     (cl-ppcre:split +meta-topic-values-delimiters+
                                                     raw-value))))
                      (:date
                       (parse-date raw-value))))))))

(defun extract-non-meta (parsed)
  (remove-if #'meta-node-p parsed))

(defclass post ()
  ((meta
    :initform nil
    :initarg  :meta
    :accessor meta)
   (content
    :initform nil
    :initarg  :content
    :accessor content)
   (original-file-path
    :initform nil
    :initarg  :original-file-path
    :accessor original-file-path)
   (archive-file-path
    :initform nil
    :initarg  :archive-file-path
    :accessor archive-file-path)))

(defmethod print-object ((object post) stream)
  (with-accessors ((meta    meta)
                   (content content)
                   (original-file-path original-file-path)) object
    (print-unreadable-object (object stream :type t)
      (format stream
              "~a ~a ~a~%"
              meta
              content
              original-file-path))))

(defun notify (control &rest args)
  (if (ui:tui-active-p)
      (ui:info-message (apply #'format nil control args))
      (apply #'format t control args)))

(defun bulk->posts (capsule-bulk-dir)
  (let* ((original-post-files (remove-if-not #'fs:regular-file-p
                                             (fs:collect-children capsule-bulk-dir)))
         (parsed-posts        (mapcar (lambda (a)
                                        (handler-case
                                            (parse-gemini-file (fs:slurp-file a))
                                          (error (e)
                                            (notify
                                                    "Unable to parse ~a: ~a"
                                                    a e)
                                            nil)))
                                      original-post-files))
         (all-meta            (mapcar (lambda (a)
                                        (handler-case
                                            (let ((meta (extract-meta a)))
                                              meta)
                                          (error (e)
                                            (notify
                                                    "Unable to parse meta ~a: ~a"
                                                    a e)
                                            nil)))
                                      parsed-posts)))
    (loop for original-post-file in original-post-files
          for parsed-post in parsed-posts
          for meta in all-meta
          when (and parsed-post meta)
            collect
            (make-instance 'post
                           :original-file-path original-post-file
                           :content            (extract-non-meta parsed-post)
                           :meta               meta))))

(defun post-topics (post)
  (with-accessors ((meta meta)) post
    (cadr (assoc +meta-topic-key+ meta))))

(defun collect-topics (posts)
  (let ((results '()))
    (loop for post in posts do
      (with-accessors ((meta meta)) post
        (a:when-let ((topics (post-topics post)))
          (loop for topic in topics do
            (pushnew topic results :test #'string-equal)))))
    results))

(defun post-date (post)
  (with-accessors ((meta meta)) post
    (cdr (assoc +meta-date-key+ meta))))

(defun sexp->gmi (parsed stream)
  (loop for node in parsed do
    (let ((line (cond
                  ((tag= :h1 node)
                   (geminize-h1 (first (children node))))
                  ((tag= :h2 node)
                   (geminize-h2 (first (children node))))
                  ((tag= :h3 node)
                   (geminize-h3 (first (children node))))
                  ((tag= :li node)
                   (geminize-list (first (children node))))
                  ((tag= :quote node)
                   (geminize-quote (first (children node))))
                  ((tag= :a node)
                   (geminize-link (strcat " "
                                          (attribute-value (find-attribute :href node))
                                          " "
                                          (first (children node)))))
                  ((or (tag= :pre node)
                       (tag= :pre-end node))
                   "```")
                  ((null (first (children node)))
                   (format nil "~%"))
                  (t
                   (first (children node))))))
      (format stream "~a~%" line))))

(defun create-archive (bulk-posts-dir output-directory)
  (let* ((posts            (sort (bulk->posts bulk-posts-dir)
                                 (lambda (a b)
                                   (local-time:timestamp> (post-date a)
                                                          (post-date b)))))
         (all-topics       (collect-topics posts))
         (archive-dir-path (strcat output-directory "/" +archive-dir+)))
    (fs:make-directory archive-dir-path)
    (loop for post in posts do
      (let* ((file-name (fs:strip-dirs-from-path (original-file-path post)))
             (file-path (fs:cat-parent-dir archive-dir-path file-name)))
        (setf (archive-file-path post) file-path)
        (handler-case
            (with-open-file (stream file-path
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists         :supersede)
              (sexp->gmi (content post) stream)
              (write-sequence (geminize-link (strcat *post-home-backlink*
                                                     " "
                                                     *post-home-backlink-name*))
                              stream)
              (notify "Processed ~a~%" (original-file-path post)))
          (error (e)
            (format *error-output*
                    "skipping ~a, reasons: ~a.~%"
                    file-path
                    e)))))
    (values posts all-topics)))

(defun write-links (posts stream)
  (loop for post in posts do
    (let* ((filename              (strip-dirs-from-path (archive-file-path post)))
           (relative-archive-path (strcat *uri-prefix-path*
                                          (cat-parent-dir +archive-dir+
                                                          (percent-encode filename))))
           (link-text             (strcat (format-date-to-string (post-date post))
                                          " "
                                          (cl-ppcre:regex-replace-all "-"
                                                                      filename
                                                                      " ")))
           (link (geminize-link (format nil
                                        " ~a ~a~%"
                                        relative-archive-path
                                        link-text))))
      (write-sequence link stream))))

(defun make-gemlog-index (all-posts output-directory)
  (let ((gemlog-index-path (cat-parent-dir output-directory +archive-gemlog-file+)))
    (with-open-file (gemlog-stream
                     gemlog-index-path
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists         :supersede)
      (format gemlog-stream *gemlog-header*)
      (write-links all-posts gemlog-stream)
      (format gemlog-stream "~%")
      (write-sequence (geminize-link (strcat *indices-home-backlink*
                                               " "
                                               *indices-home-backlink-name*))
                        gemlog-stream))))

(defun make-topic-index (all-posts output-directory all-topics)
  (let ((topics-index-path (cat-parent-dir output-directory +archive-topic-file+)))
    (with-open-file (stream
                     topics-index-path
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists         :supersede)
      (write-sequence *topic-index-header* stream)
      (loop for topic in all-topics do
        (format stream "~a~2%" (geminize-h2 topic))
        (let ((in-topic-posts (remove-if-not (lambda (post)
                                               (let ((post-topics (post-topics post)))
                                                 (find topic
                                                       post-topics
                                                       :test #'string-equal)))
                                             all-posts)))
          (write-links in-topic-posts stream)
          (format stream "~%")))
      (write-sequence (geminize-link (strcat *indices-home-backlink*
                                             " "
                                             *indices-home-backlink-name*))
                      stream))))

(defun generate-gemlog (bulk-posts-dir output-directory)
  (multiple-value-bind (all-posts all-topics)
      (create-archive bulk-posts-dir output-directory)
    (make-topic-index all-posts output-directory all-topics)
    (make-gemlog-index all-posts output-directory)))

(a:define-constant +bulk-dir-prompt+ "Original posts directory? " :test #'string=)

(a:define-constant +output-dir-prompt+ "Output directory? " :test #'string=)

(a:define-constant +root-dir-prompt+ "Type the root of the path for the gemlog (e.g: \"gemini://foo.net/cage/\" → \"/cage/\") " :test #'string=)

(defun generate-on-tui ()
  (let ((bulk-posts-dir   nil)
        (output-directory nil))
    (format t "Starting processing~%")
    (labels ((on-bulk-complete (input-text)
               (ui::with-enqueued-process ()
                 (setf bulk-posts-dir input-text)
                 (ui:ask-string-input #'on-out-complete
                                      :prompt +output-dir-prompt+
                                      :complete-fn #'complete:directory-complete)))
             (on-out-complete (out-directory)
               (ui::with-enqueued-process ()
                 (setf output-directory out-directory)
                 (ui:ask-string-input #'on-root-completed
                                      :prompt +root-dir-prompt+)))
             (on-root-completed (root)
               (ui::with-enqueued-process ()
                 (setf *uri-prefix-path* root)
                 (generate-gemlog bulk-posts-dir output-directory)
                 (notify "Gemlog generated~%")
                 (ui:open-gemini-address output-directory))))
      (ui:ask-string-input #'on-bulk-complete
                           :prompt +bulk-dir-prompt+
                           :complete-fn #'complete:directory-complete))))

(defun ask-input-cli (prompt)
  (notify "~a~%" prompt)
  (finish-output)
  (read-line))

(if (ui:tui-active-p)
    (generate-on-tui)
    (let* ((bulk-posts-dir   (ask-input-cli +bulk-dir-prompt+))
           (output-directory (ask-input-cli +output-dir-prompt+)))
      (setf *uri-prefix-path* (ask-input-cli +root-dir-prompt+))
      (generate-gemlog bulk-posts-dir output-directory)))
