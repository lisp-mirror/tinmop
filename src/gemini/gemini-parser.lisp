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

(in-package :gemini-parser)

(defrule space (or #\Space #\Tab)
  (:constant nil))

(defrule new-line  #\Newline
  (:constant nil))

(defrule carriage-return  #\Return
  (:constant nil))

(defrule cr-lf (and (? carriage-return) new-line)
  (:constant nil))

(defrule h1-prefix "#"
  (:constant :h1))

(defrule h2-prefix "##"
  (:constant :h2))

(defrule h3-prefix "###"
  (:constant :h3))

(defrule list-bullet "* "
  (:constant :li))

(defrule quote-prefix ">"
  (:constant :quote))

(defrule preformatted-text-tag (and "```"
                                    (* (not cr-lf))
                                    cr-lf)
  (:function (lambda (a)
               (list :pre
                     (list (list :alt (coerce (second a) 'string)))))))


(defrule link-prefix (and "=>"
                          (* space))
  (:constant :a))

(defrule text-line (and (+ (not cr-lf)) cr-lf)
  (:function (lambda (a)
               (list :text
                     nil
                     (coerce (first a) 'string)))))

(defrule link-url (+ (not (or space
                              cr-lf)))
  (:text t))

(defrule link-name (+ (not cr-lf))
  (:text t))

(defrule link (and link-prefix
                   link-url
                   (? (and space
                           link-name))
                   cr-lf)
  (:function (lambda (a)
               (list (first a)
                     (list (list :href (second a)))
                     (second (third a))))))

(defrule h1 (and h1-prefix
                 text-line)
  (:function (lambda (a)
               (list (first a)
                     nil
                     (tag-value (second a))))))

(defrule h2 (and h2-prefix
                 text-line)
  (:function (lambda (a)
               (list (first a)
                     nil
                     (tag-value (second a))))))

(defrule h3 (and h3-prefix
                 text-line)
  (:function (lambda (a)
               (list (first a)
                     nil
                     (tag-value (second a))))))

(defrule list-item (and list-bullet
                        text-line)
  (:function (lambda (a)
               (list (first a)
                     nil
                     (tag-value (second a))))))

(defrule preformatted-text (and preformatted-text-tag
                                (* (not preformatted-text-tag))
                                preformatted-text-tag)
  (:function (lambda (a) (append (first a)
                                 (list (coerce (second a) 'string))))))

(defrule quote-line (and quote-prefix
                         text-line)
  (:function (lambda (a) (list (first a)
                               nil
                               (tag-value (second a))))))

(defrule gemini-file (* (or h3
                            h2
                            h1
                            preformatted-text
                            link
                            list-item
                            quote-line
                            text-line
                            cr-lf)))


(define-constant +h1-underline+       #\━  :test #'char=)

(define-constant +h2-underline+       #\─  :test #'char=)

(define-constant +h3-underline+       #\-   :test #'char=)

(define-constant +quote-line-prefix+  #\>  :test #'char=)

(define-constant +bullet-line-prefix+ #\•  :test #'char=)

(defclass gemini-link ()
  ((target
    :initform nil
    :initarg  :target
    :accessor target)
   (name
    :initform nil
    :initarg  :name
    :accessor name)))

(defmethod print-object ((object gemini-link) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-accessors ((target target)
                     (name   name)) object
      (format stream "target: ~s name: ~s" target name))))

(defun tag-value (node)
  (first (html-utils:children node)))

(defun path-last-dir (path)
  (if (char= (last-elt path) #\/)
      path
      (fs:parent-dir-path path)))

(defun make-gemini-uri (host path &optional (query nil) (port +gemini-default-port+))
  (let* ((actual-path (if (string-starts-with-p "/" path)
                          (subseq path 1)
                          path))
         (actual-port (if port
                          (to-s port)
                          (to-s +gemini-default-port+)))
         (uri (strcat +gemini-scheme+ "://"
                      host            ":"
                      actual-port     "/"
                      actual-path)))
    (when query
      (setf uri (strcat uri "?" query)))
    uri))

(defgeneric normalize-path (object))

(defmethod normalize-path ((object null))
  nil)

(defmethod normalize-path ((object string))
  (flet ((make-stack ()
           (make-instance 'stack:stack
                          :test-fn #'string=))
         (fill-input-stack (stack)
           (loop
              for segment in (remove-if #'string-empty-p
                                        (reverse (split "/" object)))
              do
                (stack:stack-push stack segment))))
    (let* ((ends-with-separator-p (string-ends-with-p "/" object))
           (ends-with-dots        nil)
           (input-stack  (make-stack))
           (output-stack (make-stack)))
      (fill-input-stack input-stack)
      (labels ((fill-output-buffer ()
                 (when (not (stack:stack-empty-p input-stack))
                   (let ((popped (stack:stack-pop input-stack)))
                     (cond
                       ((and (string= popped "..")
                             (not (stack:stack-empty-p output-stack))
                             (not (stack:stack-empty-p input-stack)))
                        (stack:stack-pop output-stack))
                       ((and (or (string= popped "..")
                                 (string= popped "."))
                             (stack:stack-empty-p input-stack))
                        (setf ends-with-dots t))
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
                                 (wrap-with (join-with-strings joinable "/") "/")
                                 (strcat "/" (join-with-strings joinable "/")))
                             "/")))
          (regex-replace-all "//" merged ""))))))

(defmethod normalize-path ((object puri:uri))
  (let ((clean-path (normalize-path (puri:uri-path object)))
        (copy       (puri:copy-uri  object)))
    (when clean-path
      (setf (puri:uri-path copy) clean-path))
    copy))

(defmethod to-s ((object puri:uri))
  (with-output-to-string (stream)
    (puri:render-uri object stream)))

(defun absolutize-link (link-value original-host original-port original-path)
  (let ((parsed (puri:parse-uri link-value)))
    (cond
      ((null parsed)
       (error "Unparsable address"))
      ((null (puri:uri-host parsed))
       (let* ((absolute-path-p (string-starts-with-p "/" link-value))
              (path            (if absolute-path-p
                                   link-value
                                   (strcat (path-last-dir original-path)
                                           link-value))))
         (make-gemini-uri original-host
                          (normalize-path path)
                          nil
                          original-port)))
      ((null (puri:uri-scheme parsed))
       (strcat +gemini-scheme+ ":"
               (to-s (normalize-path parsed))))
      (t
       (to-s (normalize-path parsed))))))

(defun sexp->links (parsed-gemini original-host original-port original-path)
  (loop for node in parsed-gemini when (html-utils:tag= :a node) collect
       (let ((link-value (html-utils:attribute-value (html-utils:find-attribute :href node))))
         (make-instance 'gemini-link
                        :target (absolutize-link link-value
                                                 original-host
                                                 original-port
                                                 original-path)
                        :name   (tag-value node)))))

(defun sexp->text (parsed-gemini)
  (labels ((underlineize (stream text underline-char)
             (let* ((size      (length text))
                    (underline (build-string size underline-char)))
               (format stream "~a~%~a~%" text underline)))
           (trim (a)
             (string-trim '(#\Newline #\Return) a))
           (text-value (node &key (trim t))
             (let ((text (first (html-utils:children node))))
               (if trim
                   (trim text)
                   text))))
    (with-output-to-string (stream)
      (loop for node in parsed-gemini do
           (cond
             ((null node)
              (format stream "~%"))
             ((html-utils:tag= :text node)
              (format stream "~a~%" (text-value node)))
             ((html-utils:tag= :h1 node)
              (underlineize stream
                            (text-value node)
                            +h1-underline+))
             ((html-utils:tag= :h2 node)
              (underlineize stream
                            (text-value node)
                            +h2-underline+))
             ((html-utils:tag= :h1 node)
              (underlineize stream
                            (text-value node)
                            +h3-underline+))
             ((html-utils:tag= :li node)
              (format stream
                      "~a ~a~%"
                      +bullet-line-prefix+
                      (text-value node)))
             ((html-utils:tag= :quote node)
              (format stream
                      "~a ~a~%"
                      +quote-line-prefix+
                      (text-value node)))
             ((html-utils:tag= :pre node)
              (princ (text-value node)
                     stream))
             ((html-utils:tag= :a node)
              (let ((link-name  (text-value node :trim nil))
                    (link-value (html-utils:attribute-value (html-utils:find-attribute :href
                                                                                       node))))
                (if link-name
                    (format stream "[~a]~%" link-name)
                    (format stream "[~a]~%" link-value)))))))))

(defun parse-gemini-file (data)
  (parse 'gemini-file (strcat data (string #\Newline))
         :junk-allowed t))

;; response header

(define-constant +max-meta-length+ 1024 :test #'=)

(defrule response-first-digit (or "1" "2" "3" "4" "5" "6")
  (:text t))

(defrule response-second-digit (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
  (:text t))

(defrule meta (+ (not carriage-return))
  (:text t))

(defclass gemini-response ()
  ((status-code
    :initform nil
    :initarg  :status-code
    :accessor status-code)
   (meta
    :initarg  :meta
    :accessor meta)))

(defmethod print-object ((object gemini-response) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-accessors ((status-code status-code)
                     (meta        meta)) object
      (format stream "status: ~a meta: ~a" status-code meta))))

(defrule response (and response-first-digit
                       response-second-digit
                       space
                       meta
                       carriage-return
                       new-line)
  (:function (lambda (a)
               (make-instance 'gemini-response
                              :status-code (parse-integer (strcat (first a)
                                                                  (second a)))
                              :meta        (fourth a)))))

(defun parse-gemini-response-header (data)
  (let ((parsed (parse 'response data)))
    (if (> (length (meta parsed))
           +max-meta-length+)
        (error 'conditions:length-error
               :seq  (meta parsed)
               :text (format nil
                             " is too long. Maximum allowed length is ~a"
                             +max-meta-length+))
        parsed)))
