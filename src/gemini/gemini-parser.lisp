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

(in-package :gemini-parser)

(defparameter *raw-mode* nil)

(define-constant +h1-prefix+           "#"   :test #'string=)

(define-constant +h2-prefix+           "##"  :test #'string=)

(define-constant +h3-prefix+           "###" :test #'string=)

(define-constant +list-bullet-prefix+  "* "  :test #'string=)

(define-constant +quote-prefix+        ">"   :test #'string=)

(define-constant +preformatted-prefix+ "```" :test #'string=)

(define-constant +link-prefix+         "=>"  :test #'string=)

(defmacro gen-geminize-line (name prefix)
  `(defun ,(format-fn-symbol t "geminize-~a" name) (text)
     (strcat ,prefix text)))

(gen-geminize-line h1 +h1-prefix+)

(gen-geminize-line h2 +h2-prefix+)

(gen-geminize-line h3 +h3-prefix+)

(gen-geminize-line list +list-bullet-prefix+)

(gen-geminize-line quote +quote-prefix+)

(gen-geminize-line link +link-prefix+)

(defun geminize-preformatted (text)
  (format nil "~a~%~a~a~%"
          +preformatted-prefix+
          text
          +preformatted-prefix+))

(defun make-gemini-link (url title)
  (format nil "~a ~a"
          (geminize-link url)
          title))

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
               (let ((saved-mode *raw-mode*))
                 (setf *raw-mode* (not *raw-mode*))
                 (when (not saved-mode)
                   (list :pre
                         (list (list :alt (coerce (second a) 'string)))))))))

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
                           (? link-name)))
                   cr-lf)
  (:function (lambda (a)
               (list (first a)
                     (list (list :href (second a)))
                     (text-utils:trim-blanks (second (third a)))))))

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
                            preformatted-text-tag
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

(defun absolutize-link (link-value original-host original-port original-path)
  (let ((parsed (or (ignore-errors (iri:iri-parse link-value))
                    (iri:make-iri nil nil nil nil link-value nil nil))))
    (cond
      ((null (uri:host parsed))
       (let* ((absolute-path-p (string-starts-with-p "/" link-value))
              (path            (if absolute-path-p
                                   (uri:path parsed)
                                   (strcat (if original-path
                                               (path-last-dir original-path)
                                               "/")
                                           (uri:path parsed)))))
         (make-gemini-iri original-host
                          (uri:normalize-path path)
                          :query    (uri:query parsed)
                          :port     original-port
                          :fragment (uri:fragment parsed))))
      ((null (uri:scheme parsed))
       (strcat +gemini-scheme+ ":"
               (to-s (uri:normalize-path parsed))))
      (t
       (to-s (uri:normalize-path parsed))))))

(defun make-gemini-iri (host path &key
                                    (query    nil)
                                    (port     +gemini-default-port+)
                                    (fragment nil)
                                    (scheme  +gemini-scheme+))
  (let* ((actual-path (if (string-starts-with-p "/" path)
                          (subseq path 1)
                          path))
         (actual-port (if port
                          (to-s port)
                          (to-s +gemini-default-port+)))
         (iri (strcat scheme         "://"
                      host            ":"
                      actual-port     "/"
                      actual-path)))
    (when query
      (setf iri (strcat iri "?" query)))
    (when fragment
      (setf iri (strcat iri "#" fragment)))
    iri))

(defun sexp->links (parsed-gemini original-host original-port original-path
                    &key (comes-from-local-file nil))
  (loop
    for node in parsed-gemini
    when (html-utils:tag= :a node)
      collect
      (let* ((link-value    (html-utils:node->link node))
             (absolute-p    (iri:absolute-url-p link-value))
             (rendered-link (cond
                                  (absolute-p
                                   link-value)
                                  (comes-from-local-file
                                   (strcat original-path
                                           iri:+segment-separator+
                                           link-value))
                                  (t
                                   (absolutize-link link-value
                                                    original-host
                                                    original-port
                                                    original-path)))))
            (make-instance 'gemini-link
                           :target rendered-link
                           :name   (tag-value node)))))

(defun gemini-link-iri-p (iri)
  (conditions:with-default-on-error (nil)
    (or (text-utils:string-starts-with-p +gemini-scheme+ iri)
        (null (uri:scheme (iri:iri-parse iri))))))

(defclass gemini-page-theme ()
  ((link-prefix-gemini
    :initarg  :link-prefix-gemini
    :initform "-> "
    :accessor link-prefix-gemini)
   (link-prefix-other
    :initarg  :link-prefix-other
    :initform "^ "
    :accessor link-prefix-other)
   (h1-prefix
    :initarg  :h1-prefix
    :initform "+ "
    :accessor h1-prefix)
   (h2-prefix
    :initarg  :h2-prefix
    :initform "+ "
    :accessor h2-prefix)
   (h3-prefix
    :initarg  :h3-prefix
    :initform "+ "
    :accessor h3-prefix)
   (quote-prefix
    :initarg  :quote-prefix
    :initform +quote-line-prefix+
    :accessor quote-prefix)
   (bullet-prefix
    :initarg  :bullet-prefix
    :initform +bullet-line-prefix+
    :accessor bullet-prefix)
   (viewport
    :initarg  :viewport
    :initform nil
    :accessor viewport)))

(defun sexp->text (parsed-gemini theme)
  (labels ((header-prefix (prefix header)
             (strcat prefix header))
           (header-prefix-h1 (header)
             (header-prefix (h1-prefix theme) header))
           (header-prefix-h2 (header)
             (header-prefix (h2-prefix theme) header))
           (header-prefix-h3 (header)
             (header-prefix (h3-prefix theme) header))
           (underlineize (stream text underline-char)
             (let* ((size      (length text))
                    (underline (build-string size underline-char)))
               (format stream "~a~%~a~%" text underline)))
           (trim (a)
             (string-trim '(#\Newline #\Return) a))
           (text-value (node &key (trim t))
             (let ((text (first (html-utils:children node))))
               (if trim
                   (trim text)
                   text)))
           (linkify (link-name link-value)
             (if (gemini-link-iri-p link-value)
                 (format nil "~a~a~%" (link-prefix-gemini theme) link-name)
                 (format nil "~a~a~%" (link-prefix-other  theme) link-name)))
           (fit-quote-lines (line win-width)
             (join-with-strings (mapcar (lambda (a) (strcat (quote-prefix theme) a))
                                        (flush-left-mono-text (split-words line)
                                                              (- win-width
                                                                 (length (quote-prefix theme)))))
                                (format nil "~%"))))
    (let ((win-width (message-window:viewport-width (viewport theme))))
      (with-output-to-string (stream)
        (loop for node in parsed-gemini do
          (cond
            ((null node)
             (format stream "~%"))
            ((html-utils:tag= :as-is node)
             (format stream "~a~%" (text-value node)))
            ((html-utils:tag= :text node)
             (format stream "~a~%" (text-value node)))
            ((html-utils:tag= :h1 node)
             (underlineize stream
                           (header-prefix-h1 (text-value node))
                           +h1-underline+))
            ((html-utils:tag= :h2 node)
             (underlineize stream
                           (header-prefix-h2 (text-value node))
                           +h2-underline+))
            ((html-utils:tag= :h3 node)
             (underlineize stream
                           (header-prefix-h3 (text-value node))
                           +h3-underline+))
            ((html-utils:tag= :li node)
             (format stream
                     "~a ~a~%"
                     (bullet-prefix theme)
                     (text-value node)))
            ((html-utils:tag= :quote node)
             (write-sequence (fit-quote-lines (text-value node :trim nil)
                                              win-width)
                             stream))
            ((html-utils:tag= :pre node)
             (write-sequence (text-value node :trim nil) stream))
            ((html-utils:tag= :a node)
             (let ((link-name  (text-value node :trim nil))
                   (link-value (html-utils:attribute-value (html-utils:find-attribute :href
                                                                                      node))))
               (if link-name
                   (write-string (linkify link-name link-value)  stream)
                   (write-string (linkify link-value link-value) stream))))))))))

(defun parse-gemini-file (data)
  (let* ((was-raw-mode *raw-mode*)
         (actual-data  (if (and (string-not-empty-p data)
                                (char/= (last-elt data) #\Newline))
                           (strcat data (string #\Newline))
                           data))
         (parsed       (parse 'gemini-file actual-data :junk-allowed t)))
    (if was-raw-mode
        (if *raw-mode*
            (list (html-utils:make-tag-node :as-is nil data))
            nil)
        parsed)))

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

(defun gemini-iri-p (maybe-iri)
  (conditions:with-default-on-error (nil)
    (let ((parsed (iri:iri-parse maybe-iri)))
      (and parsed
           (string-equal +gemini-scheme+
                         (uri:scheme parsed))
           (uri:host parsed)))))
