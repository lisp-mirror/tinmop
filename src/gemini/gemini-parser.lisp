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

(defparameter *raw-mode-data* nil)

(defparameter *parser-lock* (bt:make-recursive-lock))

(defparameter *pre-group-id* -1)

(defparameter *header-group-id* -1)

(defparameter *source-line-id* -1)

(defparameter *pre-alt-text*    "")

(defun-w-lock next-pre-group-id () *parser-lock*
  (incf *pre-group-id*)
  *pre-group-id*)

(defun-w-lock current-pre-group-id () *parser-lock*
  *pre-group-id*)

(defun-w-lock next-header-group-id () *parser-lock*
  (incf *header-group-id*)
  *header-group-id*)

(defun-w-lock current-header-group-id () *parser-lock*
  *header-group-id*)

(defun-w-lock set-pre-alt-text (text) *parser-lock*
  (setf *pre-alt-text* text))

(defun-w-lock current-pre-alt-text () *parser-lock*
  *pre-alt-text*)

(defun-w-lock next-source-line-id () *parser-lock*
  (incf *source-line-id*)
  *source-line-id*)

(defun-w-lock current-source-line-id () *parser-lock*
  *source-line-id*)

(defparameter *omitted-port* +gemini-default-port+)

(define-constant +h1-prefix+           "#"   :test #'string=)

(define-constant +h2-prefix+           "##"  :test #'string=)

(define-constant +h3-prefix+           "###" :test #'string=)

(define-constant +list-bullet-prefix+  "* "  :test #'string=)

(define-constant +quote-prefix+        ">"   :test #'string=)

(define-constant +preformatted-prefix+ "```" :test #'string=)

(define-constant +link-prefix+         "=>"  :test #'string=)

(define-constant +max-header-level+    3     :test #'=)

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

(defun render-gemini-link (url title)
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
               (let ((saved-raw-mode *raw-mode-data*)
                     (alt-text       (coerce (second a) 'string)))
                 (if *raw-mode-data*
                     (setf *raw-mode-data* nil)
                     (setf *raw-mode-data* alt-text))
                 (if (not saved-raw-mode)
                     (list :pre
                           (list (list :alt alt-text)))
                     (list :pre-end
                           (list (list :alt saved-raw-mode))))))))

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
                         (? text-line))
  (:function (lambda (a) (list (first a)
                               nil
                               (if (second a)
                                   (tag-value (second a))
                                   "")))))

(defrule gemini-file (* (or h3
                            h2
                            h1
                            preformatted-text-tag
                            link
                            list-item
                            quote-line
                            text-line
                            cr-lf))
  (:function first))

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

(defun make-gemini-link (target link-name)
  (make-instance 'gemini-link :target target :name link-name))

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

(defun absolutize-link (link-value original-host original-port original-path original-query)
  (let ((parsed (or (ignore-errors (iri:iri-parse link-value))
                    (iri:make-iri nil nil nil nil link-value nil nil))))
    (cond
      ((null (uri:host parsed))
       (let* ((absolute-path-p      (string-starts-with-p "/" link-value))
              (query-path-p         (uri:query parsed))
              (path                (cond
                                     (absolute-path-p
                                      (uri:path parsed))
                                     ((and query-path-p
                                           original-query)
                                      (strcat (safe-all-but-last-elt original-path)
                                              (uri:path parsed)))
                                     ((or query-path-p
                                          original-query)
                                      (strcat original-path
                                              (uri:path parsed)))
                                     (t
                                      (strcat (if original-path
                                                  (path-last-dir original-path)
                                                  "/")
                                              (uri:path parsed))))))
         (make-gemini-iri original-host
                          (fs:normalize-path path)
                          :query    (uri:query parsed)
                          :port     original-port
                          :fragment (uri:fragment parsed))))
      ((null (uri:scheme parsed))
       (strcat +gemini-scheme+ ":"
               (to-s (fs:normalize-path parsed))))
      (t
       (to-s (fs:normalize-path parsed))))))

(defun make-gemini-iri (host path &key
                                    (query    nil)
                                    (port     +gemini-default-port+)
                                    (fragment nil)
                                    (scheme  +gemini-scheme+)
                                    (omit-default-port t)
                                    (default-port *omitted-port*)
                                    (user-info    nil))
  (let* ((actual-path              (if (string-starts-with-p "/" path)
                                       (subseq path 1)
                                       path))
         (actual-port              (cond
                                     ((null port)
                                      "")
                                     ((and omit-default-port
                                           (= port default-port))
                                      "")
                                     (t
                                      (to-s port))))
         (domain-port-separator    (if (string-not-empty-p actual-port)
                                       ":"
                                       ""))
         (actual-host              (if (iri:ipv6-address-p host)
                                       (strcat "[" host "]")
                                       host))
         (actual-user-info         (if (string-not-empty-p user-info)
                                       user-info
                                       ""))
         (user-info-host-separator (if (string-not-empty-p user-info)
                                       "@"
                                       ""))

         (iri (strcat scheme         "://"
                      actual-user-info user-info-host-separator actual-host
                      domain-port-separator actual-port     "/"
                      actual-path)))
    (when query
      (setf iri (strcat iri "?" query)))
    (when fragment
      (setf iri (strcat iri "#" fragment)))
    iri))

(defun sexp->links (parsed-gemini original-host original-port original-path original-query
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
                                                    original-path
                                                    original-query)))))
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
   (link-prefix-http
    :initarg  :link-prefix-http
    :initform "-> "
    :accessor link-prefix-http)
   (link-prefix-other
    :initarg  :link-prefix-other
    :initform "^ "
    :accessor link-prefix-other)
   (link-bg
    :initarg  :link-bg
    :initform :red
    :accessor link-bg)
   (link-fg
    :initarg  :link-fg
    :initform :yellow
    :accessor link-fg)
   (link-attributes
    :initarg  :link-attributes
    :initform (tui:attribute-underline)
    :accessor link-attributes)
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
    :initform +quote-prefix+
    :accessor quote-prefix)
   (bullet-prefix
    :initarg  :bullet-prefix
    :initform "@ "
    :accessor bullet-prefix)
   (preformatted-fg
    :initarg  :preformatted-fg
    :initform :red
    :accessor preformatted-fg)
   (viewport
    :initarg  :viewport
    :initform nil
    :accessor viewport)))

(defclass with-source-line ()
  ((source-line
    :initform nil
    :initarg  :source-line
    :accessor source-line)
   (source-line-id
    :initform nil
    :initarg  :source-line-id
    :accessor source-line-id)
   (artificial
    :initform nil
    :initarg  :artificial
    :reader   artificialp
    :writer   (setf artificial))))

(defmethod print-object ((object with-source-line) stream)
  (format stream "sid: ~a source-line: ~a" (source-line-id object) (source-line object)))

(defclass with-group-id ()
  ((group-id
    :initform nil
    :initarg :group-id
    :accessor group-id)))

(defclass with-lines ()
  ((lines
    :initform ()
    :initarg :lines
    :accessor lines)))

(defclass with-alt-text ()
  ((alt-text
    :initform nil
    :initarg :alt-text
    :accessor alt-text)))

(defclass with-pre-group-id ()
  ((pre-group-id
    :initform nil
    :initarg  :pre-group-id
    :accessor pre-group-id)))

(defclass pre-start (with-group-id with-pre-group-id with-alt-text with-source-line) ())

(defmethod print-object ((object pre-start) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "gid: ~a alt ~a" (group-id object) (alt-text object))))

(defun make-pre-start (alt-text group-id pre-group-id fg)
  (make-instance 'pre-start
                 :alt-text     (tui:make-tui-string alt-text :fgcolor fg)
                 :group-id     group-id
                 :pre-group-id pre-group-id))

(defclass pre-end (with-source-line) ())

(defun make-pre-end ()
  (make-instance 'pre-end))

(defclass quoted-lines (with-group-id with-lines with-source-line)
  ((prefix
    :initform "@ "
    :initarg  :prefix
    :accessor prefix)))

(defun make-quoted-lines (text group-id prefix)
  (make-instance 'quoted-lines
                 :prefix   prefix
                 :group-id group-id
                 :lines    (list text)))

(defclass pre-line (with-group-id with-pre-group-id with-lines with-alt-text with-source-line) ())

(defmethod print-object ((object pre-line) stream)
  (print-unreadable-object (object stream :type t)
    (format stream
            "gid: ~a alt ~a lines ~a"
            (group-id object)
            (alt-text object)
            (lines object))))

(defun make-pre-line (lines group-id pre-group-id alt-text)
  (make-instance 'pre-line
                 :lines        lines
                 :group-id     group-id
                 :pre-group-id pre-group-id
                 :alt-text     alt-text))

(defclass vertical-space (with-group-id with-source-line)
  ((size
    :initform 1
    :initarg  :size
    :accessor size)))

(defclass header-line (with-group-id with-lines with-source-line)
  ((level
    :initform nil
    :initarg :level
    :accessor level)))

(defun make-header-line (text gid level artificial)
  (make-instance 'header-line
                 :lines      (list text)
                 :group-id   gid
                 :level      level
                 :artificial artificial))

(defclass unordered-list-line (with-group-id with-lines with-source-line) ())

(defun make-unordered-list-line (text header-group-id)
  (make-instance 'unordered-list-line
                 :group-id header-group-id
                 :lines    (list text)))

(defclass link-line (with-group-id with-source-line)
  ((link-text
    :initarg  :link-text
    :initform nil
    :accessor link-text)
   (link-name
    :initarg  :link-name
    :initform nil
    :accessor link-name)
   (link-value
    :initarg  :link-value
    :initform nil
    :accessor link-value)))

(defun make-link-line (link-text link-name link-value group-id)
  (make-instance 'link-line
                 :group-id   group-id
                 :link-text  link-text
                 :link-name  link-name
                 :link-value link-value))

(defclass simple-line (with-group-id with-source-line)
  ((text-line
    :initarg :text-line
    :initform nil
    :accessor text-line)))

(defun make-simple-line (text header-group-id)
  (make-instance 'simple-line
                 :text-line text
                 :group-id  header-group-id))

(defun sexp->text-rows (parsed-gemini theme)
  (labels ((header-prefix (prefix header)
             (strcat prefix header))
           (header-prefix-h1 (header)
             (header-prefix (h1-prefix theme) header))
           (header-prefix-h2 (header)
             (header-prefix (h2-prefix theme) header))
           (header-prefix-h3 (header)
             (header-prefix (h3-prefix theme) header))
           (build-underline (text underline-char)
             (let* ((size      (length text))
                    (underline (build-string size underline-char)))
               underline))
           (make-header (level text underline-char)
             (let ((underline (build-underline text underline-char))
                   (header-group-id (next-header-group-id)))
               (list (make-header-line text header-group-id level nil)
                     (make-header-line underline header-group-id level t))))
           (extract-source-line (node)
             (html-utils:attribute-value (html-utils:find-attribute :source-line node)))
           (trim (a)
             (trim-blanks a))
           (text-value (node &key (trim t))
             (let ((text (first (html-utils:children node))))
               (if trim
                   (trim text)
                   text)))
           (linkify (link-name link-value)
             (let ((raw-link-text (cond
                                    ((gemini-link-iri-p link-value)
                                     (if (text-utils:starting-emoji link-name)
                                         (format nil
                                                 "~a~a"
                                                 (text-utils:trim-blanks (link-prefix-other theme))
                                                 link-name)
                                         (format nil
                                                 "~a~a"
                                                 (link-prefix-gemini theme)
                                                 link-name)))
                                    ((html-utils::http-link-iri-p link-value)
                                     (format nil
                                             "~a~a"
                                             (link-prefix-http theme)
                                             link-name))
                                    (t
                                     (format nil
                                             "~a~a"
                                             (link-prefix-other theme)
                                             link-name)))))
               (tui:make-tui-string raw-link-text
                                    :attributes (link-attributes theme)
                                    :fgcolor    (link-fg         theme)
                                    :bgcolor    (link-bg         theme))))
           (pre-alt-text (node)
             (trim (html-utils:attribute-value (html-utils:find-attribute :alt node))))
           (make-vertical-space ()
             (make-instance 'vertical-space
                            :group-id (current-header-group-id)))
           (add-source-metadata (thing source-line-id source-line)
             (cond
               ((typep thing 'list)
                (mapcar (lambda (a)
                          (setf (source-line-id a) source-line-id)
                          (setf (source-line    a) source-line)
                          a)
                        thing))
               (t
                (setf (source-line-id thing) source-line-id)
                (setf (source-line    thing) source-line)
                thing)))
           (build-row (node)
             (let ((source-line    (extract-source-line node))
                   (source-line-id (next-source-line-id))
                   (res (cond
                          ((null node)
                           (make-vertical-space))
                          ((html-utils:tag= :as-is node)
                           (let* ((line           (text-value node :trim nil))
                                  (fg             (preformatted-fg theme))
                                  (line           (tui:make-tui-string (format nil "~a" line)
                                                                       :fgcolor fg)))
                             (make-pre-line (list line)
                                            (current-header-group-id)
                                            (current-pre-group-id)
                                            (current-pre-alt-text))))
                          ((html-utils:tag= :text node)
                           (let ((text (text-value node :trim t)))
                             (if (string-not-empty-p text)
                                 (make-simple-line (format nil "~a~%" text)
                                                   (current-header-group-id))
                                 (make-vertical-space))))
                          ((html-utils:tag= :h1 node)
                           (make-header 1
                                        (header-prefix-h1 (text-value node))
                                        +h1-underline+))
                          ((html-utils:tag= :h2 node)
                           (make-header 2
                                        (header-prefix-h2 (text-value node))
                                        +h2-underline+))
                          ((html-utils:tag= :h3 node)
                           (make-header 3
                                        (header-prefix-h3 (text-value node))
                                        +h3-underline+))
                          ((html-utils:tag= :li node)
                           (let* ((text (format nil
                                                "~a ~a"
                                                (bullet-prefix theme)
                                                (text-value node))))
                             (make-unordered-list-line text (current-header-group-id))))
                          ((html-utils:tag= :quote node)
                           (let* ((line         (text-value node :trim nil))
                                  (quote-prefix (quote-prefix theme))
                                  (header-group-id (current-header-group-id)))
                             (make-quoted-lines line header-group-id quote-prefix)))
                          ((html-utils:tag= :pre node)
                           (let ((current-alt-text (pre-alt-text node))
                                 (pre-group-id     (next-pre-group-id))
                                 (current-group-id (current-header-group-id))
                                 (fg               (preformatted-fg theme)))
                             (set-pre-alt-text current-alt-text)
                             (make-pre-start current-alt-text current-group-id pre-group-id fg)))
                          ((html-utils:tag= :pre-end node)
                           (make-pre-end))
                          ((html-utils:tag= :a node)
                           (let* ((link-name       (text-value node :trim nil))
                                  (link-value      (html-utils:attribute-value
                                                    (html-utils:find-attribute :href
                                                                               node)))
                                  (link-text       (if link-name
                                                       (linkify link-name  link-value)
                                                       (linkify link-value link-value)))
                                  (header-group-id (current-header-group-id)))
                             (make-link-line link-text link-name link-value header-group-id))))))
               (add-source-metadata res source-line-id source-line)))
           (build-rows ()
             (flatten (loop for node in parsed-gemini collect (build-row node)))))
    (build-rows)))

(defun parse-gemini-file (data)
  (let* ((lines  (if (string= (format nil "~%") data)
                     (list (format nil "~%"))
                     (mapcar (lambda (a)
                               (strcat a (string #\Newline)))
                             (split-lines data))))
         (parsed (loop for line in lines
                       collect
                       (let ((was-raw-mode *raw-mode-data*)
                             (parsed-line (parse 'gemini-file line :junk-allowed t)))
                         (if was-raw-mode
                             (if *raw-mode-data*
                                 (let ((*blanks* '(#\Newline #\Linefeed #\Return)))
                                   (html-utils:make-tag-node :as-is
                                                             (list (list :alt *raw-mode-data*))
                                                             (trim-blanks line)))
                                 parsed-line)
                             parsed-line)))))
    (mapcar (lambda (a b)
              (when b
                (html-utils:add-attribute :source-line a b)))
            lines parsed)))

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

(defgeneric gemini-first-h1 (data))

(defmethod gemini-first-h1 ((data cons))
  (first (html-utils:children (html-utils:find-tag :h1 data))))

(defmethod gemini-first-h1 ((data string))
  (when-let ((parsed (parse-gemini-file data)))
    (gemini-first-h1 parsed)))

(defmacro with-initialized-parser (&body body)
  `(let ((gemini-parser:*pre-group-id*    -1)
         (gemini-parser:*header-group-id* -1)
         (gemini-parser:*source-line-id*  -1)
         (gemini-parser:*pre-alt-text*    "")
         (gemini-parser:*raw-mode-data*   nil))
     ,@body))
