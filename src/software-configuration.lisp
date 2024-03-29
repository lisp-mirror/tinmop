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

(in-package :software-configuration)

;; CONFIG                      := (ENTRIES)*
;; ENTRIES                     := COMMENT*
;;                               (USE-FILE
;;                                | IGNORE-USER-RE-ASSIGN
;;                                | IGNORE-USER-BOOST-RE-ASSIGN
;;                                | IGNORE-TAG-RE-ASSIGN
;;                                | COLOR-RE-ASSIGN
;;                                | SERVER-ASSIGN
;;                                | USERNAME-ASSIGN
;;                                | OPEN-LINK-HELPER
;;                                | POST-ALLOWED-LANGUAGE
;;                                | GENERIC-ASSIGN)
;;                               COMMENT*
;; SERVER-ASSIGN               := SERVER-KEY BLANKS ASSIGN BLANKS GENERIC-VALUE BLANKS
;; USERNAME-ASSIGN             := USERNAME-KEY BLANKS WITH BLANKS GENERIC-VALUE BLANKS
;; OPEN-LINK-HELPER            := OPEN-LINK-HELPER-KEY BLANKS ASSIGN BLANKS
;;                                REGEXP PROGRAM-NAME BLANKS USE-CACHE? NOWAIT?
;; GENERIC-ASSIGN              := (and key blanks assign blanks
;;                                 (or quoted-string
;;                                     hexcolor
;;                                     colorname
;;                                     generic-value) ; the order in this list *is* important
;;                                 blanks)
;; IGNORE-USER-RE-ASSIGN       := IGNORE-USER-RE-KEY ASSIGN REGEXP
;; IGNORE-USER-BOOST-RE-ASSIGN := IGNORE-USER-RE-KEY ASSIGN REGEXP
;; IGNORE-TAG-RE-ASSIGN        := IGNORE-USER-RE-KEY ASSIGN REGEXP
;; COLOR-RE-ASSIGN             := COLOR-RE-KEY ASSIGN REGEXP FG-COLOR (? ATTRIBUTE-VALUE)
;; USE-FILE                    := (AND USE BLANKS FILEPATH BLANKS)
;; POST-ALLOWED-LANGUAGE       := "post-allowed-language" BLANKS ASSIGN REGEXP
;; KEY                         := FIELD (FIELD-SEPARATOR KEY)*
;; BLANKS                      := (BLANK)*
;; FILEPATH                    := QUOTED-STRING
;; PROGRAM-NAME                := QUOTED-STRING
;; USE-CACHE                   := USE BLANKS CACHE
;; NOWAIT                      := NO BLANKS WAIT BLANKS (BUFFER-LABEL BLANKS DIGIT+)?
;; NO                          := "no"
;; WAIT                        := "wait"
;; CACHE                       := "cache"
;; USE                         := "use"
;; SERVER-KEY                  := "server"
;; USERNAME-KEY                := "username"
;; COLOR-RE-KEY                := "color-regexp"
;; IGNORE-USER-RE-KEY          := "ignore-user-regexp"
;; OPEN                        := "open"
;; OPEN-LINK-HELPER-KEY        := OPEN
;; WITH-KEY                    := "with"
;; BUFFER-LABEL                := "buffer"
;; REGEXP                      := QUOTED-STRING
;; QUOTED-STRING               := #\" (not #\") #\"
;; FIELD                       := ( (or ESCAPED-CHARACTER
;;                                  (not #\# ASSIGN BLANK FIELD-SEPARATOR) )*
;; COMMENT                     := BLANKS #\# (not #\Newline)* BLANKS
;; FIELD-SEPARATOR             := #\.
;; GENERIC-VALUE               := KEY
;; ASSIGN                      := #\=
;; BLANK                       := (or #\space #\Newline #\Tab)
;; BG-COLOR                    := COLOR
;; FG-COLOR                    := COLOR
;; COLOR                       := HEX-COLOR | COLOR-NAME
;; HEX-COLOR                   := HEXCOLOR-PREFIX
;;                               HEXDIGIT HEXDIGIT -> red
;;                               HEXDIGIT HEXDIGIT -> green
;;                               HEXDIGIT HEXDIGIT -> blue
;; ESCAPED-CHARACTER           := #\\ any-character
;; HEXCOLOR-PREFIX             := #\#
;; HEX-DIGIT                   := (and (character-ranges #\0 #\9)
;;                                    (character-ranges #\a #\f)
;;                                    (character-ranges #\A #\f)
;; DIGIT                       := (character-ranges #\0 #\9)
;; ATTRIBUTE-VALUE             := "bold"
;;                               | "italic"
;;                               | "underline"
;;                               | "blink"
;; COLOR-NAME                  := "black"
;;                               | "red"
;;                               | "green"
;;                               | "yellow"
;;                               | "blue"
;;                               | "magenta"
;;                               | "cyan"
;;                               | "white"

(define-constant +conf-filename+         "main.conf"             :test #'string=)

(define-constant +shared-conf-filename+  "shared.conf"           :test #'string=)

(define-constant +field-separator-value+ "."                     :test #'string=)

(define-constant +field-separator+       :field-separator        :test #'eq)

(define-constant +false-values+          '("no" "false")         :test #'equalp)

(defrule blank (or #\space #\Newline #\Tab)
  (:constant nil))

(defrule blanks (* blank)
  (:constant nil))

(defrule assign #\=
  (:constant nil))

(defrule comment (and blanks #\# (* (not #\Newline)) blanks)
  (:constant nil))

(defrule hexcolor-prefix #\#)

(defrule digit (character-ranges (#\0 #\9))
  (:text t))

(defrule hex-digit
    (or (character-ranges (#\0 #\9))
        (character-ranges (#\a #\f))
        (character-ranges (#\A #\F))))

(defrule hexcolor
    (and hexcolor-prefix
         hex-digit hex-digit  ; r
         hex-digit hex-digit  ; g
         hex-digit hex-digit) ; b
  (:text t)
  (:function (lambda (a) (parse-integer a :start 1 :radix 16))))

(defun keywordize (a)
  (make-keyword (string-upcase a)))

(defrule colorname
    (or "black"
        "red"
        "green"
        "yellow"
        "blue"
        "magenta"
        "cyan"
        "white")
  (:function keywordize))

(defrule escaped-character (and #\\ character)
  (:function (lambda (a) (list (second a)))))

(defrule field-separator #\.)

(defrule field
    (* (or escaped-character
           (not (or #\# assign field-separator blank))))

  (:text t))

;; this rule is not actually part of the grammar but jus a convenience
;; function to remove duplicated code (see rules: key and value)
(defrule fields
    (and field
         (? (and field-separator fields)))
  (:function flatten))

(defrule key fields
  (:function (lambda (a)
               (mapcar (lambda (element)
                         (if (string= +field-separator-value+ element)
                             nil
                             (format-keyword element)))
                       a)))
  (:function remove-if-null))

(defrule generic-value fields
  (:text t))

(defrule generic-assign
    (and key blanks assign blanks
         (or quoted-string
             hexcolor
             colorname
             generic-value) ; the order in this list *is* important
         blanks)
  (:function remove-if-null))

(defrule quoted-string (and #\" (+ (not #\")) #\")
  (:function (lambda (a) (second a)))
  (:text t))

(defrule regexp quoted-string)

(defrule color-re-key "color-regexp"
  (:constant :color-re))

(defclass color-re-assign ()
  ((re
    :initform nil
    :initarg  :re
    :accessor re)
   (color-name
    :initform nil
    :initarg  :color-name
    :accessor color-name)
   (color-value
    :initform nil
    :initarg  :color-value
    :accessor color-value)
   (attributes
    :initform nil
    :initarg  :attributes
    :accessor attributes))
  (:documentation "A color assign based on a regular expression. Slots
  color-name and color-value are mutually exclusive"))

(defmethod print-object ((object color-re-assign) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-accessors ((re         re)
                     (color-name  color-name)
                     (color-value color-value)
                     (attributes  attributes)) object
      (format stream "re: ~s colorname: ~s colorvalue: ~s attributes ~a"
              re color-name color-value attributes))))

(defun make-color-re-assign (re color-name color-value attributes)
  (assert (and (or color-name
                   color-value)
               (or (null color-name)
                   (null color-value))))
  (make-instance 'color-re-assign
                 :re          re
                 :color-name  color-name
                 :color-value color-value
                 :attributes  attributes))

(defun build-color-re-assign (parsed)
  (let* ((clean         (remove-if-null parsed))
         (re            (second parsed))
         (color         (third  parsed))
         (color-name-p  (keywordp color))
         (attributes    (first (fourth parsed))))
    (list (first clean)
          (make-color-re-assign (cl-ppcre:create-scanner re)
                                (and color-name-p       color)
                                (and (not color-name-p) color)
                                attributes))))

(defrule attribute-value (or "bold"
                             "italic"
                             "underline"
                             "blink")
  (:text t)
  (:function (lambda (a) (tui-utils:text->tui-attribute a))))

(defrule color-re-assign
    (and color-re-key blanks
         assign blanks regexp blanks
         (or hexcolor colorname) blanks
         (? (and attribute-value blanks)))
  (:function remove-if-null)
  (:function build-color-re-assign))

(defrule ignore-user-re-key "ignore-user-regexp"
  (:constant :ignore-user-re))

(defrule ignore-user-boost-re-key "ignore-user-boost-regexp"
  (:constant :ignore-user-boost-re))

(defrule ignore-tag-re-assign "ignore-tag-regexp"
  (:constant :ignore-tag-re))

(defrule ignore-user-re-assign
    (and ignore-user-re-key blanks
         assign blanks regexp blanks)
  (:function (lambda (a) (list (first a) (fifth a)))))

(defrule ignore-user-boost-re-assign
    (and ignore-user-boost-re-key blanks
         assign blanks regexp blanks)
  (:function (lambda (a) (list (first a) (fifth a)))))

(defrule server-key "server"
  (:constant :server))

(defrule username-key "username"
  (:constant :username))

(defrule open "open"
  (:constant :open))

(defrule open-link-helper-key open)

(defrule with "with"
  (:constant :with))

(defrule server-assign
    (and server-key blanks assign blanks generic-value blanks)
  (:function remove-if-null))

(defrule username-assign
    (and username-key blanks assign blanks generic-value blanks)
  (:function remove-if-null))

(define-constant +buffer-minimum-size-to-open+ (expt 1024 2) :test #'=
  :documentation "Minimum size of the saved contents (non gemini text)
  before  attempt  to  opening  with  an  user  defined  program:  see
  configuration directive 'use program foo *no wait*'")

(defclass open-link-helper ()
  ((re
    :initform nil
    :initarg  :re
    :accessor re)
   (program-name
    :initform nil
    :initarg  :program-name
    :accessor program-name)
   (use-cache
    :initform t
    :initarg  :use-cache
    :reader   use-cache-p
    :writer   (setf use-cache))
   (wait
    :initform  t
    :initarg   :wait
    :reader    waitp
    :writer    (setf wait))
   (buffer-size
    :initform  +buffer-minimum-size-to-open+
    :initarg   :buffer-size
    :accessor   buffer-size))
  (:documentation "When a gemini link matches `re' try to open it with 'program-name'"))

(defmethod print-object ((object open-link-helper) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-accessors ((re           re)
                     (program-name program-name)
                     (use-cache-p  use-cache-p)
                     (waitp        waitp)
                     (buffer-size  buffer-size)) object
      (format stream
              "re: ~s program: ~s use cache? ~a wait? ~a buffer size: ~a"
              re program-name use-cache-p waitp buffer-size))))

(defun make-open-link-helper (re program-name use-cache
                              &key (wait t) (buffer-size +buffer-minimum-size-to-open+))

  (assert (stringp program-name))
  (assert (stringp re))
  (assert (integerp buffer-size))
  (assert (> buffer-size 0))
  (make-instance 'open-link-helper
                 :re           re
                 :program-name program-name
                 :use-cache    use-cache
                 :wait         wait
                 :buffer-size  buffer-size))

(defrule use "use"
  (:text t))

(defrule cache "cache"
  (:text t))

(defrule no "no"
  (:text t))

(defrule wait "wait"
  (:text t))

(defrule buffer-label "buffer"
  (:text t))

(defrule use-cache (and use blanks cache)
  (:constant t))

(defrule no-wait (and no blanks wait)
  (:constant t))

(defrule open-link-helper
    (and open-link-helper-key
         blanks
         regexp ; 2 link-pattern
         blanks
         with
         blanks
         regexp ; 6 program to use
         blanks
         (? (and use-cache ; 8 use cache?
                 blanks))
         (? (and no-wait ; 9 wait download? Buffer size?
                 blanks
                 (? (and buffer-label blanks (+ digit)))
                 blanks)))
  (:function (lambda (args)
               (let* ((use-cache       (elt args 8))
                      (wait-parameters (elt args 9))
                      (waitp           (not (and wait-parameters (elt wait-parameters 0))))
                      (buffer-size     (if (and wait-parameters
                                                (elt wait-parameters 2))
                                           (let* ((buffer-parameters  (elt wait-parameters 2))
                                                  (buffer-string-list (elt buffer-parameters 2))
                                                  (mebibyte-string    (reduce #'strcat
                                                                              buffer-string-list))
                                                  (mebimytes   (parse-integer mebibyte-string))
                                                  (bytes       (* 1024 1024 mebimytes)))
                                             (abs bytes))
                                           swconf:+buffer-minimum-size-to-open+)))
                 (list :open-link-helper
                       (make-open-link-helper (elt args 2)
                                              (elt args 6)
                                              use-cache
                                              :wait        waitp
                                              :buffer-size buffer-size))))))

(defrule post-allowed-language (and "post-allowed-language" blanks assign regexp)
  (:function remove-if-null))

(defrule filepath quoted-string)

(defparameter *already-included-files* ())

(defrule use-file (and use blanks filepath blanks)
  (:function (lambda (a)
               (let ((file (third a)))
                 (if (find file *already-included-files* :test #'string=)
                     (error "Cyclic include of file ~s detected" file)
                     (progn
                       (push file *already-included-files*)
                       (load-config-file (third a) nil)))
                 nil))))

(defrule entries
    (and (* comment)
         (or use-file
             color-re-assign
             ignore-user-re-assign
             ignore-user-boost-re-assign
             ignore-tag-re-assign
             server-assign
             username-assign
             open-link-helper
             post-allowed-language
             generic-assign)
         (* comment))
  (:function second))

(defrule config (* entries)
  (:function remove-if-null))

(defgeneric parse-config (object))

(defmethod parse-config ((object string))
  (parse 'config object))

(defmethod parse-config ((object pathname))
  (parse-config (fs:slurp-file object)))

(defparameter *software-configuration* ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-key-constant-name (name)
    (format-fn-symbol t "+key-~a+" name)))

(defmacro gen-key-constant (name)
  `(define-constant ,(gen-key-constant-name name)
       ,(format-keyword name)
     :test #'eq))

(defmacro gen-key-constants (&rest names)
  `(progn
     ,@(loop for name in names collect
             `(gen-key-constant ,name))))

(gen-key-constants unknown
                   experimental
                   regex
                   background
                   foreground
                   title
                   start
                   end
                   left
                   right
                   geometry
                   tile
                   stopper
                   root
                   width
                   height
                   position
                   exclusive
                   search
                   mode
                   count
                   toc
                   downloading
                   animation
                   x
                   y
                   error
                   info
                   window
                   header
                   focus
                   prefix
                   postfix
                   line
                   padding
                   value
                   scheme
                   uri
                   link
                   links
                   http
                   creation-time
                   access-time
                   visibility
                   public
                   unlisted
                   private
                   direct
                   quote
                   h1
                   h2
                   h3
                   bullet
                   preformatted-text
                   other
                   attribute
                   new-message
                   mark
                   vote-vertical-bar
                   crypted
                   open-link-helper
                   histogram
                   error-dialog
                   info-dialog
                   input-dialog
                   help-dialog
                   notify-window
                   gempub-library-window
                   notification-icon
                   life
                   quick-help
                   more-choices
                   modeline
                   date-format
                   locked
                   unlocked
                   account
                   signature-file
                   main-window
                   thread-window
                   message-window
                   chat-window
                   chats-list-window
                   gemini-subscription-window
                   gemini-toc-window
                   gopher-window
                   attachment-header
                   max-numbers-allowed-attachments
                   max-message-length
                   max-report-comment-length
                   reply-quoted-character
                   line-position-mark
                   favourite
                   sensitive
                   boosted
                   tags-window
                   conversations-window
                   keybindings-window
                   suggestions-window
                   open-attach-window
                   open-message-link-window
                   open-gemini-stream-window
                   gemini-certificates-window
                   command-window
                   file-explorer
                   command-separator
                   gemini
                   gemlog
                   gempub
                   library
                   sync
                   favicon
                   tree
                   branch
                   arrow
                   left-arrow
                   data
                   data-leaf
                   leaf
                   branch
                   spacer
                   vertical-line
                   editor
                   username
                   server
                   message
                   selected
                   unselected
                   deleted
                   fetched
                   delete
                   input
                   read
                   unread
                   directory-symbol
                   directory
                   file
                   binary-file
                   text-file
                   image-file
                   images
                   gif-file
                   fetch
                   update
                   iri
                   fragment
                   close-after-select
                   password-echo-character
                   color-re
                   ignore-user-re
                   ignore-user-boost-re
                   ignore-tag-re
                   post-allowed-language
                   purge-history-days-offset
                   purge-cache-days-offset
                   mentions
                   montage)

(defun perform-missing-value-check (file)
  (handler-case
      (trivial-configuration-missing-value-check)
    (error (e)
      (error (format nil "Error while loading the file ~a~%~a~%" file e)))))

(defun load-config-file (&optional (virtual-filepath +conf-filename+)
                           (perform-missing-value-check nil))
  (let* ((file (res:get-config-file virtual-filepath))
         (tree (parse-config (fs:namestring->pathname file))))
    (loop for entry in tree do
      (let ((key   (first  entry))
            (value (second entry)))
        (cond
          ((or (eq +key-color-re+ key)
               (eq +key-ignore-user-re+ key)
               (eq +key-ignore-user-boost-re+ key)
               (eq +key-ignore-tag-re+ key)
               (eq +key-open-link-helper+ key)
               (eq +key-post-allowed-language+ key))
           (setf (access:accesses *software-configuration* key)
                 (append (access:accesses *software-configuration* key)
                         (list value))))
          ((keywordp key)
           (setf (access:accesses *software-configuration* key) value))
          (t
           (multiple-value-bind (rest all)
               (apply #'access:set-accesses value *software-configuration* key)
             (declare (ignore rest))
             (setf *software-configuration* all))))))
    (when perform-missing-value-check
      (perform-missing-value-check file))
    (if *software-configuration*
        (values *software-configuration* file)
        (error (format nil (_ "fatal error: The file ~a is empty") file)))))

;;;; end of parser

(defparameter *allowed-status-visibility* '("public" "unlisted" "private" "direct")
  "- public   Visible to everyone, shown in public timelines;
   - unlisted Visible to public, but not included in public timelines;
   - private  Visible to followers only, and to any mentioned users;
   - direct   Visible only to mentioned users.")

(defparameter *allowed-attachment-type*   '("unknown" "image" "gifv" "video" "audio"))

(define-constant +default-signature-filename+ ".signature" :test #'string=)

;;;; interface

(defun false-value-p (v)
  (or (null v)
      (member v +false-values+ :test #'string=)))

(defun access-key->user-directive (key)
  (join-with-strings (mapcar #'string-downcase key) "."))

(defun access-non-null-conf-value (object &rest keys)
  (let ((value (apply #'access:accesses object keys)))
    (if (null value)
        (error (_ (format nil
                          (_ "The configuration (*.conf) file is missing the value for ~s")
                          (access-key->user-directive keys))))
        value)))

(defun close-link-window-after-select-p ()
  (let ((value (access:accesses *software-configuration*
                                +key-open-message-link-window+
                                +key-close-after-select+)))
    (not (false-value-p value))))

(defun suggestion-window-selected-item-colors ()
  (values (access-non-null-conf-value *software-configuration*
                                               +key-suggestions-window+
                                               +key-selected+
                                               +key-background+)
          (access-non-null-conf-value *software-configuration*
                                               +key-suggestions-window+
                                               +key-selected+
                                               +key-foreground+)))
(defun gemini-downloading-animation ()
  (let ((animation (access-non-null-conf-value *software-configuration*
                                               +key-gemini+
                                               +key-downloading+
                                               +key-animation+)))
    (text-utils:split-words animation)))

(defun gemini-default-favicon ()
  (access-non-null-conf-value *software-configuration*
                              +key-gemini+
                              +key-favicon+))

(defun gemini-update-gemlog-at-start-p ()
  (let ((value (access:accesses *software-configuration*
                                +key-start+
                                +key-update+
                                +key-gemlog+)))
    (not (false-value-p value))))

(defun directory-symbol ()
  (or (access:accesses *software-configuration*
                       +key-directory-symbol+)
      (_ "(directory)")))

(defun gemini-fetch-favicon-p ()
  (let ((fetchp (access:accesses *software-configuration*
                                 +key-gemini+
                                 +key-fetch+
                                 +key-favicon+)))
    (db-utils:db-not-nil-p fetchp)))

(defun gemini-link-colors ()
  (values (access:accesses *software-configuration*
                           +key-gemini+
                           +key-link+
                           +key-background+)
          (access:accesses *software-configuration*
                           +key-gemini+
                           +key-link+
                           +key-foreground+)
          (tui-utils:text->tui-attribute (access:accesses *software-configuration*
                                                          +key-gemini+
                                                          +key-link+
                                                          +key-attribute+))))

(defun gemini-link-prefix (scheme)
  (access-non-null-conf-value *software-configuration*
                              +key-gemini+
                              +key-link+
                              +key-scheme+
                              scheme
                              +key-prefix+))

(defun gemini-link-prefix-to-gemini ()
  (gemini-link-prefix +key-gemini+))

(defun gemini-link-prefix-to-other ()
  (gemini-link-prefix +key-other+))

(defun gemini-link-prefix-to-http ()
  (gemini-link-prefix +key-http+))

(defun gemini-quote-prefix ()
  (access-non-null-conf-value *software-configuration*
                              +key-gemini+
                              +key-quote+
                              +key-prefix+))


(defun gemini-h*-prefix (level)
  (access-non-null-conf-value *software-configuration*
                              +key-gemini+
                              level
                              +key-prefix+))

(defun gemini-h1-prefix ()
  (gemini-h*-prefix +key-h1+))

(defun gemini-h2-prefix ()
  (gemini-h*-prefix +key-h2+))

(defun gemini-h3-prefix ()
  (gemini-h*-prefix +key-h3+))

(defun gemini-bullet-prefix ()
  (access-non-null-conf-value *software-configuration*
                              +key-gemini+
                              +key-bullet+
                              +key-prefix+))

(defun gemini-preformatted-fg ()
  (or (access-non-null-conf-value *software-configuration*
                                  +key-gemini+
                                  +key-preformatted-text+
                                  +key-foreground+)
      :white))

(defun gemini-subscription-url-fg ()
  (access-non-null-conf-value *software-configuration*
                              +key-gemini-subscription-window+
                              +key-uri+
                              +key-foreground+))

(defun gemini-subscription-count-fg ()
  (access-non-null-conf-value *software-configuration*
                              +key-gemini-subscription-window+
                              +key-count+
                              +key-foreground+))

(defun gemini-certificates-window-colors ()
  "return three color values"
  (values (access:accesses *software-configuration*
                           +key-gemini-certificates-window+
                           +key-link+
                           +key-foreground+)
          (access:accesses *software-configuration*
                           +key-gemini-certificates-window+
                           +key-creation-time+
                           +key-foreground+)
          (access:accesses *software-configuration*
                           +key-gemini-certificates-window+
                           +key-access-time+
                           +key-foreground+)))

(defun gemini-toc-padding-char ()
  (let ((padding-from-conf (access:accesses *software-configuration*
                                            +key-gemini-toc-window+
                                            +key-padding+)))
    (if padding-from-conf
        (elt padding-from-conf 0)
        #\Space)))

(defun signature-file-path ()
  "Returns the filepath of the signature file, the $HOME is prepended."
  (let* ((signature-file (or (access:accesses *software-configuration*
                                              +key-signature-file+)
                             +default-signature-filename+))
         (signature-path (fs:cat-parent-dir (fs:home-dir) signature-file)))
    (if (fs:file-exists-p signature-path)
        signature-path
        nil)))

(defun vote-vertical-bar ()
  (or (access:accesses *software-configuration*
                       +key-vote-vertical-bar+)
      "="))

(defun crypted-mark-value ()
  (or (access:accesses *software-configuration*
                       +key-crypted+
                       +key-mark+
                       +key-value+)
      (_ "This message was crypted.")))

(defun quick-help-header-colors ()
  (values (access:accesses *software-configuration*
                           +key-quick-help+
                           +key-header+
                           +key-background+)
          (access:accesses *software-configuration*
                           +key-quick-help+
                           +key-header+
                           +key-foreground+)
          (tui-utils:text->tui-attribute (access:accesses *software-configuration*
                                                          +key-quick-help+
                                                          +key-header+
                                                          +key-attribute+))))

(defun window-titles-end (side)
  (assert (member side (list +key-left+ +key-right+)))
  (access-non-null-conf-value *software-configuration*
                              +key-window+
                              +key-title+
                              side
                              +key-stopper+
                              +key-value+))

(defun window-titles-ends ()
  (multiple-value-bind (x y focus-value)
      (config-win-focus-mark)
    (declare (ignore x y))
    (values (window-titles-end +key-left+)
            (window-titles-end +key-right+)
            (+ 2 (length focus-value)))))

(defun tags-histogram-foreground ()
  (access:accesses *software-configuration*
                   +key-tags-window+
                   +key-histogram+
                   +key-foreground+))

(defun tags-new-message-mark ()
  (access-non-null-conf-value *software-configuration*
                              +key-tags-window+
                              +key-new-message+
                              +key-mark+
                              +key-value+))

(defun conversation-window-message-count-colors (key-read/unread)
  (values (access:accesses *software-configuration*
                           +key-conversations-window+
                           key-read/unread
                           +key-foreground+)
          (access:accesses *software-configuration*
                           +key-conversations-window+
                           key-read/unread
                           +key-background+)))

(defun conversation-window-read-colors ()
  (multiple-value-bind (fg bg)
      (conversation-window-message-count-colors +key-read+)
    (values fg bg)))

(defun conversation-window-unread-colors ()
  (multiple-value-bind (fg bg)
      (conversation-window-message-count-colors +key-unread+)
    (values fg bg)))

(defun max-message-length ()
  (num:parse-number-default (access:accesses *software-configuration*
                                             +key-max-message-length+)
                            500))

(defun max-report-comment-length ()
  (num:parse-number-default (access:accesses *software-configuration*
                                             +key-max-report-comment-length+)
                            100))

(defun quote-char ()
  (or (access:accesses *software-configuration*
                       +key-reply-quoted-character+)
      "> "))

(defun max-attachments-allowed ()
  (num:parse-number-default (access:accesses *software-configuration*
                                             +key-max-numbers-allowed-attachments+)
                            4))

(defun gempub-library-directory ()
  (or (access:accesses *software-configuration*
                       +key-gempub+
                       +key-directory+
                       +key-library+)
      (res:home-datadir)))

(defun external-editor ()
  (access:access *software-configuration*
                 +key-editor+))

(defun color-regexps ()
  (access:accesses *software-configuration*
                   +key-color-re+))

(defun ignore-users-regexps ()
  (access:accesses *software-configuration*
                   +key-ignore-user-re+))

(defun ignore-users-boost-regexps ()
  (access:accesses *software-configuration*
                   +key-ignore-user-boost-re+))

(defun ignore-tag-regexps ()
  (access:accesses *software-configuration*
                   +key-ignore-tag-re+))

(defmacro gen-win-key-access (fn-suffix key)
  `(defun ,(misc:format-fn-symbol t "win-~a" fn-suffix) (win-key)
     (access:accesses *software-configuration*
                      win-key
                      ,key)))

(gen-win-key-access bg     +key-background+)

(gen-win-key-access fg     +key-foreground+)

(gen-win-key-access height +key-height+)

(gen-win-key-access width  +key-width+)

(defmacro gen-simple-access ((fn-name &key (transform-value-fn 'identity)) &rest keys)
  `(defun ,(misc:format-fn-symbol t "config-~a" fn-name) ()
     (,transform-value-fn (access:accesses *software-configuration*
                                           ,@keys))))

(gen-simple-access (delete-fetched-mentions-p
                    :transform-value-fn db-utils:db-not-nil-p)
                    +key-delete+
                    +key-fetched+
                    +key-mentions+)

(gen-simple-access (gemini-fullscreen-toc-width
                    :transform-value-fn main-window:parse-subwin-w)
                    +key-gemini+
                    +key-exclusive+
                    +key-mode+
                    +key-toc+
                    +key-width+)

(gen-simple-access (gemini-fullscreen-links-height
                    :transform-value-fn main-window:parse-subwin-h)
                    +key-gemini+
                    +key-exclusive+
                    +key-mode+
                    +key-links+
                    +key-height+)

(gen-simple-access (post-allowed-language
                    :transform-value-fn
                    (lambda (a) (cl-ppcre:create-scanner a :case-insensitive-mode t)))
                   +key-post-allowed-language+)

(gen-simple-access (purge-history-days-offset
                    :transform-value-fn
                    (lambda (a)
                      (num:safe-parse-number a
                                             :fix-fn (lambda (e)
                                                       (declare (ignore e))
                                                       100))))
                   +key-purge-history-days-offset+)

(gen-simple-access (purge-cage-days-offset
                    :transform-value-fn
                    (lambda (a)
                      (num:safe-parse-number a
                                             :fix-fn (lambda (e)
                                                       (declare (ignore e))
                                                       100))))
                   +key-purge-history-days-offset+)

(gen-simple-access (notification-life
                    :transform-value-fn
                    (lambda (a)
                      (num:safe-parse-number a
                                             :fix-fn (lambda (e)
                                                       (declare (ignore e))
                                                       100))))
                   +key-notify-window+
                   +key-life+)

(gen-simple-access (gemini-fragment-as-regex-p
                    :transform-value-fn (lambda (a) (not (false-value-p a))))
                   +key-experimental+
                   +key-gemini+
                   +key-iri+
                   +key-fragment+
                   +key-regex+)

(gen-simple-access (gemini-images-montage-geometry)
                   +key-gemini+
                   +key-images+
                   +key-montage+
                   +key-geometry+)

(gen-simple-access (gemini-images-montage-tile)
                   +key-gemini+
                   +key-images+
                   +key-montage+
                   +key-tile+)

(defun config-notify-window-geometry ()
  (values (access:accesses  *software-configuration*
                            +key-notify-window+
                            +key-position+
                            +key-x+)
          (access:accesses  *software-configuration*
                            +key-notify-window+
                            +key-position+
                            +key-y+)
          (access:accesses  *software-configuration*
                            +key-notify-window+
                            +key-width+)))

(gen-simple-access (notification-icon)
                   +key-thread-window+
                   +key-modeline+
                   +key-notification-icon+
                   +key-value+)

(gen-simple-access (server-name)
                   +key-server+)

(gen-simple-access (username)
                   +key-username+)

(gen-simple-access (password-echo-character)
                   +key-password-echo-character+)

(gen-simple-access (all-link-open-program) +key-open-link-helper+)

(gen-simple-access (gopher-line-prefix-directory)
                   +key-gopher-window+
                   +key-line+
                   +key-prefix+
                   +key-directory+)

(gen-simple-access (gopher-line-prefix-uri)
                   +key-gopher-window+
                   +key-line+
                   +key-prefix+
                   +key-uri+)

(gen-simple-access (gopher-line-prefix-unknown)
                   +key-gopher-window+
                   +key-line+
                   +key-prefix+
                   +key-unknown+)

(gen-simple-access (gopher-line-prefix-binary-file)
                   +key-gopher-window+
                   +key-line+
                   +key-prefix+
                   +key-binary-file+)

(gen-simple-access (gopher-line-prefix-text-file)
                   +key-gopher-window+
                   +key-line+
                   +key-prefix+
                   +key-text-file+)

(gen-simple-access (gopher-line-prefix-image-file)
                   +key-gopher-window+
                   +key-line+
                   +key-prefix+
                   +key-image-file+)

(gen-simple-access (gopher-line-prefix-gif-file)
                   +key-gopher-window+
                   +key-line+
                   +key-prefix+
                   +key-gif-file+)

(gen-simple-access (gopher-line-prefix-search-index)
                   +key-gopher-window+
                   +key-line+
                   +key-prefix+
                   +key-search+)

(gen-simple-access (gopher-line-prefix-foreground)
                   +key-gopher-window+
                   +key-line+
                   +key-prefix+
                   +key-foreground+)

(gen-simple-access (gopher-line-prefix-attribute
                    :transform-value-fn tui-utils:text->tui-attribute)
                   +key-gopher-window+
                   +key-line+
                   +key-prefix+
                   +key-attribute+)

(defun link-regex->program-to-use-parameters (link)
  (find-if (lambda (a) (cl-ppcre:scan (re a) link))
           (config-all-link-open-program)))

(defun link-regex->program-to-use (link)
  (when-let ((found (link-regex->program-to-use-parameters link)))
    (values (program-name found)
            (use-cache-p  found)
            (waitp        found)
            (buffer-size  found))))

(defun link-regex->program-to-use-buffer-size (link)
  (when-let ((found (link-regex->program-to-use-parameters link)))
    (buffer-size  found)))

(defun use-tinmop-as-external-program-p (program)
  (cl-ppcre:scan "(^me$)|(^internal$)|(tinmop)" program))

(defun use-editor-as-external-program-p (program)
  (cl-ppcre:scan "(^ed$)|(^editor$)" program))

(defun config-win-focus-mark ()
  (values (access:accesses  *software-configuration*
                            +key-window+
                            +key-focus+
                            +key-mark+
                            +key-background+)
          (access:accesses  *software-configuration*
                            +key-window+
                            +key-focus+
                            +key-mark+
                            +key-foreground+)
          (access-non-null-conf-value  *software-configuration*
                                       +key-window+
                                       +key-focus+
                                       +key-mark+
                                       +key-value+)))

(defun command-separator-config-values ()
  (values (access:accesses *software-configuration*
                           +key-command-window+
                           +key-command-separator+
                           +key-background+)
          (access:accesses *software-configuration*
                           +key-command-window+
                           +key-command-separator+
                           +key-foreground+)
          (access-non-null-conf-value *software-configuration*
                                      +key-command-window+
                                      +key-command-separator+
                                      +key-value+)))

(defun command-error-message-colors ()
  (values (access:accesses *software-configuration*
                           +key-command-window+
                           +key-error+
                           +key-message+
                           +key-background+)
          (access:accesses *software-configuration*
                           +key-command-window+
                           +key-error+
                           +key-message+
                           +key-foreground+)
          (tui-utils:text->tui-attribute (access:accesses *software-configuration*
                                                          +key-command-window+
                                                          +key-error+
                                                          +key-message+
                                                          +key-attribute+))))

(defun command-info-message-colors ()
  (values (access:accesses *software-configuration*
                           +key-command-window+
                           +key-info+
                           +key-message+
                           +key-background+)
          (access:accesses *software-configuration*
                           +key-command-window+
                           +key-info+
                           +key-message+
                           +key-foreground+)
          (tui-utils:text->tui-attribute (access:accesses *software-configuration*
                                                          +key-command-window+
                                                          +key-info+
                                                          +key-message+
                                                          +key-attribute+))))

(defun left-arrow ()
  (access-non-null-conf-value *software-configuration*
                              +key-left-arrow+))

(defun tree-config-colors (tree-win-holder)
  (assert tree-win-holder)
  (values (access:accesses *software-configuration*
                           tree-win-holder
                           +key-tree+
                           +key-branch+
                           +key-foreground+)
          (access:accesses *software-configuration*
                           tree-win-holder
                           +key-tree+
                           +key-arrow+
                           +key-foreground+)
          (access:accesses *software-configuration*
                           tree-win-holder
                           +key-tree+
                           +key-data+
                           +key-foreground+)
          (access:accesses *software-configuration*
                           tree-win-holder
                           +key-tree+
                           +key-data-leaf+
                           +key-foreground+)
          (access:accesses *software-configuration*
                           tree-win-holder
                           +key-tree+
                           +key-root+
                           +key-foreground+)))

(defun tree-config-rendering-values (tree-win-holder)
  (values (access-non-null-conf-value *software-configuration*
                                      tree-win-holder
                                      +key-tree+
                                      +key-arrow+
                                      +key-value+)
          (access-non-null-conf-value *software-configuration*
                                      tree-win-holder
                                      +key-tree+
                                      +key-leaf+
                                      +key-value+)
          (access-non-null-conf-value *software-configuration*
                                      tree-win-holder
                                      +key-tree+
                                      +key-branch+
                                      +key-value+)
          (access-non-null-conf-value *software-configuration*
                                      tree-win-holder
                                      +key-tree+
                                      +key-spacer+
                                      +key-value+)
          (access-non-null-conf-value *software-configuration*
                                      tree-win-holder
                                      +key-tree+
                                      +key-vertical-line+
                                      +key-value+)))

(defun make-tree-colormap (window-key)
  (assert window-key)
  (let ((tree-color-map ()))
    (flet ((add-color-pair (key color)
             (setf tree-color-map (acons key color tree-color-map))))
      (multiple-value-bind (branch-color arrow-color data-color leaf-color root-color)
          (swconf:tree-config-colors window-key)
        (add-color-pair :branch    branch-color)
        (add-color-pair :arrow     arrow-color)
        (add-color-pair :data      data-color)
        (add-color-pair :data-leaf leaf-color)
        (add-color-pair :data-root root-color))
      tree-color-map)))

(defun thread-message-symbol-lookup (field key)
  (access:accesses *software-configuration*
                   +key-thread-window+
                   +key-message+
                   field
                   key))

(defun thread-message-symbol-value (field)
  (thread-message-symbol-lookup field +key-value+))

(defun thread-message-symbol-fg (field)
  (thread-message-symbol-lookup field +key-foreground+))

(defun thread-message-symbol (field)
  (values (thread-message-symbol-value field)
          (thread-message-symbol-fg    field)))

(defun thread-message-colors (key)
  (values (access:accesses *software-configuration*
                           +key-thread-window+
                           +key-message+
                           key
                           +key-background+)
          (access:accesses *software-configuration*
                           +key-thread-window+
                           +key-message+
                           key
                           +key-foreground+)
          (tui-utils:text->tui-attribute (access:accesses *software-configuration*
                                                          +key-thread-window+
                                                          +key-message+
                                                          key
                                                          +key-attribute+))))
(defun thread-message-read-colors ()
  (multiple-value-bind (bg fg attribute)
      (thread-message-colors +key-read+)
    (values bg fg  attribute)))

(defun thread-message-unread-colors ()
  (multiple-value-bind (bg fg attribute)
      (thread-message-colors +key-unread+)
    (values bg fg attribute)))

(defun thread-message-selected-colors ()
  (multiple-value-bind (bg fg attribute)
      (thread-message-colors +key-selected+)
    (values bg fg attribute)))

(defun thread-message-deleted-colors ()
  (multiple-value-bind (bg fg attribute)
      (thread-message-colors +key-deleted+)
    (values bg fg attribute)))

(defun modeline-colors (window-key)
  (values (access:accesses *software-configuration*
                           window-key
                           +key-modeline+
                           +key-background+)
          (access:accesses *software-configuration*
                           window-key
                           +key-modeline+
                           +key-foreground+)))

(defun modeline-fmt (window-key)
  (access-non-null-conf-value *software-configuration*
                              window-key
                              +key-modeline+
                              +key-value+))

(defun date-fmt (window-key)
  (let* ((raw (access-non-null-conf-value *software-configuration*
                                          window-key
                                          +key-date-format+
                                          +key-value+)))
    (date-formatter:expand-date-formatter-spec raw)))

(defun locked/unlocked-value (key-window locked)
  (let ((key-locked (if locked
                        +key-locked+
                        +key-unlocked+)))
    (access-non-null-conf-value *software-configuration*
                                key-window
                                key-locked
                                +key-value+)))

(defun locked/unlocked-account-mark-value (key-window locked)
  (let ((key-locked (if locked
                        +key-locked+
                        +key-unlocked+)))
    (access-non-null-conf-value *software-configuration*
                                key-window
                                +key-account+
                                key-locked
                                +key-mark+
                                +key-value+)))

(defun message-window-locked-account-mark ()
  (locked/unlocked-account-mark-value +key-message-window+ t))

(defun message-window-unlocked-account-mark ()
  (locked/unlocked-account-mark-value +key-message-window+ nil))

(defun message-window-account-locking-status-mark (locking-value)
  (if locking-value
      (message-window-locked-account-mark)
      (message-window-unlocked-account-mark)))

(defun message-window-line-mark-values ()
  "return three values: mark string fg and bg"
  (values (access-non-null-conf-value *software-configuration*
                                      +key-message-window+
                                      +key-line-position-mark+
                                      +key-value+)
          (access:accesses *software-configuration*
                           +key-message-window+
                           +key-line-position-mark+
                           +key-foreground+)
          (access:accesses *software-configuration*
                           +key-message-window+
                           +key-line-position-mark+
                           +key-background+)))

(defun message-window-visibility-mark (visibility-level)
  (access:accesses *software-configuration*
                   +key-message-window+
                   +key-visibility+
                   visibility-level))


(defmacro gen-visibility-mapping-marks (visibility-level)
  `(defun ,(format-fn-symbol t "message-window-visibility-~a-mark" visibility-level) ()
     (message-window-visibility-mark ,visibility-level)))

(gen-visibility-mapping-marks "public")

(gen-visibility-mapping-marks "unlisted")

(gen-visibility-mapping-marks "private")

(gen-visibility-mapping-marks "direct")

(defun message-windows-visibility-marks ()
  (list :public   (message-window-visibility-public-mark)
        :unlisted (message-window-visibility-unlisted-mark)
        :private  (message-window-visibility-private-mark)
        :direct   (message-window-visibility-direct-mark)))

(defun message-window-attachments-header ()
  (values (access:accesses *software-configuration*
                           +key-message-window+
                           +key-attachment-header+
                           +key-prefix+
                           +key-value+)
          (access:accesses *software-configuration*
                           +key-message-window+
                           +key-attachment-header+
                           +key-postfix+
                           +key-value+)
          (access:accesses *software-configuration*
                           +key-message-window+
                           +key-attachment-header+
                           +key-value+)))

(defclass form-style ()
  ((background
    :initform :black
    :initarg  :background
    :accessor background)
   (foreground
    :initform :white
    :initarg  :foreground
    :accessor foreground)
   (input-background
    :initform :black
    :initarg  :input-background
    :accessor input-background)
   (input-foreground
    :initform :white
    :initarg  :input-foreground
    :accessor input-foreground)
   (selected-background
    :initform :black
    :initarg  :selected-background
    :accessor selected-background)
   (selected-foreground
    :initform :white
    :initarg  :selected-foreground
    :accessor selected-foreground)
   (unselected-background
    :initform :black
    :initarg  :unselected-background
    :accessor unselected-background)
   (unselected-foreground
    :initform :white
    :initarg  :unselected-foreground
    :accessor unselected-foreground)))

(defmethod print-object ((object form-style) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((background            background)
                     (foreground            foreground)
                     (input-background      input-background)
                     (input-foreground      input-foreground)
                     (selected-background   selected-background)
                     (selected-foreground   selected-foreground)
                     (unselected-background unselected-background)
                     (unselected-foreground selected-foreground)) object
      (format stream
              "fg ~a bg ~a input-fg ~a input-bg ~a selected-fg ~a selected-bg ~a unselected-fg ~a unselected-bg ~a"
              foreground
              background
              input-foreground
              input-background
              selected-foreground
              selected-background
              unselected-foreground
              unselected-background))))

(defun form-style (window-key)
  (let* ((bg            (access:accesses *software-configuration*
                                         window-key
                                         +key-background+))
         (fg            (access:accesses *software-configuration*
                                         window-key
                                         +key-foreground+))
         (unselected-fg (or (access:accesses *software-configuration*
                                             window-key
                                             +key-input+
                                             +key-unselected+
                                             +key-foreground+)
                            fg))
         (unselected-bg (or (access:accesses *software-configuration*
                                             window-key
                                             +key-input+
                                             +key-unselected+
                                             +key-background+)
                            bg)))
    (make-instance 'form-style
                   :background           bg
                   :foreground           fg
                   :selected-background  (access:accesses *software-configuration*
                                                         window-key
                                                         +key-input+
                                                         +key-selected+
                                                         +key-background+)
                   :selected-foreground   (access:accesses *software-configuration*
                                                         window-key
                                                         +key-input+
                                                         +key-selected+
                                                         +key-foreground+)

                   :unselected-background unselected-bg
                   :unselected-foreground unselected-fg
                   :input-background      (access:accesses *software-configuration*
                                                         window-key
                                                         +key-input+
                                                         +key-background+)
                   :input-foreground      (access:accesses *software-configuration*
                                                         window-key
                                                         +key-input+
                                                         +key-foreground+))))
;;;;;; tests

(defun trivial-configuration-missing-value-check ()
  (loop for fn in (list
                   #'gemini-downloading-animation
                   #'gemini-default-favicon
                   #'gemini-link-prefix-to-gemini
                   #'gemini-link-prefix-to-other
                   #'gemini-quote-prefix
                   #'gemini-h1-prefix
                   #'gemini-h2-prefix
                   #'gemini-h3-prefix
                   #'gemini-bullet-prefix
                   #'gemini-subscription-url-fg
                   #'gemini-subscription-count-fg
                   #'signature-file-path
                   #'window-titles-ends
                   #'tags-new-message-mark
                   #'config-server-name
                   #'config-username
                   #'config-password-echo-character
                   #'config-win-focus-mark
                   #'command-separator-config-values
                   #'message-window-locked-account-mark
                   #'message-window-unlocked-account-mark
                   #'message-window-line-mark-values
                   #'message-window-attachments-header
                   #'config-post-allowed-language)
        do
           (funcall fn)))
