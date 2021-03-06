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

;; CONFIG                := (ENTRIES)*
;; ENTRIES               := COMMENT*
;;                         (USE-FILE
;;                          | IGNORE-USER-RE-ASSIGN
;;                          | COLOR-RE-ASSIGN
;;                          | SERVER-ASSIGN
;;                          | USERNAME-ASSIGN
;;                          | GENERIC-ASSIGN)
;;                         COMMENT*
;; SERVER-ASSIGN         := SERVER-KEY BLANKS ASSIGN BLANKS GENERIC-VALUE BLANKS
;; USERNAME-ASSIGN       := USERNAME-KEY BLANKS ASSIGN BLANKS GENERIC-VALUE BLANKS
;; GENERIC-ASSIGN        := (and key blanks assign blanks
;;                           (or quoted-string
;;                               hexcolor
;;                               colorname
;;                               generic-value) ; the order in this list *is* important
;;                         blanks)
;; IGNORE-USER-RE-ASSIGN := IGNORE-USER-RE-KEY ASSIGN REGEXP
;; COLOR-RE-ASSIGN       := COLOR-RE-KEY ASSIGN REGEXP FG-COLOR (? ATTRIBUTE-VALUE)
;; USE-FILE              := (AND USE BLANKS FILEPATH BLANKS)
;; KEY                   := FIELD (FIELD-SEPARATOR KEY)*
;; BLANKS                := (BLANK)*
;; FILEPATH              := QUOTED-STRING
;; USE                   := "use"
;; SERVER-KEY            := "server"
;; USERNAME-KEY          := "username"
;; COLOR-RE-KEY          := "color-regexp"
;; IGNORE-USER-RE-KEY    := "ignore-user-regexp"
;; REGEXP                := QUOTED-STRING
;; QUOTED-STRING         := #\" (not #\") #\"
;; FIELD                 := ( (or ESCAPED-CHARACTER
;;                               (not #\# ASSIGN BLANK FIELD-SEPARATOR) )*
;; COMMENT               := BLANKS #\# (not #\Newline)* BLANKS
;; FIELD-SEPARATOR       := #\.
;; GENERIC-VALUE         := KEY
;; ASSIGN                := #\=
;; BLANK                 := (or #\space #\Newline #\Tab)
;; BG-COLOR              := COLOR
;; FG-COLOR              := COLOR
;; COLOR                 := HEX-COLOR | COLOR-NAME
;; HEX-COLOR             := HEXCOLOR-PREFIX
;;                         HEXDIGIT HEXDIGIT -> red
;;                         HEXDIGIT HEXDIGIT -> green
;;                         HEXDIGIT HEXDIGIT -> blue
;; ESCAPED-CHARACTER     := #\\ any-character
;; HEXCOLOR-PREFIX       := #\#
;; HEX-DIGIT             := (and (character-ranges #\0 #\9)
;;                              (character-ranges #\a #\f)
;;                              (character-ranges #\A #\f)
;; ATTRIBUTE-VALUE       := "bold"
;;                         | "italic"
;;                         | "underline"
;;                         | "blink"
;; COLOR-NAME            := "black"
;;                         | "red"
;;                         | "green"
;;                         | "yellow"
;;                         | "blue"
;;                         | "magenta"
;;                         | "cyan"
;;                         | "white"

(define-constant +conf-filename+         "main.conf"             :test #'string=)

(define-constant +shared-conf-filename+  "shared.conf"           :test #'string=)

(define-constant +field-separator-value+ "."                     :test #'string=)

(define-constant +field-separator+       :field-separator        :test #'eq)

(defrule blank (or #\space #\Newline #\Tab)
  (:constant nil))

(defrule blanks (* blank)
  (:constant nil))

(defrule assign #\=
  (:constant nil))

(defrule comment (and blanks #\# (* (not #\Newline)) blanks)
  (:constant nil))

(defrule hexcolor-prefix #\#)

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
          (make-color-re-assign re
                                (and color-name-p       color)
                                (and (not color-name-p) color)
                                attributes))))

(defrule attribute-value (or "bold"
                             "italic"
                             "underline"
                             "blink")
  (:text t)
  (:function tui-utils:text->tui-attribute))

(defrule color-re-assign
    (and color-re-key blanks
         assign blanks regexp blanks
         (or hexcolor colorname) blanks
         (? (and attribute-value blanks)))
  (:function remove-if-null)
  (:function build-color-re-assign))

(defrule ignore-user-re-key "ignore-user-regexp"
  (:constant :ignore-user-re))

(defrule ignore-user-re-assign
    (and ignore-user-re-key blanks
         assign blanks regexp blanks)
  (:function (lambda (a) (list (first a) (fifth a)))))

(defrule server-key "server"
  (:constant :server))

(defrule username-key "username"
  (:constant :username))

(defrule server-assign
    (and server-key blanks assign blanks generic-value blanks)
  (:function remove-if-null))

(defrule username-assign
    (and username-key blanks assign blanks generic-value blanks)
  (:function remove-if-null))

(defrule filepath quoted-string)

(defparameter *already-included-files* ())

(defrule use-file (and "use" blanks filepath blanks)
  (:function (lambda (a)
               (let ((file (third a)))
                 (if (find file *already-included-files* :test #'string=)
                     (error "Cyclic include of file ~s detected" file)
                     (progn
                       (push file *already-included-files*)
                       (load-config-file (third a))))
                 nil))))

(defrule entries
    (and (* comment)
         (or use-file
             color-re-assign
             ignore-user-re-assign
             server-assign
             username-assign
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

(defmacro gen-key-constant (name)
  `(define-constant ,(format-fn-symbol t "+key-~a+" name)
       ,(format-keyword name)
       :test #'eq))

(defmacro gen-key-constants (&rest names)
  `(progn
     ,@(loop for name in names collect
            `(gen-key-constant ,name))))

(gen-key-constants background
                   foreground
                   title
                   start
                   end
                   left
                   right
                   stopper
                   root
                   width
                   height
                   error
                   info
                   window
                   header
                   focus
                   prefix
                   postfix
                   value
                   scheme
                   link
                   creation-time
                   access-time
                   quote
                   h1
                   h2
                   h3
                   bullet
                   other
                   attribute
                   new-message
                   mark
                   vote-vertical-bar
                   crypted
                   histogram
                   error-dialog
                   info-dialog
                   input-dialog
                   help-dialog
                   notify-window
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
                   command-separator
                   gemini
                   tree
                   branch
                   arrow
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
                   deleted
                   input
                   read
                   unread
                   password-echo-character
                   color-re
                   ignore-user-re
                   purge-history-days-offset
                   purge-cache-days-offset)

(defun load-config-file (&optional (virtual-filepath +conf-filename+))
  (handler-case
      (progn
        (let* ((file (res:get-config-file virtual-filepath))
               (tree (parse-config (fs:namestring->pathname file))))
          (loop for entry in tree do
               (let ((key   (first  entry))
                     (value (second entry)))
                 (cond
                   ((or (eq +key-color-re+ key)
                        (eq +key-ignore-user-re+ key))
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
          *software-configuration*))
    (error (e)
      (format *error-output* "~a~%" e)
      (os-utils:exit-program 1))))

;;;; end of parser

(defparameter *allowed-status-visibility* '("public" "unlisted" "private" "direct")
  "- public   Visible to everyone, shown in public timelines;
   - unlisted Visible to public, but not included in public timelines;
   - private  Visible to followers only, and to any mentioned users;
   - direct   Visible only to mentioned users.")

(defparameter *allowed-attachment-type*   '("unknown" "image" "gifv" "video" "audio"))

(define-constant +default-signature-filename+ ".signature" :test #'string=)

;;;; interface

(defun gemini-link-prefix (scheme)
  (access:accesses *software-configuration*
                   +key-gemini+
                   +key-link+
                   +key-scheme+
                   scheme
                   +key-prefix+))

(defun gemini-link-prefix-to-gemini ()
  (gemini-link-prefix +key-gemini+))

(defun gemini-link-prefix-to-other ()
  (gemini-link-prefix +key-other+))

(defun gemini-quote-prefix ()
  (access:accesses *software-configuration*
                   +key-gemini+
                   +key-quote+
                   +key-prefix+))


(defun gemini-h*-prefix (level)
  (access:accesses *software-configuration*
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
  (access:accesses *software-configuration*
                   +key-gemini+
                   +key-bullet+
                   +key-prefix+))

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
  (access:accesses *software-configuration*
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
  (access:accesses *software-configuration*
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

(defun external-editor ()
  (access:accesses *software-configuration*
                   +key-editor+))

(defun color-regexps ()
  (access:accesses *software-configuration*
                   +key-color-re+))

(defun ignore-users-regexps ()
  (access:accesses *software-configuration*
                   +key-ignore-user-re+))

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

(gen-simple-access (server-name)
                   +key-server+)

(gen-simple-access (username)
                   +key-username+)

(gen-simple-access (password-echo-character)
                   +key-password-echo-character+)

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
          (access:accesses  *software-configuration*
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
          (access:accesses *software-configuration*
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


(defun tree-config-colors (tree-win-holder)
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
  (values (access:accesses *software-configuration*
                           tree-win-holder
                           +key-tree+
                           +key-arrow+
                           +key-value+)
          (access:accesses *software-configuration*
                           tree-win-holder
                           +key-tree+
                           +key-leaf+
                           +key-value+)
          (access:accesses *software-configuration*
                           tree-win-holder
                           +key-tree+
                           +key-branch+
                           +key-value+)
          (access:accesses *software-configuration*
                           tree-win-holder
                           +key-tree+
                           +key-spacer+
                           +key-value+)
          (access:accesses *software-configuration*
                           tree-win-holder
                           +key-tree+
                           +key-vertical-line+
                           +key-value+)))

(defun make-tree-colormap (window-key)
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
  (access:accesses *software-configuration*
                   window-key
                   +key-modeline+
                   +key-value+))

(defun date-fmt (window-key)
  (let* ((raw (access:accesses *software-configuration*
                                    window-key
                                    +key-date-format+
                                    +key-value+)))
    (date-formatter:expand-date-formatter-spec raw)))

(defun locked/unlocked-value (key-window locked)
  (let ((key-locked (if locked
                        +key-locked+
                        +key-unlocked+)))
    (access:accesses *software-configuration*
                     key-window
                     key-locked
                     +key-value+)))

(defun locked/unlocked-account-mark-value (key-window locked)
  (let ((key-locked (if locked
                        +key-locked+
                        +key-unlocked+)))
    (access:accesses *software-configuration*
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
  (values (access:accesses *software-configuration*
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
    :accessor selected-foreground)))

(defmethod print-object ((object form-style) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((background          background)
                     (foreground          foreground)
                     (input-background    input-background)
                     (input-foreground    input-foreground)
                     (selected-background selected-background)
                     (selected-foreground selected-foreground)) object
      (format stream
              "fg ~a bg ~a input-fg ~a input-bg ~a selected-fg ~a selected-bg ~a"
              foreground
              background
              input-foreground
              input-background
              selected-foreground
              selected-background))))

(defun form-style (window-key)
  (make-instance 'form-style
                 :background          (access:accesses *software-configuration*
                                                       window-key
                                                       +key-background+)
                 :foreground          (access:accesses *software-configuration*
                                                       window-key
                                                       +key-foreground+)
                 :selected-background (access:accesses *software-configuration*
                                                       window-key
                                                       +key-input+
                                                       +key-selected+
                                                       +key-background+)
                 :selected-foreground (access:accesses *software-configuration*
                                                       window-key
                                                       +key-input+
                                                       +key-selected+
                                                       +key-foreground+)
                 :input-background    (access:accesses *software-configuration*
                                                       window-key
                                                       +key-input+
                                                       +key-background+)
                 :input-foreground    (access:accesses *software-configuration*
                                                       window-key
                                                       +key-input+
                                                       +key-foreground+)))
