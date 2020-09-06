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

(in-package :keybindings)

(define-constant +control-prefix+             "^"                            :test #'string=)

(define-constant +meta-prefix+                "ALT-"                         :test #'string=)

(define-constant +function-placeholder-re+    "function-placeholder"         :test #'string=)

(define-constant +function-placeholder-value+ :fn                            :test #'string=)

(defparameter   *default-prefix*              nil)

;; KEYPATH              := KEY KEYPATH
;;                         | epsilon
;; KEY                  := COMMAND-KEY
;;                         | META-KEY
;;                         | KEYCODE
;;                         | FUNCTION-PLACEHOLDER
;; COMMAND-KEY          := ( (COMMAND-MOD-1 DASH) | COMMAND-MOD-2 ) KEYCODE
;; META-KEY             := META-MOD DASH KEYCODE
;; KEYCODE              := NON-PRINTABLE-KEY | SIMPLE-KEY
;; SIMPLE-KEY           := CHAR
;; NON-PRINTABLE-KEY    := |'f1'
;;                         |'f2'
;;                         |'f3'
;;                         |'f4'
;;                         |'f5'
;;                         |'f6'
;;                         |'f7'
;;                         |'f8'
;;                         |'f9'
;;                         |'f10
;;                         | 'right'
;;                         | 'left'
;;                         | 'up'
;;                         | 'down'
;;                         | 'home'
;;                         | 'end'
;;                         | 'npage'
;;                         | 'ppage'
;;                         | "dc"
;; COMMAND-MOD-1        := 'C'
;; COMMAND-MOD-2        := '^'
;; META-MOD             := 'M'
;; DASH                 := '-'
;; CHAR                 := ESCAPED-CHAR | (not ( DASH | BLANK))
;; BLANKS               := BLANK*
;; BLANK                := #\space | #\Newline | #\Tab
;; FUNCTION-PLACEHOLDER := 'function-placeholder'


(defrule function-placeholder "function-placeholder"
  (:constant +function-placeholder-value+))

(defrule blank (or #\space #\Newline #\Tab)
  (:constant nil))

(defrule blanks (* blank)
  (:constant nil))

(defrule escaped-character (and #\\ character)
  (:function (lambda (a) (list (second a)))))

(defrule dash #\-
  (:text t))

(defrule non-printable-key
    (or "f10"
        "f1"
        "f2"
        "f3"
        "f4"
        "f5"
        "f6"
        "f7"
        "f8"
        "f9"
        "right"
        "left"
        "up"
        "down"
        "home"
        "end"
        "npage" ; page down
        "ppage" ; page up
        "dc")   ; canc
  (:text t)
  (:function string-upcase))

(defrule char
    (or escaped-character
        (not (or dash blank)))
  (:text t))

(defrule meta-mod #\M
  (:text t))

(defrule command-mod-1 #\C)

(defrule command-mod-2 #\^)

(defrule simple-key char)

(defrule keycode (or non-printable-key simple-key)) ; keep the order

(defun to-meta-code-string (command)
  (strcat +meta-prefix+ (string-upcase (third command))))

(defrule meta-key
    (and meta-mod dash keycode)
  (:function to-meta-code-string)
  (:text t))

(defun to-control-code-string (command)
  (strcat +control-prefix+ (string-upcase (third command))))

;; not part of the actual grammar, just syntactic sugar

(defrule command-key-1
    (and command-mod-1 dash keycode)
  (:function to-control-code-string))

(defrule command-key-2
    (and command-mod-2 keycode)
  (:text t))

(defrule command-key
    (or command-key-1
        command-key-2))

(defrule key
    (and (or function-placeholder ; keep the order
             command-key
             meta-key
             keycode)
         (? blanks))
  (:function first))

(defrule keypath
    (and key (? keypath))
  (:function (lambda (a) (remove-if-null (flatten a)))))

(defun make-starting-comand-tree ()
  (make-command-tree *default-prefix*))

(defun make-command-tree (data)
  (make-instance 'sorted-m-tree
                 :data       data
                 :compare-fn #'string<))

(defun parse-keypath (keypath &key (existing-tree (make-starting-comand-tree)))
  "Parse a string representing a list of keys (see `*global-keymap*'),
produces a tree and graft the latter on `existing-tree'"
  (labels ((placeholderp (child)
             (eq +function-placeholder-value+
                 (data child)))
           (remove-function-siblings (node)
             (top-down-visit node
                             (lambda (a)
                               (if (find-if #'placeholderp (children a))
                                   (setf (children a)
                                         (remove-if-not #'placeholderp (children a)))))))
           (remove-function-children (node)
             (top-down-visit node
                             (lambda (a)
                               (when (placeholderp a)
                                 (remove-all-children a)))))
           (add (tree commands)
             (if commands
                 (let ((new-node (make-command-tree (first commands))))
                   (add-child tree new-node)
                   (add new-node (rest commands)))
                 nil)))
    (let ((raw      (parse 'keypath (strcat keypath " " +function-placeholder-re+)))
          (new-tree (make-starting-comand-tree)))
      (add  new-tree raw)
      (graft-branch existing-tree
                    new-tree
                    :test (lambda (a b)
                            (cond
                              ((functionp a)
                               t)
                              ((eq b +function-placeholder-value+)
                               t)
                              (t
                               (string= a b)))))
      (remove-function-siblings existing-tree)
      (remove-function-children existing-tree)
      existing-tree)))

(defparameter *global-keymap* (make-starting-comand-tree)
  "The global keymap.

   A keymap looks like a tree with function as leaf nodes like:

       a
      / \
     b   c
    /   / \
   #'+ b   d
      /     \
      #'*    #'-

  So, pressing a sequence of 'a -> c -> d' will trigger the function #'-

")

(defparameter *thread-keymap* (make-starting-comand-tree)
  "The keymap for thread window.")

(defparameter *message-keymap* (make-starting-comand-tree)
  "The keymap for message window.")

(defparameter *gemini-message-keymap* (make-starting-comand-tree)
  "The keymap for message-window when displaing gemini text.")

(defparameter *tags-keymap* (make-starting-comand-tree)
  "The keymap for tags window.")

(defparameter *conversations-keymap* (make-starting-comand-tree)
  "The keymap for conversations windows.")

(defparameter *send-message-keymap* (make-starting-comand-tree)
  "The keymap for window to confirm sending a message.")

(defparameter *follow-requests-keymap* (make-starting-comand-tree)
  "The keymap for window to accept follow requests.")

(defparameter *open-attach-keymap* (make-starting-comand-tree)
  "The keymap for window to open message's attachments.")

(defparameter *open-message-link-keymap* (make-starting-comand-tree)
  "The keymap for window to open message's links.")

(defparameter *open-gemini-link-keymap* (make-starting-comand-tree)
  "The keymap for window to open gemini's links.")

(defparameter *gemini-downloads-keymap* (make-starting-comand-tree)
  "The keymap for window that shows all gemini streams.")

(defparameter *chats-list-keymap* (make-starting-comand-tree)
  "The keymap for window that shows all the chats.")

(defun define-key (key-sequence function &optional (existing-keymap *global-keymap*))
  "Define a key sequence that trigger a function:

   The syntax is a simple list of keys or a single key:

   - key [key ...]

   where   key    can   be    either   a   printable    character,   a
   <control>-character code or ALT-character.  A character code can be
   defined using  caret notation:  '^character' (e.g.  '^A')  or using
   'C-' as placeholder for <control> (e.g. 'C-A').

   The allowed character in alt-character code are downcase only.

   Please note that this function will modify existing keymap.
"
  (let* ((tree        (parse-keypath key-sequence :existing-tree existing-keymap))
         (placeholder (find-child tree +function-placeholder-value+ :compare #'eq)))
    (assert placeholder)
    (assert (functionp function))
    (setf (data placeholder) function)
    tree))

(define-constant +croatoan-last-standard-key+ 511 :test #'=)

(defclass encoded-map-entry ()
  ((terminal-code
    :initform nil
    :initarg  :terminal-code
    :accessor terminal-code)
   (croatoan-code
    :initform nil
    :initarg  :croatoan-code
    :accessor croatoan-code)
   (curses-code
    :initform nil
    :initarg  :curses-code
    :accessor curses-code))
  (:documentation "terminal-code:  the raw string that the terminal provide to encode a key
   croatoan-code:  ther symbol croatoan use and returns to  usercode
   curses-keycode: integer that map terminal-code
   (used internally by croatoan to match croatoan-code
   Example: \"^[1\" :alt-1 512.
   Note that \"^]\" is the character #\Esc"))

(defmethod print-object ((object encoded-map-entry) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-accessors ((terminal-code terminal-code)
                     (croatoan-code croatoan-code)
                     (curses-code   curses-code)) object
      (format stream "~s ~s ~s" terminal-code croatoan-code curses-code))))

(defun make-encoded-map-entry (terminal-code croatoan-code curses-code)
  (make-instance 'encoded-map-entry
                 :terminal-code terminal-code
                 :croatoan-code croatoan-code
                 :curses-code   curses-code))

(defun term-escaped-sequence (&rest chars)
  (append (list #\Esc)
          chars))

(defun other-codes ()
    "Others keycode (the terminal encoding is non standard and will requires tweak).
Values a list of `encoded-map-entry'"
    (flet ((collect-codes (from-ascii-code below-ascii-code from-curses-code)
             (loop
                for character-code from from-ascii-code  below below-ascii-code by 1
                for curses-code    from from-curses-code                        by 1 collect
                  (let ((char (code-char character-code)))
                    (make-encoded-map-entry (format nil "~a~a" #\Esc char)              ; terminal
                                            (format-keyword (format nil "alt-~a" char)) ; croatoan
                                            curses-code)))))                            ; curses
      (let* ((number-and-symbols (collect-codes 33 65 (1+ +croatoan-last-standard-key+)))
             (last-curses-code   (curses-code         (last-elt number-and-symbols)))
             (characters         (collect-codes 97
                                                127
                                                (1+ last-curses-code))))
        (setf last-curses-code (curses-code (last-elt characters)))
        (let ((misc (list (make-encoded-map-entry (term-escaped-sequence #\[ #\1 #\; #\3 #\D)
                                                  :alt-left
                                                  (+ 1 last-curses-code))
                          (make-encoded-map-entry (term-escaped-sequence #\[ #\1 #\; #\3 #\A)
                                                  :alt-up
                                                  (+ 2 last-curses-code))
                          (make-encoded-map-entry (term-escaped-sequence #\[ #\1 #\; #\3 #\C)
                                                  :alt-right
                                                  (+ 3 last-curses-code))
                          (make-encoded-map-entry (term-escaped-sequence #\[ #\1 #\; #\3 #\B)
                                                  :alt-down
                                                  (+ 4 last-curses-code))
                          (make-encoded-map-entry (term-escaped-sequence #\[ #\1 #\; #\5 #\D)
                                                  :control-left
                                                  (+ 5 last-curses-code))
                          (make-encoded-map-entry (term-escaped-sequence #\[ #\1 #\; #\5 #\A)
                                                  :control-up
                                                  (+ 6 last-curses-code))
                          (make-encoded-map-entry (term-escaped-sequence #\[ #\1 #\; #\5 #\C)
                                                  :control-right
                                                  (+ 7 last-curses-code))
                          (make-encoded-map-entry (term-escaped-sequence #\[ #\1 #\; #\5 #\B)
                                                  :control-down
                                                  (+ 8 last-curses-code)))))
        (append number-and-symbols characters misc)))))

(defparameter *local-key-alist* (other-codes))

(defun add-keymap-to-curses-db ()
  "Remove  nonstandard key  form 'croatoan:*key-alist*'  and add  ours
instead"
  (loop for keymapping in *local-key-alist* do
       (croatoan:define-function-key (croatoan-code keymapping) (terminal-code keymapping)
         :key-code (curses-code keymapping))))

(defun init-keyboard-mapping ()
  (add-keymap-to-curses-db))

(defun find-keymap-node (key tree)
  "Find `key' in keymap `tree'.
   Returns the node  of the tree or a function if the node found node
   has a single children (the function to call).
   If `key' can not be found returns nil.
"
  (when-let* ((node-pos (num:binary-search (children tree)
                                           (make-node key)
                                           :compare-fn (lambda (a b)
                                                         (string< (data a) (data b)))
                                           :equal-fn   (lambda (a b)
                                                         (string= (data a) (data b)))))
              (node     (interfaces:clone (elt (children tree) node-pos))))
    (assert (or (null node)
                (not  (leafp node))))
    (setf (parent node) nil)
    (let* ((child-with-function-to-call (find-if (lambda (child) (functionp (data child)))
                                                 (children node)))
           (function-to-call            (and child-with-function-to-call
                                             (data child-with-function-to-call))))
      (or function-to-call
          node))))

(defun humanize-key (key)
  "Transform a node  of the keymap in something that  humans can easly
understand"
  (cond
    ((functionp key)
     (if (documentation key t)
         (with-input-from-string (stream (documentation key t))
           (read-line stream))
         (function-name key)))
    ((string= key "^J")
     (_ "Enter"))
    ((string= key "DC")
     (_ "Delete"))
    ((string= key "NPAGE")
     (_ "Page-up"))
    ((string= key "PPAGE")
     (_ "Page-down"))
    (t
     (to-s key))))

(defun key-paths (keymapping-tree)
  "Transform a keymap tree in a list  of path from root of the tree to
each leaf (as strings)"
  (let ((all-texts     ())
        (all-functions ()))
    (labels ((collect-fn (&optional (starting-path ()))
               (let ((path (copy-list starting-path)))
                 (lambda (node)
                   (let ((data (data node)))
                     (push data path)
                     (if (functionp data)
                         (progn
                           (push (reverse path) all-texts)
                           (push data all-functions))
                         path)))))
             (collect (node &optional (collector (collect-fn)))
               (let* ((path-so-far   (funcall collector node)))
                 (do-children (child node)
                   (let ((new-collector (collect-fn path-so-far)))
                     (collect child new-collector)))))
             (humanize-paths (paths)
               (loop for path in paths collect
                    (mapcar #'humanize-key (rest path))))
             (build-string (paths)
               (mapcar (lambda (a) (format nil "~a" (join-with-strings a " ")))
                       paths)))
      (collect keymapping-tree)
      (mapcar #'make-help-fields
              (build-string (humanize-paths all-texts))
              all-functions))))

(defun help-fields-get-function (fields)
  (getf fields :function))

(defun help-fields-get-text (fields)
  (getf fields :text))

(defun make-help-fields (text function)
  (list :text text :function function))

(defun help-expand (x fields)
  "Expands an entry in quick help window (see function `print-help'
and `make-blocking-list-dialog-window') showing the full docstring for a command"
  (declare (ignore x))
  (when-let* ((function (help-fields-get-function fields))
              (bg       (swconf:win-bg swconf:+key-help-dialog+))
              (fg       (swconf:win-fg swconf:+key-help-dialog+)))
    (windows:make-blocking-message-dialog specials:*main-window*
                                          nil
                                          (function-name function)
                                          (if (string-not-empty-p (documentation function t))
                                              (split-lines (documentation function t))
                                              (list (_ "No documentation available, you can help! :-)")))
                                          bg
                                          fg)))

(defun print-help (main-window)
  "Generate an help text for the focused window and main window"
  (multiple-value-bind (header-bg header-fg attribute-header)
      (swconf:quick-help-header-colors)
    (labels ((colorize-header (a)
               (tui-utils:make-tui-string a
                                          :fgcolor    header-fg
                                          :bgcolor    header-bg
                                          :attributes attribute-header))
             (sort-help (help-values)
               (sort help-values
                     (lambda (a b)
                       (let* ((text-a    (help-fields-get-text a))
                              (text-b    (help-fields-get-text b))
                              (alpha-a-p (scan "(?i)^[a-z]" text-a))
                              (alpha-b-p (scan "(?i)^[a-z]" text-b)))
                         (cond
                           ((and alpha-a-p
                                 alpha-b-p)
                            (string< text-a text-b))
                           (alpha-a-p
                            t)
                           (alpha-b-p
                            nil)
                           (t
                            (string< text-a text-b))))))))
      (when-let* ((focused-keybindings   (main-window:focused-keybindings main-window))
                  (global-help           (sort-help (key-paths *global-keymap*)))
                  (header-focused        (colorize-header (_ "Focused window keys")))
                  (header-global         (colorize-header  (_ "Global keys")))
                  (focused-help          (sort-help (key-paths focused-keybindings)))
                  (global-header-fields  (make-help-fields header-global nil))
                  (focused-header-fields (make-help-fields header-focused nil))
                  (fields                (list focused-header-fields)))
        (setf fields
              (append fields
                      focused-help
                      (list global-header-fields)
                      global-help))
        (let ((all-lines (mapcar #'help-fields-get-text
                                 fields)))
          (line-oriented-window:make-blocking-list-dialog-window specials:*main-window*
                                                                 fields
                                                                 all-lines
                                                                 #'help-expand
                                                                 (_ "Quick help")))))))
