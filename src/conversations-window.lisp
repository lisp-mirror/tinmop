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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :conversations-window)

(defclass conversations-window (focus-marked-window
                                simple-line-navigation-window
                                title-window
                                border-window)
  ((read-message-fg
    :initarg  :read-message-fg
    :initform nil
    :accessor read-message-fg
    :documentation "Read message foreground color")
   (read-message-bg
    :initarg  :read-message-bg
    :initform nil
    :accessor read-message-bg
    :documentation "Read message background color")
   (unread-message-fg
    :initarg  :unread-message-fg
    :initform nil
    :accessor unread-message-fg
    :documentation "Unread message foreground color")
   (unread-message-bg
    :initarg  :unread-message-bg
    :initform nil
    :accessor unread-message-bg
    :documentation "unread message background color"))
  (:documentation "The window that shows active conversation"))

(defmethod refresh-config :after ((object conversations-window))
  (with-accessors ((croatoan-window croatoan-window)
                   (histogram-fg      histogram-fg)
                   (read-message-fg   read-message-fg)
                   (read-message-bg   read-message-bg)
                   (selected-line-bg  selected-line-bg)
                   (selected-line-fg  selected-line-fg)
                   (unread-message-fg unread-message-fg)
                   (unread-message-bg unread-message-bg)) object
    (let* ((theme-style       (swconf:form-style swconf:+key-conversations-window+))
           (fg                (swconf:foreground theme-style))
           (bg                (swconf:background theme-style))
           (selected-fg       (swconf:selected-foreground theme-style))
           (selected-bg       (swconf:selected-background theme-style))
           (width             (- (win-width *main-window*)
                                 (win-width *thread-window*)))
           (y                 (win-height *tags-window*))
           (height            (- (win-height *main-window*)
                                 (win-height *command-window*)
                                 (win-height *tags-window*)))
           (x                 0))
      (multiple-value-bind (fg-read bg-read)
          (swconf:conversation-window-read-colors)
        (multiple-value-bind (fg-unread bg-unread)
            (swconf:conversation-window-unread-colors)
          (setf read-message-bg   bg-read)
          (setf read-message-fg   fg-read)
          (setf unread-message-bg bg-unread)
          (setf unread-message-fg fg-unread)
          (setf (background croatoan-window)
                (tui:make-background bg))
          (setf (bgcolor croatoan-window) bg)
          (setf (fgcolor croatoan-window) fg)
          (setf selected-line-fg selected-fg)
          (setf selected-line-bg selected-bg)
          (win-resize object width height)
          (win-move object x y)
          object)))))

(defmethod draw :before ((object conversations-window))
  (with-accessors ((rows              rows)
                   (single-row-height single-row-height)
                   (top-row-padding   top-row-padding)
                   (read-message-fg   read-message-fg)
                   (read-message-bg   read-message-bg)
                   (unread-message-fg unread-message-fg)
                   (unread-message-bg unread-message-bg)) object
    (win-clear object)
    (with-croatoan-window (croatoan-window object)
      (loop
         for y from (+ 2 top-row-padding) by single-row-height
         for row-fields  in (mapcar #'fields rows) do
           (let ((attributes-to-read (if (= (db:messages-to-read row-fields)
                                            0)
                                         (attribute-dim)
                                         (attribute-bold))))
             (print-text object
                         (text-utils:to-s (db:messages-to-read row-fields))
                         1 y
                         :bgcolor unread-message-bg
                         :fgcolor unread-message-fg
                         :attributes attributes-to-read)
             (print-text object "/" nil nil)
             (print-text object
                         (text-utils:to-s (+ (db:messages-to-read row-fields)
                                             (db:messages-red row-fields)))
                         nil nil
                         :bgcolor read-message-bg
                         :fgcolor read-message-fg))))))


(defmethod resync-rows-db ((object conversations-window) &key
                                                           (redraw t)
                                                           (suggested-message-index nil))
  "Resync   this  window   ehit  he   conversation  in   database,  if
`suggested-message-index' is not nil masrks as selected the message in
position indicated by this variable."
  (with-accessors ((rows             rows)
                   (selected-line-bg selected-line-bg)
                   (selected-line-fg selected-line-fg)) object
    (flet ((make-rows (line-fields bg fg)
             (mapcar (lambda (fields)
                       (let ((name (db:conversation-name fields)))
                         (make-instance 'line
                                        :fields        fields
                                        :normal-text   name
                                        :selected-text name
                                        :normal-bg     bg
                                        :normal-fg     fg
                                        :selected-bg   fg
                                        :selected-fg   bg)))
                     line-fields)))
      (let ((line-fields (db:all-conversation-stats)))
        (with-croatoan-window (croatoan-window object)
          (setf rows (make-rows line-fields
                                selected-line-bg
                                selected-line-fg))
          (when suggested-message-index
            (select-row object suggested-message-index))
          (when redraw
            (draw object)))))))

(defun init ()
  "Init the window"
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *conversations-window*
          (make-instance 'conversations-window
                         :title             (_ "Conversations")
                         :single-row-height 3
                         :uses-border-p     t
                         :keybindings       keybindings:*conversations-keymap*
                         :croatoan-window   low-level-window))
    (refresh-config *conversations-window*)
    (resync-rows-db *conversations-window* :redraw nil)
    (when (rows *conversations-window*)
      (select-row *conversations-window* 0))
    (draw *conversations-window*)
    *conversations-window*))
