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

(in-package :sending-message)

(define-constant +header-send-window-height+ 5 :test #'=)

(defclass message-ready-to-send ()
  ((subject
    :initform nil
    :initarg  :subject
    :accessor subject)
   (attachments
    :initform ()
    :initarg  :attachments
    :accessor attachments)
   (reply-to
    :initform ()
    :initarg  :reply-to
    :accessor reply-to
    :documentation "The id  of table 'status' you are replying to.")
   (visibility
    :initform +status-public-visibility+
    :initarg  :visibility
    :accessor visibility
    :documentation "One of swconf:*allowed-status-visibility*.")
   (body
    :initform nil
    :initarg  :body
    :accessor body)))

(defmethod print-object ((object message-ready-to-send) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((subject     subject)
                     (attachments attachments)
                     (reply-to    reply-to)
                     (body        body)) object
      (format stream
              "~@[subj: ~a ~] ~@[reply-to: ~a ~] ~@[attach: ~a ~] ~a"
              subject reply-to attachments body))))

(defclass confirm-sending-window (focus-marked-window simple-line-navigation-window)
  ((screen
    :initarg  :screen
    :initform nil
    :accessor screen)
   (message-data
    :initarg  :message-data
    :initform (make-instance 'message-ready-to-send)
    :accessor message-data
    :type     message-ready-to-send)
   (style
    :initarg  :style
    :initform nil
    :accessor style)))

(defmethod refresh-config :after ((object confirm-sending-window))
  (with-accessors ((screen          screen)
                   (croatoan-window croatoan-window)
                   (bgcolor         bgcolor)
                   (fgcolor         fgcolor)
                   (top-row-padding top-row-padding)
                   (style           style)) object
    (let* ((theme-style    (swconf:form-style swconf:+key-input-dialog+))
           (fg             (swconf:foreground theme-style))
           (bg             (swconf:background theme-style))
           (width          (truncate (/ (win-width screen)
                                        2)))
           (height         (truncate (/ (win-height screen)
                                        2)))
           (y              (truncate (- (/ (win-height screen) 2)
                                        (/ height 2))))
           (x              (truncate (- (/ (win-width  screen) 2)
                                        (/ width 2))))
           (attach-y-start +header-send-window-height+))
      (setf (background croatoan-window)
            (tui:make-background bg))
      (setf (bgcolor croatoan-window) bg)
      (setf (fgcolor croatoan-window) fg)
      (setf style theme-style)
      (win-resize object width height)
      (win-move object x y)
      (setf (top-row-padding object) attach-y-start)
      object)))

(defmethod draw :after ((object confirm-sending-window))
  (with-accessors ((message-data  message-data)
                   (style         style)) object
    (with-accessors ((reply-to    reply-to)
                     (attachments attachments)
                     (body        body)
                     (subject     subject)
                     (visibility  visibility)) message-data
      (with-croatoan-window (croatoan-window object)
        (let* ((bgcolor                     (bgcolor croatoan-window))
               (fgcolor                     (fgcolor croatoan-window))
               (input-bg                    (swconf:input-background style))
               (input-fg                    (swconf:input-foreground style))
               (user                        (if reply-to
                                                (db:status-id->username reply-to)
                                                (_ "none")))
               (label-reply-raw             (_ "Reply to: "))
               (label-subject-raw           (_ "Subject:"))
               (label-visibility-raw        (_ "Visibility:"))
               (label-reply-length-raw      (length label-reply-raw))
               (label-subject-raw-length    (length label-subject-raw))
               (label-visibility-raw-length (length label-visibility-raw))
               (max-field-length            (max label-reply-length-raw
                                                 label-subject-raw-length
                                                 label-visibility-raw-length))
               (label-subject               (text-utils:right-padding label-subject-raw
                                                                      max-field-length))
               (label-reply                 (text-utils:right-padding label-reply-raw
                                                                      max-field-length))
               (label-visibility            (text-utils:right-padding label-visibility-raw
                                                                      max-field-length))
               (value-max-length            (- (win-width-no-border object)
                                               max-field-length))
               (label-attachments           (_ "Attachments")))
          (flet ((print-field (text x y bg fg &key (inverse nil))
                   (print-text object text x y
                               :bgcolor (if inverse
                                            fg
                                            bg)
                               :fgcolor (if inverse
                                            bg
                                            fg))))
            (print-field label-reply 1 1 bgcolor fgcolor)
            (print-field (right-padding user
                                        value-max-length)
                         max-field-length
                         1
                         input-bg input-fg)
            (print-field label-subject 1 2 bgcolor fgcolor)
            (print-field (right-padding subject
                                        value-max-length)
                         max-field-length
                         2
                         input-bg input-fg)
            (print-field label-visibility 1 3 bgcolor fgcolor)
            (print-field (right-padding visibility
                                        value-max-length)
                         max-field-length
                         3
                         input-bg input-fg)
            (print-field (right-padding (text-ellipsize label-attachments
                                                        (win-width-no-border object))
                                        (win-width-no-border object))
                         1
                         4
                         bgcolor fgcolor
                         :inverse t)))))))

(defun init (message-data screen)
  (flet ((make-rows (data bg fg)
           (mapcar #'(lambda (a)
                       (make-instance 'line
                                      :normal-text   a
                                      :selected-text a
                                      :normal-bg     bg
                                      :normal-fg     fg
                                      :selected-bg   fg
                                      :selected-fg   bg))
                   data)))
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *send-message-window*
          (make-instance 'confirm-sending-window
                         :uses-border-p   t
                         :screen          screen
                         :keybindings     keybindings:*send-message-keymap*
                         :croatoan-window low-level-window
                         :message-data    message-data))
    (refresh-config *send-message-window*)
    (setf (rows *send-message-window*)
          (make-rows (attachments message-data)
                     (bgcolor low-level-window)
                     (fgcolor low-level-window)))
    (setf (row-selected-index *send-message-window*) 0)
    *send-message-window*)))
