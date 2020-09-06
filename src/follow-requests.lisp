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

(in-package :follow-requests)

(defclass follow-requests-window (focus-marked-window simple-line-navigation-window)
  ((requests
    :initarg  :requests
    :initform ()
    :accessor requests
    :type tooter:account
    :documentation "All the accounts that request to follow you")
   (header-message-lines
    :initarg  :header-message-lines
    :initform ()
    :accessor header-message-lines
    :documentation "lines of text printed on top of the window")
   (screen
    :initarg  :screen
    :initform nil
    :accessor screen
    :documentation "A reference to the main window (the screen)")
   (style
    :initarg  :style
    :initform nil
    :accessor style
    :documentation "The visual style of the window")))

(defmethod refresh-config :after ((object follow-requests-window))
  (with-accessors ((screen                screen)
                   (croatoan-window       croatoan-window)
                   (bgcolor               bgcolor)
                   (fgcolor               fgcolor)
                   (top-row-padding       top-row-padding)
                   (header-message-lines  header-message-lines)
                   (style                 style)) object
    (let* ((theme-style    (swconf:form-style swconf:+key-input-dialog+))
           (fg             (swconf:foreground theme-style))
           (bg             (swconf:background theme-style))
           (width          (truncate (/ (win-width screen)
                                        3)))
           (height         (truncate (/ (win-height screen)
                                        3)))
           (y              (truncate (- (/ (win-height screen) 2)
                                        (/ height 2))))
           (x              (truncate (- (/ (win-width  screen) 2)
                                        (/ width 2)))))
      (setf (background croatoan-window)
            (tui:make-win-background bg))
      (setf (bgcolor croatoan-window) bg)
      (setf (fgcolor croatoan-window) fg)
      (setf style theme-style)
      (win-resize object width height)
      (win-move object x y)
      (let* ((header         (_ "Please evaluate the following requests, only items shown below will be accepted, deleted ones will be rejected:"))
             (header-words   (text-utils:split-words header))
             (header-lines   (text-utils:flush-left-mono-text header-words
                                                              (win-width-no-border object)))
             (attach-y-start (1+ (length header-lines))))
        (setf top-row-padding attach-y-start)
        (setf header-message-lines header-lines))
      object)))

(defmethod draw :after ((object follow-requests-window))
  (with-accessors ((style                style)
                   (header-message-lines header-message-lines)) object
    (with-croatoan-window (croatoan-window object)
      (let* ((bgcolor   (bgcolor croatoan-window))
             (fgcolor   (fgcolor croatoan-window))
             (win-width (win-width-no-border object)))
        (loop
           for y from 1
           for line in header-message-lines do
             (print-text object
                         (text-utils:right-padding line win-width)
                         1 y
                         :fgcolor    fgcolor
                         :bgcolor    bgcolor
                         :attributes (attribute-bold)))))))

(defun init (follow-requests usernames-follow-requests screen)
  "Initialize the window

- follows-requests the account entity (from tooter library) that requestes to follow you
- username-follow-requests the username of the accounts that requestes to follow you
- screen the main window
"
  (flet ((make-rows (usernames bg fg)
           (mapcar (lambda (username)
                     (make-instance 'line
                                    :normal-text   username
                                    :selected-text username
                                    :normal-bg     bg
                                    :normal-fg     fg
                                    :selected-bg   fg
                                    :selected-fg   bg))
                   usernames)))
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *follow-requests-window*
          (make-instance 'follow-requests-window
                         :requests        follow-requests
                         :uses-border-p   t
                         :screen          screen
                         :keybindings     keybindings:*follow-requests-keymap*
                         :croatoan-window low-level-window))
    (refresh-config *follow-requests-window*)
    (setf (rows *follow-requests-window*)
          (make-rows usernames-follow-requests
                     (bgcolor low-level-window)
                     (fgcolor low-level-window)))
    (setf (row-selected-index *follow-requests-window*) 0)
    *follow-requests-window*)))

(defun process-requests ()
  "Process  the accepted  or follow'  requests, the  accepted are  the
requeste  that are not be erased from the window (see the class
row-oriented-widget)"
  (with-accessors ((all-accounts requests)
                   (rows         rows)) specials:*follow-requests-window*
    (let* ((accepted-usernames (mapcar #'normal-text rows))
           (accepted-accounts  (remove-if-not (lambda (acc)
                                                (find-if (lambda (a)
                                                           (string= a
                                                                    (tooter:account-name acc)))
                                                         accepted-usernames))
                                              all-accounts))
           (rejected-accounts  (set-difference all-accounts
                                               accepted-accounts
                                               :key  #'tooter:id
                                               :test #'string=)))
      (loop for accepted-account in accepted-accounts do
           (let ((id (tooter:id accepted-account)))
             (api-client:accept-follow-request id)))
      (loop for rejected-account in rejected-accounts do
           (let ((id (tooter:id rejected-account)))
             (api-client:reject-follow-request id))))))
