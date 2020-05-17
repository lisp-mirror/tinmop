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

(in-package :open-message-link-window)

(defclass open-message-link-window (open-attach-window:open-attach-window) ())

(defmethod refresh-config :after ((object open-message-link-window))
  (open-attach-window:refresh-view-links-window-config object
                                                       swconf:+key-open-message-link-window+))

(defmethod resync-rows-db ((object open-message-link-window) &key
                                                               (redraw t)
                                                               (suggested-message-index nil))
  (with-accessors ((rows             rows)
                   (status-id        open-attach-window:status-id)
                   (selected-line-bg selected-line-bg)
                   (selected-line-fg selected-line-fg)) object
    (flet ((make-rows (links bg fg)
             (mapcar (lambda (link)
                       (make-instance 'line
                                      :normal-text   link
                                      :selected-text link
                                      :normal-bg     bg
                                      :normal-fg     fg
                                      :selected-bg   fg
                                      :selected-fg   bg))
                     links)))
      (let* ((message (db:find-status-id status-id))
             (links   (text-utils:collect-links (db:row-message-rendered-text message))))
        (with-croatoan-window (croatoan-window object)
          (setf rows (make-rows links
                                selected-line-bg
                                selected-line-fg))
          (when suggested-message-index
            (select-row object suggested-message-index))
          (when redraw
            (draw object)))))))

(defun init (status-id)
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *open-message-link-window*
          (make-instance 'open-message-link-window
                         :title             (_ "Links")
                         :status-id         status-id
                         :single-row-height 1
                         :uses-border-p     t
                         :keybindings       keybindings:*open-message-link-keymap*
                         :croatoan-window   low-level-window))
    (refresh-config *open-message-link-window*)
    (resync-rows-db *open-message-link-window* :redraw nil)
    (when (rows *open-message-link-window*)
      (select-row *open-message-link-window* 0))
    (draw *open-message-link-window*)
    *open-message-link-window*))

(defun open-message-link (url)
  (os-utils:xdg-open url))
