;; tinmop: an humble gemini and pleroma client
;; Copyright (C) 2021  cage

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

(in-package :gemini-subscription-window)

(defclass gemini-subscription-window (focus-marked-window
                                      simple-line-navigation-window
                                      title-window
                                      border-window)
  ())

(defmethod refresh-config :after ((object gemini-subscription-window))
  (open-attach-window:refresh-view-links-window-config object
                                                       swconf:+key-gemini-subscription-window+)
  (refresh-config-sizes object swconf:+key-thread-window+)
  (win-move object
            (- (win-width *main-window*)
               (win-width object))
            0)
  (win-move object
            (- (win-width *main-window*)
               (win-width object))
            0)
  (adjust-win-vertical-positioning-if-gemini-fullscreen object)
  object)

(defun gemlog->text (gemlog-db-row window)
  (format nil
          "~a ~s ~a/~a"
          (tui:text-ellipsis (db:row-title gemlog-db-row)
                             (truncate (/ (win-width window)
                                          3)))
          (if (db:row-subtitle gemlog-db-row)
              (tui:text-ellipsis (db:row-subtitle gemlog-db-row)
                                 (truncate (/ (win-width window)
                                              3)))
              (_ "No subtitle"))
          (db:row-unseen-count gemlog-db-row)
          (+ (db:row-unseen-count gemlog-db-row)
             (db:row-seen-count   gemlog-db-row))))

(defmethod resync-rows-db ((object gemini-subscription-window)
                           &key
                             (redraw t)
                             (suggested-message-index nil))
  (with-accessors ((rows             rows)
                   (selected-line-bg selected-line-bg)
                   (selected-line-fg selected-line-fg)) object
    (flet ((make-rows (gemlogs bg fg)
             (mapcar (lambda (gemlog)
                       (make-instance 'line
                                      :normal-text   (gemlog->text gemlog object)
                                      :selected-text (gemlog->text gemlog object)
                                      :fields        gemlog
                                      :normal-bg     fg
                                      :normal-fg     bg
                                      :selected-bg   bg
                                      :selected-fg   fg))
                     gemlogs)))
      (with-croatoan-window (croatoan-window object)
        (line-oriented-window:update-all-rows object
                                              (make-rows (db:gemini-all-subscriptions)
                                                         selected-line-bg
                                                         selected-line-fg))
        (when suggested-message-index
          (select-row object suggested-message-index))
        (when redraw
          (win-clear object)
          (draw object))))))

(defun open-gemini-subscription-window ()
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *gemini-subscription-window*
          (make-instance 'gemini-subscription-window
                         :top-row-padding   0
                         :title             (_ "Subscribed gemlogs")
                         :single-row-height 1
                         :uses-border-p     t
                         :keybindings       keybindings:*gemlog-subscription-keymap*
                         :croatoan-window   low-level-window))
    (refresh-config  *gemini-subscription-window*)
    (resync-rows-db  *gemini-subscription-window* :redraw nil)
    (when (not (line-oriented-window:rows-empty-p *gemini-subscription-window*))
      (select-row  *gemini-subscription-window* 0))
    (draw  *gemini-subscription-window*)
     *gemini-subscription-window*))
