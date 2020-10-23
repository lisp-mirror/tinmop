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

(in-package :gemini-certificates-window)

(defclass gemini-certificates-window (focus-marked-window
                                      simple-line-navigation-window
                                      title-window
                                      border-window)
  ())

(defmethod refresh-config :after ((object gemini-certificates-window))
  (open-attach-window:refresh-view-links-window-config object
                                                       swconf:+key-gemini-certificates-window+)
  (refresh-config-sizes object swconf:+key-thread-window+)
  (win-move object
            (- (win-width *main-window*)
               (win-width object))
            0)
  (win-move object
            (- (win-width *main-window*)
               (win-width object))
            0)
  object)

(defun cache->list-item (cache-db-row &optional (attributes (tui:attribute-bold)))
  (multiple-value-bind (link-fg creation-fg  access-fg)
      (swconf:gemini-certificates-window-colors)
    (let ((creation-date (db-utils:encode-datetime-string (db:row-cache-created-at  cache-db-row)))
          (access-date   (db-utils:encode-datetime-string (db:row-cache-accessed-at cache-db-row)))
          (link          (db:row-cache-key cache-db-row)))
      (reduce (lambda (a b) (cat-tui-string a b :color-attributes-contagion nil))
              (list (_ "address: ")
                    (make-tui-string link :fgcolor link-fg :attributes attributes)
                    (_ " creation date: ")
                    (make-tui-string (db-utils:decode-date-string creation-date)
                                     :fgcolor creation-fg :attributes attributes)
                    (_ " last access date: ")
                    (make-tui-string (db-utils:decode-date-string access-date)
                                     :fgcolor access-fg :attributes attributes))))))

(defun cache->unselected-list-item (cache-db-row)
  (cache->list-item cache-db-row (tui:combine-attributes (tui:attribute-bold))))

(defun cache->selected-list-item (cache-db-row)
  (tui:tui-string->chars-string (cache->list-item cache-db-row)))

(defmethod resync-rows-db ((object gemini-certificates-window)
                           &key
                             (redraw t)
                             (suggested-message-index nil))
  (with-accessors ((rows             rows)
                   (selected-line-bg selected-line-bg)
                   (selected-line-fg selected-line-fg)) object
    (flet ((make-rows (cache-rows bg fg)
             (mapcar (lambda (cache-row)
                       (make-instance 'line
                                      :normal-text   (cache->unselected-list-item cache-row)
                                      :selected-text (cache->selected-list-item   cache-row)
                                      :fields        cache-row
                                      :normal-bg     fg
                                      :normal-fg     bg
                                      :selected-bg   bg
                                      :selected-fg   fg))
                     cache-rows)))
      (with-croatoan-window (croatoan-window object)
        (setf rows (make-rows (db:find-tls-certificates-rows)
                              selected-line-bg
                              selected-line-fg))
        (when suggested-message-index
          (handler-bind ((conditions:out-of-bounds
                          (lambda (e)
                            (invoke-restart 'ignore-selecting-action e))))
            (select-row object suggested-message-index)))
        (when redraw
          (win-clear object)
          (draw object))))))

(defun open-gemini-certificates-window ()
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *gemini-certificates-window*
          (make-instance 'gemini-certificates-window
                         :top-row-padding   0
                         :title             (_ "Generated certificates")
                         :single-row-height 1
                         :uses-border-p     t
                         :keybindings       keybindings:*gemini-certificates-keymap*
                         :croatoan-window   low-level-window))
    (refresh-config  *gemini-certificates-window*)
    (resync-rows-db  *gemini-certificates-window* :redraw nil)
    (when (rows  *gemini-certificates-window*)
      (select-row  *gemini-certificates-window* 0))
    (draw  *gemini-certificates-window*)
    *gemini-certificates-window*))
