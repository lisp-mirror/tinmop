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

(in-package :open-attach-window)

(defclass open-attach-window (focus-marked-window
                              simple-line-navigation-window
                              title-window
                              border-window)
  ((status-id
    :initform nil
    :initarg  :status-id
    :accessor status-id)))

(defmethod refresh-config :after ((object open-attach-window))
  (with-accessors ((croatoan-window croatoan-window)
                   (selected-line-bg  selected-line-bg)
                   (selected-line-fg  selected-line-fg)) object
    (let* ((theme-style (swconf:form-style swconf:+key-open-attach-window+))
           (fg          (swconf:foreground theme-style))
           (bg          (swconf:background theme-style))
           (selected-fg (swconf:selected-foreground theme-style))
           (selected-bg (swconf:selected-background theme-style))
           (win-w       (truncate (/ (win-width  specials:*main-window*) 2)))
           (win-h       (truncate (/ (win-height specials:*main-window*) 2)))
           (x           (truncate (- (/ (win-width specials:*main-window*) 2)
                                     (/ win-w 2))))
           (y           (truncate (- (/ (win-height specials:*main-window*) 2)
                                     (/ win-h 2)))))
      (setf (background croatoan-window)
            (tui:make-background bg))
      (setf (bgcolor croatoan-window) bg)
      (setf (fgcolor croatoan-window) fg)
      (setf selected-line-fg selected-fg)
      (setf selected-line-bg selected-bg)
      (win-resize object win-w win-h)
      (win-move object x y)
      object)))

(defmethod resync-rows-db ((object open-attach-window) &key
                                                         (redraw t)
                                                         (suggested-message-index nil))
  (with-accessors ((rows             rows)
                   (status-id        status-id)
                   (selected-line-bg selected-line-bg)
                   (selected-line-fg selected-line-fg)) object
    (flet ((make-rows (attach-names bg fg)
             (mapcar (lambda (name)
                         (make-instance 'line
                                        :normal-text   name
                                        :selected-text name
                                        :normal-bg     bg
                                        :normal-fg     fg
                                        :selected-bg   fg
                                        :selected-fg   bg))
                     attach-names)))
      (let ((attach-names (db:all-attachments-urls-to-status status-id)))
        (with-croatoan-window (croatoan-window object)
          (setf rows (make-rows attach-names
                                selected-line-bg
                                selected-line-fg))
          (when suggested-message-index
            (select-row object suggested-message-index))
          (when redraw
            (draw object)))))))

(defun init (status-id)
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *open-attach-window*
          (make-instance 'open-attach-window
                         :title             (_ "Attachments")
                         :status-id         status-id
                         :single-row-height 1
                         :uses-border-p     t
                         :keybindings       keybindings:*open-attach-keymap*
                         :croatoan-window   low-level-window))
    (refresh-config *open-attach-window*)
    (resync-rows-db *open-attach-window* :redraw nil)
    (when (rows *open-attach-window*)
      (select-row *open-attach-window* 0))
    (draw *open-attach-window*)
    *open-attach-window*))

(defun get-extension (file)
  (multiple-value-bind (matchedp res)
      (cl-ppcre:scan-to-strings "(?i)[a-z0-9]\(\\.[^.]+)(\\?.+)$" file)
    (when matchedp
      (first-elt res))))

(defun open-attachment (url)
  (flet ((add-extension (cached-value)
           (strcat (to-s cached-value) (get-extension url))))
    (let ((cached (db:cache-get-value url)))
      (if (not cached)
          (let* ((cached-file-name (add-extension (db:cache-put url)))
                 (cached-output-file (os-utils:cached-file-path cached-file-name))
                 (stream (get-url-content url)))
            (fs:create-file cached-output-file :skip-if-exists t)
            (with-open-file (out-stream
                             cached-output-file
                             :element-type      '(unsigned-byte 8)
                             :if-does-not-exist :error
                             :if-exists         :supersede
                             :direction         :output)
              (loop for byte = (read-byte stream nil nil) while byte do
                   (write-byte byte out-stream)))
            (open-attachment url))
          (let ((cached-file (os-utils:cached-file-path (add-extension cached))))
            (if (or (not (fs:file-exists-p cached-file))
                    (<= (fs:file-size cached-file) 0))
                (progn
                  (db:cache-invalidate url)
                  (open-attachment url))
                (os-utils:xdg-open cached-file)))))))
