;; tinmop module to delete multiple posts matching a regular expression
;; Copyright © 2021 cage

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

(in-package :modules)

(defun mark-message-deleted-by-regex (regex)
  (when-let ((scanner (ignore-errors (create-scanner regex))))
    (with-accessors ((row-selected-index row-selected-index)
                     (rows               rows)
                     (timeline-type      thread-window:timeline-type)
                     (timeline-folder    thread-window:timeline-folder)) *thread-window*
      (let ((event-payload
              (lambda ()
                (line-oriented-window:map-rows specials:*thread-window*
                                               (lambda (a)
                                                 (let* ((fields    (line-oriented-window:fields a))
                                                        (user      (db:row-message-username  fields))
                                                        (subject   (db:row-message-subject   fields))
                                                        (status-id (db:row-message-status-id fields)))
                                                   (when (or (scan scanner user)
                                                             (scan scanner subject))
                                                     (db:mark-status-deleted-p timeline-type
                                                                               timeline-folder
                                                                               status-id)))))
                (line-oriented-window:resync-rows-db *thread-window* :redraw t))))
        (push-event (make-instance 'function-event
                                   :payload event-payload))))))

(defun delete-post-using-regex ()
  "Delete all posts matching (in field username or subject) a regual expression."
  (flet ((on-input-complete (regex)
           (mark-message-deleted-by-regex regex)))
    (ask-string-input #'on-input-complete
                      :prompt "Regex: ")))
