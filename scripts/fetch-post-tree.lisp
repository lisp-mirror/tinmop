;; tinmop module for utility move command in thread window
;; Copyright © 2022 cage

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

(defpackage :fetch-post-tree
  (:use
   :cl
   :misc
   :fs
   :text-utils
   :specials
   :program-events
   :ui-goodies)
  (:local-nicknames (:a :alexandria)))

(in-package :fetch-post-tree)

(if (ui:tui-active-p)
    (labels ((on-input-complete (status-id)
               (with-enqueued-process ()
                 (if (string-not-empty-p status-id)
                     (let* ((timeline      (thread-window:timeline-type *thread-window*))
                            (folder        (thread-window:timeline-folder *thread-window*))
                            (expand-event  (make-instance 'expand-thread-event
                                                          :force-saving-of-ignored-status t
                                                          :new-folder   folder
                                                          :new-timeline timeline
                                                          :status-id    status-id))
                            (refresh-event (make-instance 'refresh-thread-windows-event
                                                          :priority +minimum-event-priority+
                                                          :message-status-id status-id)))
                       (notify "Fetching data")
                       (push-event expand-event)
                       (push-event refresh-event)
                       (notify "Data fetched"))
                     (error-message "ID empty")))))
      (ui:ask-string-input #'on-input-complete
                           :prompt "Type the post's ID: "))
    (format *error-output* "This script needs can not be ran from the command line.~%"))
