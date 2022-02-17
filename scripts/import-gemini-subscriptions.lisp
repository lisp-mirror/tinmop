;; follow a list of users
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

;; usage:

;; after configuring tinmop (https://www.autistici.org/interzona/tinmop.html)
;; tinmop -e import-gemini-subscriptions.lisp < subscriptions
;; the file must be in the format
;; gemlog-url     newline
;; gemlog-url-2   newline
;; gemlog-url-3   newline
;; etc.

(in-package :scripts)

(defun main ()
  (db-utils:with-ready-database (:connect t)
    (loop for line = (read-line *standard-input* nil nil nil)
          while line do
            (let* ((url (text-utils:trim-blanks line)))
              (format t "Subscribing to: ~a…" url)
              (gemini-subscription:subscribe url)
              (format t "Done~%")))))

(main)
