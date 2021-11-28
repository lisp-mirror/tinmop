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
;; tinmop -e get-following.lisp < follows-file
;; the file must be in the format
;; account name   newline
;; account name 2 newline
;; account name 3 newline
;; etc.

(in-package :scripts)

(defun connect ()
  (client:init)
  (client:authorize))

(defun main ()
  (connect)
  (db-utils:with-ready-database (:connect t)
    (loop for line = (read-line *standard-input* nil nil nil)
          while line do
            (let* ((text-utils:*blanks* (append *blanks* '(#\")))
                   (name                (text-utils:trim-blanks line))
                   (id                  (program-events::find-user-id-from-exact-acct name)))
              (if id
                  (progn
                    (format t "following: ~a…" name)
                    (finish-output)
                    (api-client:follow-user id)
                    (format t "Done.~%"))
                  (format *error-output* "User ~s not found~%" name))))))
(main)
