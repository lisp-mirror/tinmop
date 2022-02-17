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

;; tinmop -e export-gemini-subscriptions.lisp

(in-package :scripts)

(defun main ()
  (db-utils:with-ready-database (:connect t)
    (let ((all-subscriptions (db:gemini-all-subscriptions)))
      (loop for row in all-subscriptions do
        (format t "~a~%" (db-utils:db-getf row :url))))))

(main)
