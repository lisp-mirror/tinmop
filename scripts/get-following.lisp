;; geat a list of people you are following
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
;; tinmop -e get-following.lisp > follows.csv

(in-package :scripts)

(defun connect ()
  (client:init)
  (client:authorize))

(defun main ()
  (connect)
  (db-utils:with-ready-database (:connect t)
    (let* ((username          (db:acct->id (swconf:config-username)))
           (local-server-name (swconf:config-server-name))
           (follows-accounts  (api-pleroma:get-following username)))
      (loop for account in follows-accounts do
        (let ((acct (tooter:account-name account)))
          (if (scan "@" acct)
              (format t "~s~%" acct)
              (format t "\"~a@~a\"~%" acct local-server-name)))))))

(main)
