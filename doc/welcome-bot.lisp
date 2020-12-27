;; a simple welcomebot for tinmop
;; Copyright © 2020 cage

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
;; tinmop -e welcome.lisp

(in-package :scripts)

(define-constant +table-id+ "welcome-minimum-id" :test #'string=)

(defun make-table-last-id ()
  (db-utils:query-low-level (strcat (db::prepare-table +table-id+
                                                       :autogenerated-id-p nil)
                                    " id TEXT"
                                    db::+make-close+)))

(defun dump-minimum-id (minimum-id)
  (db-utils:query (db-utils:make-insert :welcome-minimum-id (:id) (minimum-id))))

(defun minimum-id ()
  (let ((res (db-utils:get-max-id :welcome-minimum-id)))
    (when (not (numberp res)) ; workaround: db-utils:get-max-id return
                              ; 0 instead of nil when table is empty
      res)))

(defun connect ()
  (client:init)
  (client:authorize))

(defun create-table ()
  (ignore-errors (make-table-last-id)))

(defun main ()
  (connect)
  (db-utils:with-ready-database (:connect t)
    (create-table)
    (let ((minimum-id (minimum-id)))
      (assert (or (stringp minimum-id)
                  (null    minimum-id)))
      (let* ((toots      (api-client::sort-id< (api-client:get-timeline :public
                                                                        :local  t
                                                                        :min-id minimum-id)))
             (last-toot  (lastcar toots)))
        (when last-toot
          (dump-minimum-id (tooter:id last-toot))
          (loop for toot in toots do
            (let* ((account          (tooter:account toot))
                   (welcome-message  (format nil
                                             "Hi ~s!~%Enjoy the fediverse!"
                                             (tooter:display-name account)))
                   (reply-id         (tooter:id toot)))
              (when (and (not (tooter:bot            account))
                         (<=  (tooter:statuses-count account)
                              1))
                (format t
                        "[INFO] sending welcome message to ~a (~a)~%"
                        account reply-id)
                (send-status welcome-message reply-id nil "Welcome!" :public)))))))))

(main)