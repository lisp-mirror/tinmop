;; delete old posts from the server (script for for tinmop)
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

;; usage: tinmop -e delete-old-posts.lisp and follow instructions

;; IMPORTANT NOTE: this script  will *permanently delete* the messages
;; (aka, post  or toot) from  your server beyond any  recovering! Use
;; with caution!

;; after configuring tinmop (https://www.autistici.org/interzona/tinmop.html)
;; tinmop -e delete-old-post.lisp

(in-package :scripts)

(defparameter *min-past-date*        nil)

(defparameter *date-delete-treshold* nil)

(defun decode-post-date (post)
  (multiple-value-bind (x y z day month year)
      (decode-universal-time (tooter:created-at post) 0)
    (declare (ignore x y z))
    (format nil
            "~a-~2,'0d-~2,'0d"
            year month day)))

(defun date>= (a b)
  (string>= a b))

(defun date< (a b)
  (string< a b))

(defun list-posts (min-past-date show-progress &optional (max-id nil) (accum ()))
  (let* ((username       (swconf:config-username))
         (user-id        (db:acct->id username))
         (timeline-type  (ui::timeline->kind db:+home-timeline+))
         (posts          (api-client:get-timeline timeline-type
                                                  :local  t
                                                  :max-id max-id
                                                  :limit  10))
         (sorted-posts   (api-client:sort-id< posts))
         (min-post-id    (tooter:id (first sorted-posts)))
         (min-post-date  (decode-post-date (first sorted-posts)))
         (my-posts       (remove-if-not (lambda (a)
                                 (string= (tooter:id (tooter:account a))
                                          user-id))
                                posts)))
    (if (and posts
             (date>= min-post-date min-past-date))
        (progn
          (when show-progress
            (format t "downloaded until ~a~%" min-post-date))
          (list-posts min-past-date t min-post-id (append my-posts accum)))
        accum)))

(defun main ()
  (client:init)
  (client:authorize)
  (format t "This client has been authorized.~%")
  (format t "Please provide the post's maximum creation date.~%")
  (format t "Posts with date older than the provided one will be deleted~%")
  (write-string "Maximum date (format \"YYY-MM-DD\"): ")
  (finish-output)
  (let* ((input-date             (read-line))
         (*date-delete-treshold* (db-utils:encode-datetime-string input-date)))
    (format t "Please provide the post's minimum creation date.~%")
    (format t "Posts with date older than the provided one will *not* be deleted~%")
    (write-string "Minimum date (format \"YYY-MM-DD\"): ")
    (finish-output)
    (let* ((min-input-date  (read-line))
           (*min-past-date* (db-utils:encode-datetime-string min-input-date)))
      (when (yes-or-no-p "Deleting posts older than ~s and newer than ~s. Continue?"
                         input-date
                         min-input-date)
        (cond
          ((null *date-delete-treshold*)
           (format *error-output* "Date ~s is not valid, exiting.~%" input-date))
          ((null *min-past-date*)
           (format *error-output* "Date ~s is not valid, exiting.~%" min-input-date))
          (t
           (let ((posts (list-posts (db-utils:decode-date-string *min-past-date*) t)))
             (format t  "Start deleting...~%")
             (loop for post in posts
                   when (date< (decode-post-date post)
                               (db-utils:decode-date-string *date-delete-treshold*))
                     do
                        (let ((post-contents (with-output-to-string (stream)
                                               (tooter::present post stream))))
                          (format t "deleting~2%~a~%" post-contents))))))))))

;                          (api-client:delete-status (tooter:id post)))))))))))


(main)
