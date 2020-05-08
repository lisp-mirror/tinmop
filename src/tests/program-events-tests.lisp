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
;; along with this program.
;; If not, see [[http://www.gnu.org/licenses/][http://www.gnu.org/licenses/]].

(in-package :program-events-tests)

(defsuite program-events-suite (all-suite))

(defclass dummy-ask-user-string-events (program-events::ask-user-input-string-event) ())

(defmethod process-event ((object dummy-ask-user-string-events))
  (simulate-user-input object))

(defun simulate-user-input (ask-event)
  (let ((input-done-event (make-instance 'user-input-string-event
                                         :payload            (payload            ask-event)
                                         :lock
                                         (program-events::lock               ask-event)
                                         :condition-variable
                                         (program-events::condition-variable ask-event))))
    (setf (dunbox (payload input-done-event)) "foo")
    (push-event input-done-event)))

(defun dummy-ask-string-input (payload)
  (let ((event (make-instance 'dummy-ask-user-string-events
                              :payload payload)))
    (with-accessors ((lock               program-events::lock)
                     (condition-variable program-events::condition-variable)) event
      (push-event event)
      (with-lock (lock)
        (format t "wait!~%")
        (bt:condition-wait condition-variable lock)
        (format t "input was ~a~%" (dunbox (payload event)))))))

(defun main-thread ()
  (loop repeat 8 do
       (sleep 1)
       (if (event-available-p)
           (progn
             (format t "heap ~a~%" (pq::heap program-events::*events-queue*))
             (process-event (pop-event)))
           (format t "no event~%"))))

(defun simulated-string-input ()
  (let ((payload                        (dbox "bar"))
        (program-events::*events-queue* (make-instance 'events-queue))
        (main-thread                    (bt:make-thread #'main-thread)))
    (sleep 3)
    (bt:make-thread (lambda ()
                      (dummy-ask-string-input payload)))
    (bt:join-thread main-thread)
    payload))

(deftest test-ask-input (program-events-suite)
  (assert-true
      (string= "foo" (dunbox (simulated-string-input)))))
