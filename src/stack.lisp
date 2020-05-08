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

(in-package :stack)

(defparameter *stack* (misc:make-array-frame 0))

(defparameter *equal-function* #'equalp)

(defparameter *key-function* #'identity)

(defun push (val)
  (vector-push-extend val *stack*))

(defun pop ()
  (if (not (emptyp))
      (prog1
          (alexandria:last-elt *stack*)
        (setf *stack* (misc:safe-delete@ *stack* (1- (length *stack*)))))
      nil))

(defun find (element)
  (cl:find element *stack* :key *key-function* :test *equal-function*))

(defun emptyp ()
  (not (> (length *stack*) 0)))

(defmacro with-stack ((equal key) &body body)
  `(let ((*stack* (misc:make-array-frame 0))
         (*equal-function* ,equal)
         (*key-function* ,key))
     ,@body))
