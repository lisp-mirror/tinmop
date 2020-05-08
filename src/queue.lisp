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

(in-package :queue)

(defparameter *queue* (misc:make-array-frame 0))

(defparameter *equal-function* #'equalp)

(defparameter *key-function* #'identity)

(defun push (val)
  (vector-push-extend val *queue*))

(defun pop ()
  (if (not (emptyp))
      (prog1
          (alexandria:first-elt *queue*)
        (setf *queue* (misc:safe-delete@ *queue* 0)))
      nil))

(defun find (element)
  (cl:find element *queue* :key *key-function* :test *equal-function*))

(defun emptyp ()
  (not (> (length *queue*) 0)))

(defmacro with-queue ((equal key) &body body)
  `(let ((*queue* (misc:make-array-frame 0))
         (*equal-function* ,equal)
         (*key-function* ,key))
     ,@body))

(defclass simple-queue ()
  ((container
    :initform (misc:make-array-frame 0)
    :accessor container)))

(defgeneric q-pop (object))

(defgeneric q-peek (object))

(defgeneric q-push (object value))

(defgeneric q-empty-p (object))

(defgeneric q-size (object))

(defgeneric q-sort (object predicate))

(defgeneric q-dbg-print (object))

(defmethod q-pop ((object simple-queue))
  (with-accessors ((container container)) object
    (let ((peek (q-peek object)))
      (if peek
          (progn
            (setf container (misc:safe-delete@ container 0))
            peek)
          nil))))

(defmethod q-push ((object simple-queue) value)
  (with-accessors ((container container)) object
    (vector-push-extend value container)))

(defmethod q-empty-p ((object simple-queue))
  (with-accessors ((container container)) object
    (misc:vector-empty-p container)))

(defmethod q-peek ((object simple-queue))
  (with-accessors ((container container)) object
    (if (not (q-empty-p object))
        (elt container 0)
        nil)))

(defmethod q-size ((object simple-queue))
  (length (container object)))

(defmethod q-sort ((object simple-queue) predicate)
  (with-accessors ((container container)) object
    (setf container (stable-sort container predicate))))

(defmethod q-dbg-print ((object simple-queue))
  (misc:dbg "--queue--")
  (loop
     for i from 0
     for a across (container object) do
       (misc:dbg "~a ~a" i a))
  (misc:dbg "----"))
