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

(in-package :box)

(defclass box ()
  ((contents
   :initform nil
   :initarg  :contents
   :accessor contents
   :documentation "the thing inside the box"))
  (:documentation "A generic object that contains a value"))

(defmethod print-object ((object box) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (contents object))))

(defun boxp (thing)
  (typep thing 'box))

(defun box (thing)
  "Put `thing' in a box, if thing is aalready a box return `thing'."
  (if (boxp thing)
      thing
      (make-instance 'box :contents thing)))

(defun unbox (thing)
  "Unbox `thing' and returns the value  contained, if `thing' is not a
box return `thing'."
  (if (boxp thing)
      (contents thing)
      thing))

(defsetf unbox (object) (val)
  `(setf (contents ,object) ,val))

(defun dboxp (thing)
  "Return non nil id `thing' is a box that contains a box (double box)."
  (and (boxp thing)
       (boxp (contents thing))))

(defun dbox (thing)
  "Box `thing' in a double box."
  (if (dboxp thing)
      thing
      (make-instance 'box :contents (box thing))))

(defun dunbox (object)
  "Unbox a double box"
  (contents (contents object)))

(defsetf dunbox (object) (val)
  `(setf (contents (contents ,object)) ,val))
