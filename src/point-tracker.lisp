;; tinmop: an humble gemini and pleroma client
;; Copyright (C) 2018  cage

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

(in-package :point-tracker)

(defclass point-tracker ()
  ((point-position
    :initform 0
    :initarg  :point-position
    :accessor point-position)
   (point-fg
    :initform :white
    :initarg  :point-fg
    :accessor point-fg)
   (point-bg
    :initform :white
    :initarg  :point-bg
    :accessor point-bg)
   (prompt
    :initform "> "
    :initarg  :prompt
    :accessor prompt)))

(defmethod initialize-instance :after ((object point-tracker) &key &allow-other-keys)
  (with-accessors ((point-position point-position)
                   (prompt         prompt)) object
    (setf point-position (length prompt))))

(defgeneric no-prompt-point-pos (object))

(defgeneric move-point-left (object &key offset))

(defgeneric move-point-right (object max &key offset))

(defgeneric move-point (object to max))

(defgeneric move-point-to-end (object text))

(defgeneric move-point-to-start (object))

(defgeneric insert-at-point (object thing text))

(defgeneric delete-at-point (object text &key direction))

(defmethod no-prompt-point-pos ((object point-tracker))
  (- (point-position object)
     (length (prompt object))))

(defmethod move-point-left ((object point-tracker) &key (offset 1))
  (with-accessors ((point-position point-position)
                   (prompt         prompt)) object
    (setf point-position
          (max (length prompt)
               (- point-position offset)))
    point-position))

(defmethod move-point-right ((object point-tracker) max &key (offset 1))
  (with-accessors ((point-position point-position)
                   (prompt         prompt)) object
    (let ((prompt-length (length prompt)))
      (setf point-position
            (min (+ max prompt-length)
                 (+ offset point-position))))
    point-position))

(defmethod move-point ((object point-tracker) to max)
  (with-accessors ((point-position point-position)
                   (prompt         prompt)) object
    (let ((prompt-length (length prompt)))
    (setf point-position
          (clamp (+ to prompt-length)
                 prompt-length
                 (+ max prompt-length)))
    point-position)))

(defmethod move-point-to-end ((object point-tracker) text)
  (let ((length (length text)))
    (move-point object length length)))

(defmethod move-point-to-start ((object point-tracker))
  (move-point object 0 1))

(defmethod insert-at-point ((object point-tracker) thing text)
  (with-accessors ((point-position point-position)) object
    (let* ((actual-point-pos (no-prompt-point-pos object))
           (res (strcat (subseq text 0 actual-point-pos)
                        (to-s thing)
                        (subseq text actual-point-pos))))
      (move-point-right object (length res))
      res)))

(defmethod insert-at-point ((object point-tracker) thing (text null))
  (insert-at-point object nil (to-s thing)))

(defmethod insert-at-point ((object point-tracker) (thing null) text)
  (prog1
      (to-s text)
    (move-point-right object (length text))))

(defmethod delete-at-point ((object point-tracker) text &key (direction :right))
  (with-accessors ((point-position point-position)) object
    (handler-bind ((conditions:out-of-bounds
                    (lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'misc:return-whole))))
      (if (eq direction
              :left)
          (if (> (no-prompt-point-pos object)
                 0)
              (progn
                (move-point-left object)
                (to-s (safe-delete@ text (no-prompt-point-pos object))))
              (to-s text))
          (to-s (safe-delete@ text (no-prompt-point-pos object)))))))
