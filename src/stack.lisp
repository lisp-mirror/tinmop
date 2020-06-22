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

(defclass stack ()
  ((container
    :initform ()
    :initarg  :container
    :accessor container)
   (test-fn
    :initform #'eq
    :initarg  :test-fn
    :accessor test-fn)
   (key-fn
    :initform #'identity
    :initarg  :key-fn
    :accessor key-fn)))

(defmethod print-object ((object stack) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~s" (container object))))

(defgeneric stack-push (object val))

(defgeneric stack-pop (object))

(defgeneric stack-remove (object val))

(defgeneric stack-find (object val))

(defgeneric stack-position (object val))

(defgeneric stack-raise-to-top (object val))

(defgeneric stack-empty-p (object))

(defmethod stack-push ((object stack) val)
  (with-accessors ((container container)) object
    (setf container (push val container))
    object))

(defmethod stack-pop ((object stack))
  (with-accessors ((container container)) object
    (if (not (stack-empty-p object))
        (prog1
            (alexandria:first-elt container)
          (setf container (misc:safe-delete@ container 0)))
      nil)))

(defmethod stack-find ((object stack) val)
  (with-accessors ((container container)
                   (key-fn    key-fn)
                   (test-fn   test-fn)) object
    (find val container :key key-fn :test test-fn)))

(defmethod stack-position ((object stack) val)
  (with-accessors ((container container)
                   (key-fn    key-fn)
                   (test-fn   test-fn)) object
    (position val container :key key-fn :test test-fn)))

(defmethod stack-empty-p ((object stack))
  (not (> (length (container object))
          0)))

(defmethod stack-raise-to-top ((object stack) val)
  (with-accessors ((container container)) object
    (when-let ((val-position  (stack-position object val))
               (last-position (1- (length container))))
      (misc:swap (elt container val-position)
                 (elt container 0)))
    object))

(defmethod stack-remove ((object stack) val)
  (with-accessors ((container container)) object
    (when-let ((val-position  (stack-position object val)))
      (setf container (misc:safe-delete@ container val-position)))
    object))

(defmacro do-stack-element ((element stack) &body body)
  `(loop for ,element in (reverse (container ,stack)) do
        ,@body))
