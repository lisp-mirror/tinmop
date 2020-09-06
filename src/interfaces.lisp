;; tinmop: an humble gemini and pleroma client
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

(in-package :interfaces)

(defgeneric clone (object))

(defmethod clone (object))

(defgeneric clone-into (from to))

(defmethod  clone-into (from to))

(defgeneric copy-flat (object))

(defmethod  copy-flat (object))

(defgeneric copy-flat-into (from to))

(defmethod  copy-flat-into (from to))

(defmacro with-simple-clone ((object type))
  (alexandria:with-gensyms (res)
    `(let ((,res (make-instance ,type)))
       (clone-into ,object ,res)
       ,res)))

(defmacro with-simple-copy-flat ((object type))
  (alexandria:with-gensyms (res)
    `(let ((,res (make-instance ,type)))
       (copy-flat-into ,object ,res)
       ,res)))

(defgeneric serialize (object))

(defgeneric serialize-to-stream (object stream))

(defgeneric deserialize (object file))

(defmethod serialize (object)
  (format nil "~s" (marshal:marshal object)))

(defmethod serialize-to-stream (object stream)
  (prin1 (marshal:marshal object) stream))

(defmethod deserialize (object file)
  (declare (ignore object))
  (marshal:unmarshal (read-from-string (filesystem-utils:slurp-file file))))

;; to use with ms:initialize-unmarshalled-instance
(defgeneric post-deserialization-fix (object))

(defmethod post-deserialization-fix (object)
  object)

(defmethod post-deserialization-fix ((object (eql nil)))
  nil)
