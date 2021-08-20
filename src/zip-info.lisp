;; tinmop: an humble gemini and pleroma client
;; Copyright (C) 2021  cage

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

(in-package :zip-info)

(alexandria:define-constant +byte-type+                '(unsigned-byte 8) :test #'equalp)

(alexandria:define-constant +eocd-signature+                   #x06054b50 :test #'=)

(alexandria:define-constant +eocd-signature-size+                       4 :test #'=)

(alexandria:define-constant +eocd-number-of-disk+                       2 :test #'=)

(alexandria:define-constant +eocd-number-of-disk-w/start-eocd+          2 :test #'=)

(alexandria:define-constant +eocd-tot-no-entry-cd-this-disk+            2 :test #'=)

(alexandria:define-constant +eocd-tot-no-entry-cd+                      2 :test #'=)

(alexandria:define-constant +eocd-cd-size+                              4 :test #'=)

(alexandria:define-constant +eocd-cd-offset+                            4 :test #'=)

(alexandria:define-constant +eocd-zip-file-comment-length+              2 :test #'=)

(alexandria:define-constant +eocd-fixed-size+   (+ +eocd-signature-size+
                                                   +eocd-number-of-disk+
                                                   +eocd-number-of-disk-w/start-eocd+
                                                   +eocd-tot-no-entry-cd-this-disk+
                                                   +eocd-tot-no-entry-cd+
                                                   +eocd-cd-size+
                                                   +eocd-cd-offset+
                                                   +eocd-zip-file-comment-length+)
  :test #'=)

(alexandria:define-constant +eocd-zip-file-comment-offset+ (- +eocd-fixed-size+
                                                              +eocd-zip-file-comment-length+)
  :test #'=)

(defun open-file (path)
  (open path :element-type +byte-type+ :direction :input :if-does-not-exist :error))

(defun close-file (stream)
  (close stream))

(defmacro with-open-zip-file ((stream path) &body body)
  `(let ((,stream (open-file ,path)))
     (unwind-protect
        (progn ,@body)
       (close-file ,stream))))

(defun read-bytes->int (stream size)
  (misc:byte->int (loop repeat size collect (read-byte stream))))

(defun zip-file-p (path)
  (let ((file-size  (file-size path))
        (eocd-start nil))
    (when (>= file-size +eocd-fixed-size+)
      (with-open-zip-file (stream path)
        (loop named signature-finder for position
              from (- file-size +eocd-signature-size+)
              downto 0 do
          (file-position stream position)
          (let ((maybe-signature (read-bytes->int stream +eocd-signature-size+)))
            (when (= maybe-signature +eocd-signature+)
              (setf eocd-start position)
              (return-from signature-finder t))))
        (when eocd-start
          (let* ((eocd-fixed-part-offset         (+ eocd-start +eocd-fixed-size+))
                 (eocd-offset-minus-zip-comment  (- eocd-fixed-part-offset
                                                    +eocd-zip-file-comment-length+)))
            (file-position stream eocd-offset-minus-zip-comment)
            (let ((comment-size (read-bytes->int stream +eocd-zip-file-comment-length+)))
              (= (+ eocd-fixed-part-offset comment-size)
                 file-size))))))))
