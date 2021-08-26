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

(alexandria:define-constant +eocd-signature-value+             #x06054b50 :test #'=)

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

(alexandria:define-constant +cd-signature-value+                 #x02014b50 :test #'=)

(alexandria:define-constant +cd-central-file-header-signature+            4 :test #'=)

(alexandria:define-constant +cd-version-made-by+                          2 :test #'=)

(alexandria:define-constant +cd-version-needed-to-extract+                2 :test #'=)

(alexandria:define-constant +cd-general-purpose-bit-flag+                 2 :test #'=)

(alexandria:define-constant +cd-compression-method+                       2 :test #'=)

(alexandria:define-constant +cd-last-mod-file-time+                       2 :test #'=)

(alexandria:define-constant +cd-last-mod-file-date+                       2 :test #'=)

(alexandria:define-constant +cd-crc-32+                                   4 :test #'=)

(alexandria:define-constant +cd-compressed-size+                          4 :test #'=)

(alexandria:define-constant +cd-uncompressed-size+                        4 :test #'=)

(alexandria:define-constant +cd-file-name-length+                         2 :test #'=)

(alexandria:define-constant +cd-extra-field-length+                       2 :test #'=)

(alexandria:define-constant +cd-file-comment-length+                      2 :test #'=)

(alexandria:define-constant +cd-disk-number-start+                        2 :test #'=)

(alexandria:define-constant +cd-internal-file-attributes+                 2 :test #'=)

(alexandria:define-constant +cd-external-file-attributes+                 4 :test #'=)

(alexandria:define-constant +cd-relative-offset-of-local-header+          4 :test #'=)

(alexandria:define-constant +cd-fixed-size+ (+ +cd-central-file-header-signature+
                                               +cd-version-made-by+
                                               +cd-version-needed-to-extract+
                                               +cd-general-purpose-bit-flag+
                                               +cd-compression-method+
                                               +cd-last-mod-file-time+
                                               +cd-last-mod-file-date+
                                               +cd-crc-32+
                                               +cd-compressed-size+
                                               +cd-uncompressed-size+
                                               +cd-file-name-length+
                                               +cd-extra-field-length+
                                               +cd-file-comment-length+
                                               +cd-disk-number-start+
                                               +cd-internal-file-attributes+
                                               +cd-external-file-attributes+
                                               +cd-relative-offset-of-local-header+)
  :test #'=)


(define-condition zip-error (conditions:text-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~a" (conditions:text condition))))
  (:documentation "Error for zip files procedures"))

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

(defun make-zip-error (reason)
  (error 'zip-error :text reason))

(alexandria:define-constant +max-eocd-total-size+  65536 :test #'=)

(defun zip-file-p (path)
  (let ((file-size  (file-size path))
        (eocd-start nil))
    (if (>= file-size +eocd-fixed-size+)
        (with-open-zip-file (stream path)
          (let ((buffer (make-array +max-eocd-total-size+ :element-type +byte-type+)))
            (file-position stream (- file-size
                                     (min +max-eocd-total-size+
                                          file-size)))
            (read-sequence buffer stream)
            (loop named signature-finder
                  for eocd-position
                  from (- file-size +eocd-signature-size+) downto 0
                  for position
                  from (- +max-eocd-total-size+ +eocd-signature-size+) downto 0
                  do
                  (let* ((maybe-signature (misc:byte->int (subseq buffer
                                                                  position
                                                                  (+ position
                                                                     +eocd-signature-size+)))))
                    (when (= maybe-signature +eocd-signature-value+)
                      (setf eocd-start eocd-position)
                      (return-from signature-finder t)))))
          (if eocd-start
              (let* ((eocd-fixed-part-offset         (+ eocd-start +eocd-fixed-size+))
                     (eocd-offset-minus-zip-comment  (- eocd-fixed-part-offset
                                                        +eocd-zip-file-comment-length+)))
                (file-position stream eocd-offset-minus-zip-comment)
                (let ((comment-size (read-bytes->int stream +eocd-zip-file-comment-length+)))
                  (values (= (+ eocd-fixed-part-offset comment-size)
                             file-size)
                          eocd-start)))
              (make-zip-error (format nil "File ~s contains no zip signature" path))))
        (make-zip-error (format nil "File ~s is too short to be a zip file" path)))))

(defun start-of-central-directory (path)
  (multiple-value-bind (zipp eocd-start)
      (zip-file-p path)
    (when zipp
      (with-open-zip-file (stream path)
        (file-position stream (- (+ eocd-start +eocd-fixed-size+)
                                 +eocd-zip-file-comment-length+
                                 +eocd-cd-offset+))
        (read-bytes->int stream +eocd-cd-offset+)))))

(defun cd-variable-data-lengths (stream start-of-central-directory)
  (file-position stream (+ start-of-central-directory
                           +cd-central-file-header-signature+
                           +cd-version-made-by+
                           +cd-version-needed-to-extract+
                           +cd-general-purpose-bit-flag+
                           +cd-compression-method+
                           +cd-last-mod-file-time+
                           +cd-last-mod-file-date+
                           +cd-crc-32+
                           +cd-compressed-size+
                           +cd-uncompressed-size+))
  (let ((file-name-length    (read-bytes->int stream +cd-file-name-length+))
        (extra-field-length  (read-bytes->int stream +cd-extra-field-length+))
        (file-comment-length (read-bytes->int stream +cd-file-comment-length+)))
    (values file-name-length extra-field-length file-comment-length)))

(defun cd-total-length (stream start-of-central-directory)
  (multiple-value-bind (file-name-length extra-field-length file-comment-length)
      (cd-variable-data-lengths stream start-of-central-directory)
      (+ +cd-fixed-size+
         file-name-length
         extra-field-length
         file-comment-length)))

(defun list-file-from-cd (stream start-of-central-directory)
  (let ((file-name-length (cd-variable-data-lengths stream start-of-central-directory)))
    (file-position stream (+ +cd-fixed-size+ start-of-central-directory))
    (let ((res (make-array file-name-length :element-type +byte-type+)))
      (read-sequence res stream)
      (babel:octets-to-string res))))

(defun list-entries (path)
  (let ((start-of-central-directory (start-of-central-directory path))
        (files                      '()))
    (if start-of-central-directory
        (labels ((read-file-path (stream)
                   (ignore-errors
                    (file-position stream start-of-central-directory)
                    (let ((signature (read-bytes->int stream
                                                      +cd-central-file-header-signature+)))
                      (when (= signature +cd-signature-value+)
                        (push (list-file-from-cd stream start-of-central-directory)
                              files)
                        (setf start-of-central-directory
                              (+ start-of-central-directory
                                 (cd-total-length stream
                                                  start-of-central-directory)))
                        (read-file-path stream))))))
          (with-open-zip-file (stream path)
            (file-position stream start-of-central-directory)
            (let ((signature (read-bytes->int stream +cd-central-file-header-signature+)))
              (if (= signature +cd-signature-value+)
                  (progn
                    (read-file-path stream)
                    files)))))
        (make-zip-error (format nil
                                "File ~s does not contains directory signature"
                                path)))))
