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
;; along with this program.
;; If not, see [[http://www.gnu.org/licenses/][http://www.gnu.org/licenses/]].

(in-package :x509)

(defun dump-certificate (ssl-stream)
  (let* ((cert               (cl+ssl:ssl-stream-x509-certificate ssl-stream))
         (certificate-length (i2d-x509 cert (cffi:null-pointer))))
    (unwind-protect
         (if (< certificate-length 0)
             (error "i2d-X509 failed")
             (cffi:with-foreign-object (buf* :unsigned-char certificate-length)
               (cffi:with-foreign-object (buf** :pointer)
                 (setf (cffi:mem-ref buf** :pointer) buf*)
                 (i2d-x509 cert buf**)
                 (let* ((data (loop for i from 0 below certificate-length
                                    collect
                                    (cffi:mem-aref buf* :unsigned-char i)))
                        (res  (misc:make-fresh-array certificate-length 0 '(unsigned-byte 8) t)))
                   (misc:copy-list-into-array data res)
                   res))))
      (cl+ssl:x509-free cert))))


(defun pem->der (pem-file)
  (handler-case
      (let* ((raw     (fs:slurp-file pem-file))
             (encoded (cl-ppcre:regex-replace-all "-----(BEGIN|END) CERTIFICATE-----" raw ""))
             (decoded (base64:base64-string-to-usb8-array encoded)))
        (fs:with-anaphoric-temp-file (stream)
          (write-sequence decoded stream)
          filesystem-utils::temp-file))
   (error () pem-file)))

(defgeneric certificate-fingerprint (object &key hash-algorithm))

(defmacro decode-fingerprint (cert hash-algorithm)
  (alexandria:with-gensyms (hash hash-string algo-string)
    `(unwind-protect
          (let* ((,hash        (cl+ssl:certificate-fingerprint ,cert ,hash-algorithm))
                 (,hash-string (format nil "~{~2,'0x~}" (map 'list #'identity ,hash)))
                 (,algo-string (format nil "~:@(~a~)" ,hash-algorithm)))
            (text-utils:strcat ,algo-string ":" (string-downcase ,hash-string)))
       (cl+ssl:x509-free ,cert))))

(defmethod certificate-fingerprint ((object cl+ssl::ssl-stream) &key (hash-algorithm :sha256))
  (let* ((cert (cl+ssl:ssl-stream-x509-certificate object)))
    (decode-fingerprint cert hash-algorithm)))

(defmethod certificate-fingerprint ((object string) &key (hash-algorithm :sha256))
  (let* ((cert (cl+ssl:decode-certificate-from-file (pem->der object) :format :der)))
    (decode-fingerprint cert hash-algorithm)))
