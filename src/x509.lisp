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

(in-package :x509)

(defun dump-certificate (ssl-stream)
  (let ((cert (cl+ssl:ssl-stream-x509-certificate ssl-stream)))
    (cffi:with-foreign-object (buf* :pointer)
      (cffi:with-foreign-object (buf** :pointer)
        (setf (cffi:mem-ref buf** :pointer) buf*)
        (let ((len (i2d-x509 cert buf**)))
          (if (< len 0)
              (error "i2d-X509 failed")
              (let* ((data (loop for i from 0 below len collect
                                (cffi:mem-aref buf* :unsigned-char i)))
                     (res  (misc:make-fresh-array len 0 '(unsigned-byte 8) t)))
                (misc:copy-list-into-array data res)
                res)))))))
