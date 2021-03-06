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

(in-package :crypto-utils)

(define-constant +crypto-data-prefix+             "CRYPTO:" :test #'string=)

(define-constant +crypto-message-field-separator+ ":"       :test #'string=)

(define-constant +crypto-data-prefix-re+ (text-utils:strcat "^" +crypto-data-prefix+)
  :test #'string=
  :documentation "The encrypted text must starts with this prefix")

(defun crypto-text-p (data)
  "Non nil if data starts with `+crypto-data-prefix+'"
  (scan +crypto-data-prefix-re+ data))

(defun add-crypto-prefix (data)
  (text-utils:strcat +crypto-data-prefix+ data))

(defun strip-crypto-prefix (data)
  (misc:safe-subseq data (length +crypto-data-prefix+)))

(defun decode-key (key)
  (base64:base64-string-to-usb8-array key))

(defun encode-key (key)
  (base64:usb8-array-to-base64-string key))

(defun decode-iv (iv)
  (base64:base64-string-to-usb8-array iv))

(defun encode-iv (iv)
  (base64:usb8-array-to-base64-string iv))

(defun generate-key (&optional (length 32))
  (with-open-file (stream "/dev/urandom" :element-type '(unsigned-byte 8))
    (let ((data (misc:make-fresh-array length 0 '(unsigned-byte 8) t)))
      (read-sequence data stream)
      (encode-key data))))

(defun encrypt (data key)
  "Encrypt `data' with `key', note that the initialization vector is autogenerated."
  (multiple-value-bind (encrypted-text x y z iv)
      (cryptos:encrypt data
                       (decode-key key)
                       :mode   :cbc
                       :cipher :aes)
    (declare (ignore x y z))
    (values encrypted-text
            (encode-iv iv))))

(defun decrypt (data key iv)
  "Decrypt `data' with `key' and iv (initialization vector)."
  (cryptos:decrypt data
                   (decode-key key)
                   :iv     (decode-iv iv)
                   :mode   :cbc
                   :cipher :aes))

(defun encrypt-message (data key)
  "encrypt a  message and  wrap it in a  valid text to  be sent  by the
program (add prefix, add iv, separates fields etc.)"
  (multiple-value-bind (encrypted-text iv)
      (encrypt data key)
    (text-utils:strcat +crypto-data-prefix+
                       iv
                       +crypto-message-field-separator+
                       encrypted-text)))

(defun decrypt-message (encrypted-message key)
  "Extract  iv and  actual data  from `encrypted-message'  and try  to
decrypt the latter with key."
  (let* ((raw            (strip-crypto-prefix encrypted-message))
         (iv-mesg        (split +crypto-message-field-separator+ raw))
         (iv             (first iv-mesg))
         (encrypted-body (second iv-mesg))
         (decrypted      (decrypt encrypted-body key iv)))
    decrypted))
