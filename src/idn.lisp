(in-package :idn)

(cffi:define-foreign-library libidn2
  (:unix (:or "libidn2.so.0" "libidn2.so"))
  (t (:default "libmagic")))

(cffi:use-foreign-library libidn2)

;; int idn2_to_ascii_8z (const char * input, char ** output, int flags)

(cffi:defcfun (idn2-to-ascii-8z "idn2_to_ascii_8z")
    :int
  (input  :pointer)
  (output :pointer)
  (flags  :int))

;; int idn2_to_unicode_8z8z (const char *input, char **output, int flags)

(cffi:defcfun (idn2-to-unicode-8z8z "idn2_to_unicode_8z8z")
    :int
  (input  :pointer)
  (output :pointer)
  (flags  :int))  ; unused

(define-condition punycode-conversion-error (error)
  ((host
    :initarg :host
    :reader host)
   (error-code
    :initarg :error-code
    :reader error-code))
  (:report (lambda (condition stream)
             (format stream
                     "error converting ~a to ASCII (code: ~a)"
                     (host condition)
                     (error-code condition))))
  (:documentation "Error conversion unicode -> ASCII"))

(defun ->ascii-default-flags ()
  (logior (cffi:foreign-enum-value 'flags :nontransitional)
          (cffi:foreign-enum-value 'flags :nfc-input)))

(defun unicode->ascii (host &optional (flags (->ascii-default-flags)))
  (labels ((deref (ptr** index)
             (cffi:mem-aref (cffi:mem-aref ptr** :pointer)
                            :char index)))
    (cffi:with-foreign-string (input host)
      (cffi:with-foreign-object (buf* :unsigned-char)
        (cffi:with-foreign-object (buf** :pointer)
          (setf (cffi:mem-ref buf** :pointer) buf*)
          (let ((results (idn2-to-ascii-8z input buf** flags)))
            (unwind-protect
                 (if (= (cffi:foreign-enum-value 'idn2-rc :ok)
                        results)
                     (with-output-to-string (punycode)
                       (loop for i from 0 while (/= (deref buf** i)
                                                    0)
                             do
                                (let ((octect (deref buf** i)))
                                  (write-char (code-char octect) punycode))))
                     (error 'punycode-conversion-error
                            :host       host
                            :error-code (cffi:foreign-enum-keyword 'idn2-rc results)))
              (cffi:foreign-free (cffi:mem-aref buf** :pointer)))))))))


(defun ascii->unicode (host)
  (labels ((deref (ptr** index)
             (cffi:mem-aref (cffi:mem-aref ptr** :pointer)
                            :char index)))
    (cffi:with-foreign-string (input host)
      (cffi:with-foreign-object (buf* :unsigned-char)
        (cffi:with-foreign-object (buf** :pointer)
          (setf (cffi:mem-ref buf** :pointer) buf*)
          (let ((results (idn2-to-unicode-8z8z input buf** 0)))
            (unwind-protect
                 (if (= (cffi:foreign-enum-value 'idn2-rc :ok)
                        results)
                     (let ((octects (misc:make-array-frame 0 0 '(unsigned-byte 8))))
                       (loop for i from 0 while (/= (deref buf** i)
                                                    0)
                             do
                                (vector-push-extend (logand (deref buf** i) 255)
                                                    octects))
                       (babel:octets-to-string octects))
                     (error 'punycode-conversion-error
                            :host       host
                            :error-code (cffi:foreign-enum-keyword 'idn2-rc results)))
              (cffi:foreign-free (cffi:mem-aref buf** :pointer)))))))))
