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

(in-package :gemini-client)

(define-constant +gemini-scheme+       "gemini" :test #'string=)

(define-constant +gemini-default-port+ 1965     :test #'=)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass response-status-code ()
    ((code
      :initform nil
      :initarg  :code
      :accessor code)
     (description
      :initform nil
      :initarg  :description
      :accessor description)))

  (defun response= (a b)
    (= (code a)
       (code b))))

(defmacro gen-status-constant (value description)
  `(define-constant ,(format-fn-symbol t "+~a+" value)
       (make-instance 'response-status-code
                      :code        ,value
                      :description ,description)
     :test #'response=))

(gen-status-constant 10 "Input")

(gen-status-constant 11 "Sensitive input")

(gen-status-constant 20 "success")

(gen-status-constant 30 "redirect - temporary")

(gen-status-constant 31 "redirect - permanent")

(gen-status-constant 40 "temporary failure")

(gen-status-constant 41 "server unavailable")

(gen-status-constant 42 "cgi error")

(gen-status-constant 43 "proxy error")

(gen-status-constant 44 "slow down")

(gen-status-constant 50 "permanent failure")

(gen-status-constant 51 "not found")

(gen-status-constant 52 "gone")

(gen-status-constant 53 "proxy request refused")

(gen-status-constant 59 "bad request")

(gen-status-constant 60 "client certificate required")

(gen-status-constant 61 "certificate not authorised")

(gen-status-constant 62 "certificate not valid")

(defparameter *all-codes* (list +10+ +11+
                                +20+
                                +30+ +31+
                                +40+ +41+ +42+ +43+ +44+
                                +50+ +51+ +52+ +53+ +59+
                                +60+ +61+ +62+))

(defun code= (code code-class)
  (= code
     (code code-class)))

(defun find-code-class (code)
  (find-if (lambda (a) (code= code a)) *all-codes*))

(defun read-all (stream)
  (let ((raw (loop
                for c = (read-byte stream nil nil)
                while c
                collect c)))
    (coerce raw '(vector (unsigned-byte 8)))))

(defun mime-gemini-p (header-meta)
  (string-starts-with-p "text/gemini" header-meta))

(defun header-code= (header code-class)
  (code= (status-code header)
         code-class))

(defun header-input-request-p (header)
  (or (header-code= header +10+)
      (header-code= header +11+)))

(defun header-success-p (header)
  (header-code= header +20+))

(defun header-redirect-p (header)
  (or (header-code= header +30+)
      (header-code= header +31+)))

(defun header-temporary-failure-p (header)
  (or (header-code= header +40+)
      (header-code= header +41+)
      (header-code= header +42+)
      (header-code= header +43+)
      (header-code= header +44+)))

(defun header-permanent-failure-p (header)
  (or (header-code= header +50+)
      (header-code= header +51+)
      (header-code= header +52+)
      (header-code= header +53+)
      (header-code= header +59+)))

(defun header-not-implemented-p (header)
  (or (header-code= header +60+)
      (header-code= header +61+)
      (header-code= header +62+)))

(define-condition gemini-protocol-error (error)
  ((error-code
    :initarg :error-code
    :reader error-code)
   (error-description
    :initarg :error-description
    :reader  error-description))
  (:report (lambda (condition stream)
             (format stream
                     "The server responded with the error ~a: ~a"
                     (error-code condition)
                     (error-description condition))))
  (:documentation "The condition signalled for error codes (i.e. 4x and 5x)"))

(defun parse-response (stream)
  (let* ((header        (read-line stream))
         (parsed-header (parse-gemini-response-header (format nil "~a~a" header #\Newline))))
    (with-accessors ((meta        meta)
                     (status-code status-code)) parsed-header
      (flet ((results (code-class body)
               (values status-code
                       (description code-class)
                       meta
                       body)))
        (cond
          ((header-success-p parsed-header)
           (let ((body (read-all stream)))
             (if (mime-gemini-p meta)
                 (let ((parsed (parse-gemini-file (babel:octets-to-string body))))
                   (values status-code
                           (description +20+)
                           meta
                           parsed
                           (sexp->text  parsed)
                           (sexp->links parsed)))
                 (results +20+ body))))
          ((or (header-input-request-p parsed-header)
               (header-redirect-p parsed-header))
           (results (find-code-class status-code) nil))
          ((or (header-permanent-failure-p parsed-header)
               (header-temporary-failure-p parsed-header))
           (let ((response-code (find-code-class status-code)))
             (error 'gemini-protocol-error
                    :error-code        (code        response-code)
                    :error-description (description response-code))))
          ((header-not-implemented-p parsed-header)
           (error 'conditions:not-implemented-error
                  :text (_ "The server requested a certificate but client validation is not implemented by this program")))
          (t
           parsed-header))))))

(defun request (host path &key
                            (query nil)
                            (port  +gemini-default-port+))
  (let ((uri (strcat +gemini-scheme+ "://"
                     host            ":"
                     (to-s port)     "/"
                     path))
        (ctx (cl+ssl:make-context :verify-mode cl+ssl:+ssl-verify-none+)))
    (when query
      (setf uri (strcat uri "?" query)))
    (cl+ssl:with-global-context (ctx :auto-free-p t)
      (usocket:with-client-socket (socket stream
                                          host
                                          port
                                          :element-type '(unsigned-byte 8))
        (let* ((ssl-stream (cl+ssl:make-ssl-client-stream stream
                                                          :external-format
                                                          '(:utf-8)
                                                          :unwrap-stream-p t
                                                          :verify          nil
                                                          :hostname       host))
               (request    (format nil "~a~a~a" uri #\Return #\Newline)))
          (write-string request ssl-stream)
          (force-output ssl-stream)
          (multiple-value-bind (status description meta body gemini-text gemini-links)
              (parse-response ssl-stream)
            (values status description meta body gemini-text gemini-links)))))))
