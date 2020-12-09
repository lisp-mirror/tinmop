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

(in-package :x509-tests)

(defsuite x509-suite (all-suite))

(defparameter *host* nil)

(defparameter *port* 1965)

(defun test-x509-dump (host port)
  (assert host)
  (assert port)
  (let ((ctx (cl+ssl:make-context :verify-mode cl+ssl:+ssl-verify-none+)))
    (cl+ssl:with-global-context (ctx :auto-free-p t)
      (let ((socket (usocket:socket-connect host
                                            port
                                            :element-type '(unsigned-byte 8))))
        (when socket
          (alexandria:when-let* ((stream     (usocket:socket-stream socket))
                                 (ssl-stream (cl+ssl:make-ssl-client-stream stream
                                                                            :external-format nil
                                                                            :unwrap-stream-p t
                                                                            :verify          nil
                                                                            :hostname        host))
                                 (cert (x509:dump-certificate ssl-stream)))
            cert))))))

(deftest test-x509-dump (x509-suite)
  (assert-true (test-x509-dump *host* *port*)))
