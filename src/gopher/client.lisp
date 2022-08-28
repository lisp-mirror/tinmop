;; tinmop: an humble gemini kami and pleroma client
;; Copyright © 2022  cage

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

(in-package :gopher-client)

(a:define-constant +request-terminal+  (format nil "~a~a" #\Return #\Newline) :test #'string=)

(a:define-constant +response-terminal+ (format nil ".~a~a" #\Return #\Newline) :test #'string=)

(a:define-constant +response-read-buffer-size+ 4096 :test #'=)

(defun make-collect-fn (collected)
  (lambda (buffer)
    (loop for b across buffer do
      (vector-push-extend b collected 1024))))

(defun %request (host &key
                        (port 70)
                        (selector "")
                        (terminate-strategy :response-terminal)
                        (collect-fn (lambda (data) (format t "~a" (to-s data)))))
  (assert (or (null terminate-strategy)
              (eq terminate-strategy :response-terminal)))
  (flet ((open-socket (hostname port)
           (usocket:socket-connect hostname
                                   port
                                   :element-type '(unsigned-byte 8)))
         (end-response-p (read-so-far buffer)
           (if (< read-so-far (length buffer))
               t
               (let ((maybe-terminal-data (subseq buffer
                                                  (- read-so-far
                                                     (length +response-terminal+))
                                                  read-so-far)))
                 (and (eq terminate-strategy :response-terminal)
                      (string= (to-s maybe-terminal-data)
                               +response-terminal+))))))
    (let* ((socket (open-socket host port))
           (stream (usocket:socket-stream socket)))
      (write-sequence (babel:string-to-octets (format nil
                                                      "~a~a"
                                                      selector
                                                      +request-terminal+))
                      stream)
      (finish-output stream)
      (let* ((buffer (misc:make-fresh-array +response-read-buffer-size+
                                            0
                                            '(unsigned-byte 8)
                                            t))
             (first-chunk-size (read-sequence buffer stream)))
        (labels ((read-all (buffer read-so-far)
                   (funcall collect-fn (subseq buffer 0 read-so-far))
                   (when (not (end-response-p read-so-far buffer))
                     (let ((new-chunk-size (read-sequence buffer stream)))
                       (read-all buffer new-chunk-size)))))
          (read-all buffer first-chunk-size))))))

(defmacro gen-request-function (return-types strategies)
  `(defun ,(format-fn-symbol t "request")
       (host response-type
        &key
          (port 70)
          (selector "")
          (collect-fn (lambda (data) (format t "~s" (to-s data)))))
     (cond
       ,@(append
          (loop for return-type in return-types
                for strategy    in strategies
                collect
                `((string= response-type ,return-type)
                  (%request host
                            :port               port
                            :selector           selector
                            :terminate-strategy ,strategy
                            :collect-fn         collect-fn)))
          `(((string= response-type +line-type-uri+)
             (open-message-link-window:open-message-link selector nil)))
          `((t
             (%request host :port               port
                            :selector           selector
                            :terminate-strategy nil
                            :collect-fn         collect-fn)))))))

(gen-request-function (+line-type-file+
                       +line-type-dir+
                       +line-type-error+
                       +line-type-mac-hex-file+
                       +line-type-dos-archive-file+
                       +line-type-uuencoded-file+
                       +line-type-index-search+
                       +line-type-binary-file+
                       +line-type-gif-image-file+
                       +line-type-image-file+
                       +line-type-info+)
                      (:response-terminal
                       :response-terminal
                       :response-terminal
                       nil
                       nil
                       nil
                       :response-terminal
                       nil
                       nil
                       nil
                       :response-terminal))

(defun request-from-iri (iri &optional (collect-function (lambda (data)
                                                           (format t "~a" (to-s data)))))
  (multiple-value-bind (host port type selector)
      (parse-iri iri)
    (request host
             type
             :port     port
             :selector selector
             :collect-fn collect-function)))
