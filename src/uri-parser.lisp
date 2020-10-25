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

(in-package :uri-parser)

(defrule alpha (character-ranges (#\a #\z) (#\A #\Z))
  (:text t))

(defrule digit (character-ranges (#\0 #\9))
  (:text t))

(defrule scheme-delim #\:
  (:constant :scheme-delim))

(defrule query-delim #\?
  (:constant :query-delim))

(defrule fragment-delim #\#
  (:constant :fragment-delim))

(defrule port-delim #\:
  (:constant :port-delim))

(defrule credential-delim #\@
  (:constant :credential-delim))

(defrule authority-start "//"
  (:constant :authority-start))

(defrule sub-delims  (or #\!  #\$  #\&  #\'  #\(  #\) #\*  #\+  #\,  #\;  #\=)
  (:text t))

(defrule gen-delims (or ":" "?"  "#"  "["  "]"  "@" "")
  (:text t))

(defrule unreserved-chars (or alpha digit  #\-  #\.  #\_  #\~)
  (:text t))

(defrule reserved-chars (or gen-delims sub-delims)
  (:text t))

(defrule scheme (and alpha (* (or alpha  digit  "+"  "-"  "." )))
  (:text t))

(defrule hier-part  (and authority-start authority)
  (:function second))

(defrule user-credentials (and userinfo credential-delim)
  (:function first))

(defrule port-block (and port-delim port)
  (:function second)
  (:function parse-integer))

(defrule authority (and (? user-credentials)
                        host
                        (? port-block)))

(defrule reg-name (* (or unreserved-chars pct-encoded sub-delims ))
  (:text t))

(defrule host (or ipv4-address ip-literal reg-name)
  (:text t))

(defrule port (+ digit)
  (:text t))

(defrule userinfo (* (or unreserved-chars  pct-encoded  sub-delims  ":" ))
  (:text t))

(defrule pct-encoded (and "%" hexdig hexdig)
  (:text t))

(defrule hexdig (or (character-ranges #\a #\f) digit)
  (:text t))

(defrule ipv4-address (and dec-octet "." dec-octet "." dec-octet "." dec-octet)
  (:text t))

(defrule ip-literal  (and "["
                          (+ (not (or "[" "]")))
                          "]")
  (:text t))

(defrule pchar (or unreserved-chars pct-encoded  sub-delims  ":"  "@")
  (:text t))

(defrule segment (* pchar)
  (:text t))

(defrule segment-non-zero (+ pchar)
  (:text t))

(defrule segment-nz-nc (+ (or unreserved-chars  pct-encoded  sub-delims  "@" ))
  (:text t))

(defrule path-abempty (* (and "/" segment))
  (:text t))

(defrule path  (or path-abempty)
  (:text t))

(defrule path-absolute (and "/" (or segment-nz (* (and "/" segment ))))
  (:text t))

(defrule path-rootless (and segment-non-zero (* (and "/" segment )))
  (:text t))

(defrule path-noscheme (and segment-nz-nc (* (and  "/" segment )))
  (:text t))

(defrule path-empty ""
  (:constant nil))

(defun octect-p (maybe-octect)
  (ignore-errors
   (let ((number (parse-integer (text-utils:strcat* maybe-octect))))
     (when (<= 0 number 255)
       number))))

(defrule dec-octet (octect-p (+ digit))
  (:text t))

(defun extract-fields-from-absolute-uri (parsed)
  (let ((authority (third parsed)))
    (list (first  parsed)    ; scheme
          (first  authority) ; user-credentials
          (second authority) ; host
          (third  authority) ; port
          (fourth parsed)    ; path
          (fifth  parsed)    ; query
          (sixth  parsed)))) ; fragment

(defrule uri (and scheme ":"
                  hier-part
                  (or path-abempty
                      path-absolute
                      path-noscheme
                      path-empty)
                  (? query)
                  (? fragment))
  (:function extract-fields-from-absolute-uri))

(defrule relative-part (and authority-start
                            authority
                            (or path-abempty
                                path-absolute
                                path-noscheme
                                path-empty))
  (:function (lambda (a) (list (second a)
                               (third a)))))

(defun extract-fields-from-relative-uri (parsed)
  (let ((authority (first  (first parsed)))
        (path      (second (first parsed))))
    (list nil                ; scheme
          (first  authority) ; user-credentials
          (second authority) ; host
          (third  authority) ; port
          path
          (second  parsed)    ; query
          (third  parsed)))) ;fragment))))

(defrule relative-ref (and relative-part (? query) (? fragment))
  (:function extract-fields-from-relative-uri))

(defrule query (and query-delim (* (or pchar "/" "?")))
  (:function second)
  (:text t))

(defrule fragment (and fragment-delim (* (or pchar "/" "?")))
  (:function second)
  (:text t))

(defrule uri-reference (or uri relative-ref))

(defclass uri ()
  ((uri-scheme
    :initform nil
    :initarg  :scheme
    :accessor uri-scheme)
   (uri-user-info
    :initform nil
    :initarg  :user-info
    :accessor uri-user-info)
   (uri-host
    :initform nil
    :initarg  :host
    :writer   (setf uri-scheme))
   (uri-port
    :initform nil
    :initarg  :port
    :accessor uri-port)
   (uri-path
    :initform nil
    :initarg  :path
    :accessor uri-path)
   (uri-query
    :initform nil
    :initarg  :query
    :accessor uri-query)
   (uri-fragment
    :initform nil
    :initarg  :fragment
    :accessor uri-fragment)))

(defgeneric uri-host (object))

(defmethod uri-host ((object uri))
  (let ((host (slot-value object 'uri-host)))
    (if (text-utils:string-starts-with-p "[" host)
        (subseq host 1 (1- (length host)))
        host)))

(defun make-uri (&optional scheme user-info host port path query fragment)
  (make-instance 'uri
                 :scheme    scheme
                 :user-info user-info
                 :host      host
                 :port      port
                 :path      path
                 :query     query
                 :fragment  fragment))

(defun uri-parse (uri)
  (let* ((parsed (parse 'uri-reference uri :junk-allowed nil))
         (res    (mapcar (lambda (a) (cond
                                       ((typep a 'string)
                                        (if (text-utils:string-empty-p a)
                                            nil
                                            a))
                                       (t a)))
                         (list (first   parsed)       ; scheme
                               (second  parsed)       ; user-credentials
                               (third   parsed)       ; host
                               (fourth  parsed)       ; port
                               (fifth   parsed)       ; path
                               (sixth   parsed)       ; query
                               (seventh parsed)))))   ; fragment
    (values (apply #'make-uri res)
            res)))

(defun copy-uri (from)
  (let ((scheme    (uri-scheme    from))
        (user-info (uri-user-info from))
        (host      (slot-value    from 'uri-host))
        (port      (uri-port      from))
        (path      (uri-path      from))
        (query     (uri-query     from))
        (fragment  (uri-fragment  from)))
    (make-uri scheme
              user-info
              host
              port
              path
              query
              fragment)))

(defun render-uri (uri &optional (stream *standard-output*))
  (flet ((render ()
           (with-output-to-string (string-stream)
             (let ((scheme    (uri-scheme    uri))
                   (user-info (uri-user-info uri))
                   (host      (slot-value    uri 'uri-host))
                   (port      (uri-port      uri))
                   (path      (uri-path      uri))
                   (query     (uri-query     uri))
                   (fragment  (uri-fragment  uri)))
               (when scheme
                 (format string-stream "~a:" scheme))
               (write-string "//" string-stream)
               (when user-info
                 (format string-stream "~a@" user-info))
               (when host
                 (format string-stream "~a" host))
               (when port
                 (format string-stream ":~a" port))
               (when path
                 (format string-stream "~a" path))
               (when query
                 (format string-stream "?~a" query))
               (when fragment
                 (format string-stream "#~a" fragment))))))
    (write-string (render) stream)))


(defmethod normalize-path ((object uri:uri))
  (let ((clean-path (normalize-path (uri:uri-path object)))
        (copy       (uri:copy-uri  object)))
    (when clean-path
      (setf (uri:uri-path copy) clean-path))
    copy))

(defgeneric normalize-path (object))

(defmethod normalize-path ((object null))
  nil)

(defmethod normalize-path ((object string))
  (flet ((make-stack ()
           (make-instance 'stack:stack
                          :test-fn #'string=))
         (fill-input-stack (stack)
           (loop
              for segment in (remove-if #'string-empty-p
                                        (reverse (split "/" object)))
              do
                (stack:stack-push stack segment))))
    (let* ((ends-with-separator-p (string-ends-with-p "/" object))
           (ends-with-dots        nil)
           (input-stack  (make-stack))
           (output-stack (make-stack)))
      (fill-input-stack input-stack)
      (labels ((fill-output-buffer ()
                 (when (not (stack:stack-empty-p input-stack))
                   (let ((popped (stack:stack-pop input-stack)))
                     (cond
                       ((and (string= popped "..")
                             (not (stack:stack-empty-p output-stack))
                             (not (stack:stack-empty-p input-stack)))
                        (stack:stack-pop output-stack))
                       ((and (or (string= popped "..")
                                 (string= popped "."))
                             (stack:stack-empty-p input-stack))
                        (setf ends-with-dots t))
                       ((and (string/= popped ".")
                             (string/= popped ".."))
                        (stack:stack-push output-stack popped))))
                   (fill-output-buffer)))
               (output-stack->list ()
                 (reverse (loop
                             for segment = (stack:stack-pop output-stack)
                             while segment
                             collect segment))))
        (fill-output-buffer)
        (let* ((joinable (output-stack->list))
               (merged   (if joinable
                             (if (or ends-with-separator-p
                                     ends-with-dots)
                                 (wrap-with (join-with-strings joinable "/") "/")
                                 (strcat "/" (join-with-strings joinable "/")))
                             "/")))
          (regex-replace-all "//" merged ""))))))

(defmethod to-s ((object uri:uri))
  (with-output-to-string (stream)
    (uri:render-uri object stream)))
