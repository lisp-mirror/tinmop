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

(in-package :iri-parser)

(defrule alpha (character-ranges (#\a #\z) (#\A #\Z))
  (:text t))

(defrule digit (character-ranges (#\0 #\9))
  (:text t))

(defrule scheme-delim #\:
  (:constant :scheme-delim))

(defrule iquery-delim #\?
  (:constant :iquery-delim))

(defrule ifragment-delim #\#
  (:constant :ifragment-delim))

(defrule port-delim #\:
  (:constant :port-delim))

(defrule credential-delim #\@
  (:constant :credential-delim))

(defrule iauthority-start "//"
  (:constant :iauthority-start))

(defrule sub-delims  (or #\!  #\$  #\&  #\'  #\(  #\) #\*  #\+  #\,  #\;  #\=)
  (:text t))

(defrule gen-delims (or ":" "?"  "#"  "["  "]"  "@" "")
  (:text t))

(defrule iunreserved-chars (or alpha digit  #\-  #\.  #\_  #\~ ucschar)
  (:text t))

(defrule iprivate (or (character-ranges (#\UE000 #\UF8FF))
                      (character-ranges (#\UF0000 #\UFFFFD))
                      (character-ranges (#\U100000 #\U10FFFD)))
  (:text t))


(defrule ucschar (or (character-ranges (#\UA0    #\UD7FF))
                     (character-ranges (#\UF900  #\UFDCF))
                     (character-ranges (#\UFDF0  #\UFFEF))
                     (character-ranges (#\U10000 #\U1FFFD))
                     (character-ranges (#\U20000 #\U2FFFD))
                     (character-ranges (#\U30000 #\U3FFFD))
                     (character-ranges (#\U40000 #\U4FFFD))
                     (character-ranges (#\U50000 #\U5FFFD))
                     (character-ranges (#\U60000 #\U6FFFD))
                     (character-ranges (#\U70000 #\U7FFFD))
                     (character-ranges (#\U80000 #\U8FFFD))
                     (character-ranges (#\U90000 #\U9FFFD))
                     (character-ranges (#\UA0000 #\UAFFFD))
                     (character-ranges (#\UB0000 #\UBFFFD))
                     (character-ranges (#\UC0000 #\UCFFFD))
                     (character-ranges (#\UD0000 #\UDFFFD))
                     (character-ranges (#\UE1000 #\UEFFFD)))
  (:text t))

(defrule reserved-chars (or gen-delims sub-delims)
  (:text t))

(defrule scheme (and alpha (* (or alpha  digit  "+"  "-"  "." )))
  (:text t))

(defrule ihier-part  (and iauthority-start iauthority)
  (:function second))

(defrule user-credentials (and iuserinfo credential-delim)
  (:function first))

(defrule port-block (and port-delim port)
  (:function second)
  (:function parse-integer))

(defrule iauthority (and (? user-credentials)
                        ihost
                        (? port-block)))

(defrule ireg-name (* (or iunreserved-chars pct-encoded sub-delims ))
  (:text t))

(defrule ihost (or ipv4-address ip-literal ireg-name)
  (:text t))

(defrule port (+ digit)
  (:text t))

(defrule iuserinfo (* (or iunreserved-chars  pct-encoded  sub-delims  ":" ))
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

(defrule ipchar (or iunreserved-chars pct-encoded  sub-delims  ":"  "@")
  (:text t))

(defrule isegment (* ipchar)
  (:text t))

(defrule isegment-non-zero (+ ipchar)
  (:text t))

(defrule isegment-nz-nc (+ (or iunreserved-chars  pct-encoded  sub-delims  "@" ))
  (:text t))

(defrule ipath-abempty (* (and "/" isegment))
  (:text t))

(defrule ipath  (or ipath-abempty
                    ipath-absolute
                    ipath-noscheme
                    ipath-rootless
                    ipath-empty)
  (:text t))

(defrule ipath-absolute (and "/" (? (and isegment-non-zero (* (and "/" isegment )))))
  (:text t))

(defrule ipath-rootless (and isegment-non-zero (* (and "/" isegment )))
  (:text t))

(defrule ipath-noscheme (and isegment-nz-nc (* (and  "/" isegment )))
  (:text t))

(defrule ipath-empty ""
  (:constant nil))

(defun octect-p (maybe-octect)
  (ignore-errors
   (let ((number (parse-integer (text-utils:strcat* maybe-octect))))
     (when (<= 0 number 255)
       number))))

(defrule dec-octet (octect-p (+ digit))
  (:text t))

(defun extract-fields-from-absolute-iri (parsed)
  (let ((authority (third parsed)))
    (list (first  parsed)    ; scheme
          (first  authority) ; user-credentials
          (second authority) ; host
          (third  authority) ; port
          (fourth parsed)    ; path
          (fifth  parsed)    ; iquery
          (sixth  parsed)))) ; ifragment

(defrule iri (and scheme ":"
                  ihier-part
                  (or ipath-abempty
                      ipath-absolute
                      ipath-noscheme
                      ipath-empty)
                  (? iquery)
                  (? ifragment))
  (:function extract-fields-from-absolute-iri))

(defrule irelative-part (or (and iauthority-start
                                 iauthority
                                 ipath-abempty)
                            ipath-absolute
                            ipath-noscheme
                            ipath-empty))

(defun extract-fields-from-relative-iri-w-authority (parsed)
  ;; ((:IAUTHORITY-START (NIL "bar.baz" NIL) "/foo.gmi") "a=b" "afrag")
  (let ((authority (second (first parsed)))
        (path      (third  (first parsed))))
    (list nil                ; scheme
          (first  authority) ; user-credentials
          (second authority) ; host
          (third  authority) ; port
          path
          (second parsed)    ; iquery
          (third  parsed)))) ; fragment

(defun extract-fields-from-relative-iri-w/o-authority (parsed)
  (list nil                ; scheme
        nil                ; user-credentials
        nil                ; host
        nil                ; port
        (first parsed)     ; path
        (second parsed)    ; iquery
        (third  parsed)))  ; fragment

(defun extract-fields-from-relative-iri (parsed)
  (if (consp (first parsed))
      (extract-fields-from-relative-iri-w-authority   parsed)
      (extract-fields-from-relative-iri-w/o-authority parsed)))

(defrule irelative-ref (and irelative-part (? iquery) (? ifragment))
  (:function extract-fields-from-relative-iri))

(defrule iquery (and iquery-delim (* (or ipchar iprivate "/" "?")))
  (:function second)
  (:text t))

(defrule ifragment (and ifragment-delim (* (or ipchar "/" "?")))
  (:function second)
  (:text t))

(defrule iri-reference (or iri irelative-ref))

(defclass iri (uri:uri) ())

(defun make-iri (&optional scheme user-info host port path query fragment)
  (make-instance 'iri
                 :scheme    scheme
                 :user-info user-info
                 :host      host
                 :port      port
                 :path      path
                 :query     query
                 :fragment  fragment))

(defun iri-parse (iri)
  (let* ((parsed (parse 'iri-reference iri :junk-allowed nil))
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
    (values (apply #'make-iri res)
            res)))

(defun copy-iri (from)
  (let ((scheme    (uri:scheme    from))
        (user-info (uri:user-info from))
        (host      (slot-value from 'uri:host))
        (port      (uri:port      from))
        (path      (uri:path      from))
        (query     (uri:query     from))
        (fragment  (uri:fragment  from)))
    (make-iri scheme
              user-info
              host
              port
              path
              query
              fragment)))

(defmethod uri:normalize-path ((object iri))
  (let ((clean-path (uri:normalize-path (uri:path object)))
        (copy       (copy-iri  object)))
    (when clean-path
      (setf (uri:path copy) clean-path))
    copy))

(defun render-iri (iri &optional (stream *standard-output*))
  (flet ((render ()
           (with-output-to-string (string-stream)
             (let ((scheme    (uri:scheme    iri))
                   (user-info (uri:user-info iri))
                   (host      (slot-value    iri 'uri:host))
                   (port      (uri:port      iri))
                   (path      (uri:path      iri))
                   (query     (uri:query     iri))
                   (fragment  (uri:fragment  iri)))
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

(defmethod to-s ((object iri))
  (with-output-to-string (stream)
    (render-iri object stream)))
