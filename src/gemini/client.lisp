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

(in-package :gemini-client)

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

(defun find-code-description (code)
  (when-let ((found (find-code-class code)))
    (description found)))

(defun mime-gemini-p (header-meta)
  (string-starts-with-p "text/gemini" header-meta))

(defun mime-text-p (header-meta)
  (string-starts-with-p "text/" header-meta))

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

(defun header-certificate-failure-p (header)
  (or (header-code= header +61+)
      (header-code= header +62+)))

(defun header-not-implemented-p (header)
  (declare (ignore header))
  nil)

(defun header-certificate-requested-p (header)
  (header-code= header +60+))

(defun response-input-p (code)
  (code= code +10+))

(defun response-certificate-requested-p (code)
  (code= code +60+))

(defun response-sensitive-input-p (code)
  (code= code +11+))

(defun response-redirect-p (code)
  (or (code= code +30+)
      (code= code +31+)))

(defun response-success-p (code)
  (code= code +20+))

(define-condition gemini-protocol-error (error)
  ((error-code
    :initarg :error-code
    :reader error-code)
   (error-description
    :initarg :error-description
    :reader  error-description))
  (:report (lambda (condition stream)
             (format stream
                     (_ "The server responded with the error ~a: ~a")
                     (error-code condition)
                     (error-description condition))))
  (:documentation "The condition signalled for error codes (i.e. 4x and 5x)"))

(define-condition gemini-tofu-error (error)
  ((host
    :initarg :host
    :reader host))
  (:report (lambda (condition stream)
             (format stream
                     (_ "The certificate of host ~a has changed from your latest visit.")
                     (host condition))))
  (:documentation "The condition signalled when tofu failed"))

(defparameter *gemini-page-theme* nil)

(defun init-default-gemini-theme (viewport)
  (setf *gemini-page-theme*
        (multiple-value-bind (link-bg link-fg link-attributes)
            (swconf:gemini-link-colors)
          (make-instance 'gemini-parser:gemini-page-theme
                         :viewport           viewport
                         :link-prefix-other  (swconf:gemini-link-prefix-to-other)
                         :link-prefix-gemini (swconf:gemini-link-prefix-to-gemini)
                         :link-bg            link-bg
                         :link-fg            link-fg
                         :link-attributes    link-attributes
                         :quote-prefix       (swconf:gemini-quote-prefix)
                         :h1-prefix          (swconf:gemini-h1-prefix)
                         :h2-prefix          (swconf:gemini-h2-prefix)
                         :h3-prefix          (swconf:gemini-h3-prefix)
                         :bullet-prefix      (swconf:gemini-bullet-prefix)
                         :preformatted-fg    (swconf:gemini-preformatted-fg)))))

(defclass gemini-file-response ()
  ((status-code
    :initform nil
    :initarg  :status-code
    :accessor status-code)
   (status-code-message
    :initform ""
    :initarg  :status-code-message
    :accessor status-code-message)
   (meta
    :initform nil
    :initarg  :meta
    :accessor meta)
   (parsed-file
    :initform nil
    :initarg  :parsed-file
    :accessor parsed-file)
   (source-url
    :initform nil
    :initarg  :source-url
    :accessor source-url)
   (source
    :initform nil
    :initarg  :source
    :accessor source)
   (links
    :initform nil
    :initarg  :links
    :accessor links)
   (text-rendering-theme
    :initform *gemini-page-theme*
    :initarg  :text-rendering-theme
    :accessor text-rendering-theme)))

(defun gemini-file-response-p (object)
  (typep object 'gemini-file-response))

(defun make-gemini-file-response (status-code   status-code-message
                                  meta          parsed-file
                                  source-url    source
                                  links)
  (make-instance 'gemini-file-response
                 :status-code              status-code
                 :status-code-message      status-code-message
                 :meta                     meta
                 :parsed-file              parsed-file
                 :source-url               source-url
                 :source                   source
                 :links                    links))

(defun parse-response (stream)
  (let* ((header-raw   (read-line-into-array stream :add-newline-stopper nil))
         (header       (text-utils:to-s header-raw :errorp nil))
         (parsed-header (parse-gemini-response-header (format nil "~a~a" header #\Newline))))
    (debug-gemini (format nil "response header ~s" header))
    (with-accessors ((meta        meta)
                     (status-code status-code)) parsed-header
      (flet ((results (code-class body)
               (values status-code
                       (description code-class)
                       meta
                       body)))
        (cond
          ((header-success-p parsed-header)
           (values status-code
                   (description +20+)
                   meta
                   stream))
          ((or (header-input-request-p parsed-header)
               (header-redirect-p parsed-header)
               (header-certificate-requested-p parsed-header))
           (results (find-code-class status-code) nil))
          ((or (header-permanent-failure-p   parsed-header)
               (header-temporary-failure-p   parsed-header)
               (header-certificate-failure-p parsed-header))
           (let ((response-code (find-code-class status-code)))
             (error 'gemini-protocol-error
                    :error-code        (code        response-code)
                    :error-description (description response-code))))
          ;; ((header-not-implemented-p parsed-header)
          ;;  (error 'conditions:not-implemented-error
          ;;         :text (_ "The server requested a certificate but client validation is not implemented by this program")))
          (t
           parsed-header))))))

(defun absolute-gemini-url-p (url)
  (when-let ((iri (iri:iri-parse url :null-on-error t)))
    (string= (uri:scheme iri) +gemini-scheme+)))

(defun close-ssl-socket (socket)
  (usocket:socket-close socket))

(defun make-client-certificate (iri)
  (let* ((cache-id (db:cache-put iri +cache-tls-certificate-type+))
         (cert-dir (os-utils:cached-file-path (text-utils:to-s cache-id))))
    (fs:make-directory cert-dir)
    (multiple-value-bind (certificate key)
        (os-utils:generate-ssl-certificate cert-dir)
      (values certificate key))))

(defun percent-encode-path (path)
  (let ((splitted (split "/" path :limit (1+ (length path)))))
    (if splitted
        (reduce (lambda (a b) (strcat a "/" (maybe-percent-encode b)))
                splitted)
        path)))

(defun percent-encode-query (query)
  (maybe-percent-encode query))

(defun percent-encode-fragment (fragment)
  (maybe-percent-encode fragment))

(defun displace-iri (iri)
  (let* ((host       (uri:host     iri))
         (path       (uri:path     iri))
         (query      (uri:query    iri))
         (fragment   (uri:fragment iri))
         (port       (or (uri:port iri)
                         +gemini-default-port+))
         (scheme     (uri:scheme   iri))
         (user-info  (uri:user-info iri))
         (actual-iri (gemini-parser:make-gemini-iri host
                                                    path
                                                    :user-info user-info
                                                    :query     query
                                                    :port      port
                                                    :fragment  fragment
                                                    :scheme    scheme)))
    (values actual-iri
            host
            path
            query
            port
            fragment
            scheme
            user-info)))

(defun debug-gemini (&rest data)
  (declare (ignorable data))
  #+(and debug-mode
         debug-gemini-request)
  (apply #'misc:dbg (text-utils:strcat "[gemini] " (first data)) (rest data)))

(defun open-tls-socket (host port)
  (flet ((open-socket (hostname)
           (usocket:socket-connect hostname
                                   port
                                   :element-type '(unsigned-byte 8))))
    (or (ignore-errors (open-socket host))
        (open-socket (idn:host-unicode->ascii host)))))

(defun request (host path &key
                            (query nil)
                            (port  +gemini-default-port+)
                            (fragment nil)
                            (client-certificate nil)
                            (certificate-key    nil))
  (let* ((iri (make-gemini-iri (idn:host-unicode->ascii host)
                               (percent-encode-path path)
                               :query    (percent-encode-query query)
                               :port     port
                               :fragment (percent-encode-fragment fragment)))
         (ctx (cl+ssl:make-context :verify-mode cl+ssl:+ssl-verify-none+)))
    (cl+ssl:with-global-context (ctx :auto-free-p t)
      (let* ((socket       (open-tls-socket host port))
             (stream       (usocket:socket-stream socket))
             (ssl-hostname (if (or (iri:ipv4-address-p host)
                                   (iri:ipv6-address-p host))
                               nil
                               host))
             (ssl-stream   (cl+ssl:make-ssl-client-stream stream
                                                          :certificate     client-certificate
                                                          :key             certificate-key
                                                          :external-format nil ; unsigned byte 8
                                                          :unwrap-stream-p t
                                                          :verify          nil
                                                          :hostname        ssl-hostname))
             (request    (format nil "~a~a~a" iri #\return #\newline))
             (cert-hash  (crypto-shortcuts:sha512 (x509:dump-certificate ssl-stream))))
        (debug-gemini "sending request ~a" request)
        (if (not (db:tofu-passes-p host cert-hash))
            (error 'gemini-tofu-error :host host)
            (progn
              (write-sequence (babel:string-to-octets request) ssl-stream)
              (force-output ssl-stream)
              (multiple-value-bind (status description meta response)
                  (parse-response ssl-stream)
                (values status description meta response socket))))))))

(defun missing-dispath-function (status code-description meta response socket iri parsed-iri)
  (declare (ignore response socket parsed-iri))
  (error (make-condition 'conditions:not-implemented-error
                         :text (format nil
                                       "received an unknown response from server ~s ~a ~s ~s"
                                       iri status code-description meta))))

(defun request-dispatch (url manage-functions &key (certificate nil) (certificate-key nil))
  (let ((parsed-iri (iri:iri-parse url)))
    (multiple-value-bind (actual-iri host path query port)
        (displace-iri parsed-iri)
      (multiple-value-bind (status code-description meta response socket)
          (gemini-client:request host
                                 path
                                 :certificate-key    certificate-key
                                 :client-certificate certificate
                                 :query              query
                                 :port               port
                                 :fragment           nil)
        (flet ((call-appropriate-function (response-type)
                 (funcall (getf manage-functions
                                response-type
                                #'missing-dispath-function)
                          status
                          code-description
                          meta
                          response
                          socket
                          actual-iri
                          parsed-iri)))
          (cond
            ((gemini-client:response-redirect-p status)
             (call-appropriate-function :redirect))
            ((gemini-client:response-certificate-requested-p status)
             (call-appropriate-function :certificate-requested))
            ((gemini-client:response-success-p status)
             (call-appropriate-function :success))
            ((gemini-client:response-input-p status)
             (call-appropriate-function :input-requested))
            ((gemini-client:response-sensitive-input-p status)
             (call-appropriate-function :sensitive-input-requested))
            (t
             (call-appropriate-function :fallback))))))))

(define-constant +allowed-dispatch-keys+ '(:redirect
                                           :certificate-requested
                                           :success
                                           :input-requested
                                           :sensitive-input-requested
                                           :fallback)
  :test #'equalp)

(defmacro with-request-dispatch-table ((table &key (ignore-warning nil)) &body body)
  "Anaphoric, the anaphora is `dispatch-table'"
  (assert (listp table))
  (let* ((unknown-keys (loop for i in (remove-if-not #'keywordp table)
                             when (not (find i +allowed-dispatch-keys+))
                               collect i)))
    (when (not ignore-warning)
      (when (null table)
        (error "Empty dispatch-table"))
      (when unknown-keys
        (warn (format nil
                      "found unkown keys in dispatch-table table: ~s"
                      unknown-keys)))
      (when (null (getf table :redirect))
        (warn "No dispatch for redirect found"))
      (when (null (getf  table :certificate-requested))
        (warn "No dispatch for certificate request"))
      (when (null (getf table :success))
        (warn "No dispatch for success found"))
      (when (null (getf table :input-requested))
        (warn "No dispatch for input request"))
      (when (null (getf table :sensitive-input-requested))
        (warn "No dispatch for sensitive-input request")))
    `(let ((dispatch-table (list ,@table)))
       ,@body)))

(defun gemini-file-stream-p (meta)
  (mime-gemini-p meta))

(defun text-file-stream-p (meta)
  (mime-text-p meta))

(defun tls-cert-find (request-iri)
  (when-let* ((all-rows           (db:find-tls-certificates-rows))
              (parsed-request-iri (iri:iri-parse request-iri :null-on-error t)))
    (multiple-value-bind (request-iri request-host request-path request-query request-port
                          request-fragment request-scheme request-user-info)
        (gemini-client:displace-iri parsed-request-iri)
      (declare (ignore request-iri request-query request-fragment))
      (loop for row in all-rows do
        (let ((id (db:row-id row)))
          (multiple-value-bind (iri host path query port fragment scheme user-info)
              (gemini-client:displace-iri (iri:iri-parse (db:row-cache-key row)))
            (declare (ignore iri query fragment))
            (when (and (string= request-host      host)
                       (string= request-scheme    scheme)
                       (string= request-user-info user-info)
                       (=       request-port      port)
                       (text-utils:string-starts-with-p (gemini-parser:path-last-dir path)
                                                        request-path))
              (return-from tls-cert-find
                (values (strcat (os-utils:cached-file-path (to-s id))
                                fs:*directory-sep* os-utils:+ssl-cert-name+)
                        (strcat (os-utils:cached-file-path (to-s id))
                                fs:*directory-sep* os-utils:+ssl-key-name+))))))))))

(defun fetch-cached-certificate (url)
  (let ((certificate nil)
        (key         nil))
    (multiple-value-bind (certificate-cache key-cache)
        (tls-cert-find url)
      (if (and certificate-cache
               key-cache)
          (setf certificate certificate-cache
                key         key-cache)
          (multiple-value-bind (certificate-new key-new)
              (gemini-client:make-client-certificate url)
            (setf certificate certificate-new
                  key         key-new)))
      (assert certificate)
      (assert key)
      (values certificate key))))

(defgeneric build-redirect-iri (meta iri-from))

(defmethod build-redirect-iri (meta (iri-from iri:iri))
  (let* ((meta-url        (ignore-errors (iri:iri-parse meta))))
    (when meta-url
      (let* ((meta-query      (uri:query     meta-url))
             (meta-path       (uri:path      meta-url))
             (meta-path-query (if meta-query
                                  (strcat meta-path "?" meta-query)
                                  meta-path))
             (new-url    (gemini-parser:absolutize-link meta-path-query
                                                        (uri:host iri-from)
                                                        (uri:port iri-from)
                                                        (uri:path iri-from))))
        new-url))))

(defmethod build-redirect-iri (meta (iri-from string))
  (build-redirect-iri meta (iri:iri-parse iri-from)))

(define-constant +maximum-redirections+ 5 :test `=)

(defun slurp-gemini-url (url &optional (redirect-count 0))
  "Read 'full'  data from gemini  address `url'; note that  specs says
that gemini flow  is streamed by default so this  function has limited
use as there is a chance that  it would not returns. Anyway for gemlog
subscription (for example) could be used.

TODO: Add client certificate."
  (labels ((redirect-dispatch (status code-description meta response socket iri parsed-iri)
             (declare (ignore status code-description response socket parsed-iri))
             (when (< redirect-count +maximum-redirections+)
               (when-let ((iri-redirect (build-redirect-iri meta iri)))
                 (slurp-gemini-url iri-redirect (1+ redirect-count)))))
           (success-dispatch (status code-description meta response socket iri parsed-iri)
             (declare (ignorable code-description iri meta parsed-iri))
             (debug-gemini "success response data: ~s ~s ~s ~s ~s ~s"
                           status code-description meta response socket iri)
             (let ((data (misc:make-fresh-array 0 0 '(unsigned-byte 8) nil)))
               (loop for new-byte = (read-byte response nil nil)
                     while new-byte do
                       (vector-push-extend new-byte data))
               (close-ssl-socket socket)
               data)))
    (if (absolute-gemini-url-p url)
        (with-request-dispatch-table ((:success  #'success-dispatch
                                       :redirect #'redirect-dispatch)
                                      :ignore-warning t)
          (request-dispatch url dispatch-table))
        (fs:slurp-file url :convert-to-string nil))))
