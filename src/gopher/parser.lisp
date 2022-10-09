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

(in-package :gopher-parser)

(defmacro def-line-type-constant (name value documentation)
  `(a:define-constant ,(format-fn-symbol t "+line-type-~a+" name) ,value
     :test #'string=
     :documentation ,documentation))

(defmacro gen-line-constants (name-values-doc-list)
  `(progn
     ,@(loop for data in name-values-doc-list
             collect
             `(def-line-type-constant ,(first  data) ,(second data) ,(third data)))))

(gen-line-constants ((file             "0" "identifier for a file")
                     (dir              "1" "identifier for a directory")
                     (cso              "2" "identifier for a CSO phone-book server")
                     (error            "3" "identifier for an error")
                     (mac-hex-file     "4" "identifier for a BinHexed Macintosh file")
                     (dos-archive-file "5" "identifier for a DOS binary archive of some sort")
                     (uuencoded-file   "6" "identifier for a UNIX uuencoded file")
                     (index-search     "7" "identifier for Index-Search server")
                     (telnet-session   "8" "identifier for a text-based telnet session.")
                     (binary-file      "9" "identifier for a binary file")
                     (redundant-server "+" "identifier for a redundant server")
                     (tn3270-session   "T" "identifier for a tn3270 session")
                     (gif-image-file   "g" "identifier for an image in GIF")
                     (image-file       "I" "identifier for an image file")
                     (info             "i" "information line")
                     (uri              "h" "hyperlink")))

(a:define-constant +gopher-scheme+ "gopher" :test #'string=)

(defun %check-line-type (data reference)
  (string= data reference))

(defmacro %gen-check-line-predicate (name reference)
  (a:with-gensyms (data)
  `(defun ,(format-fn-symbol t "%line-type-~a-p" name) (,data)
     (%check-line-type ,data ,reference))))

(%gen-check-line-predicate file             +line-type-file+)

(%gen-check-line-predicate dir              +line-type-dir+)

(%gen-check-line-predicate cso              +line-type-cso+)

(%gen-check-line-predicate error            +line-type-error+)

(%gen-check-line-predicate mac-hex-file     +line-type-mac-hex-file+)

(%gen-check-line-predicate dos-archive-file +line-type-dos-archive-file+)

(%gen-check-line-predicate uuencoded-file   +line-type-uuencoded-file+)

(%gen-check-line-predicate index-search     +line-type-index-search+)

(%gen-check-line-predicate telnet-session   +line-type-telnet-session+)

(%gen-check-line-predicate binary-file      +line-type-binary-file+)

(%gen-check-line-predicate redundant-server +line-type-redundant-server+)

(%gen-check-line-predicate tn3270-session   +line-type-tn3270-session+)

(%gen-check-line-predicate gif-file         +line-type-gif-image-file+)

(%gen-check-line-predicate image-file       +line-type-image-file+)

(%gen-check-line-predicate info             +line-type-info+)

(%gen-check-line-predicate uri              +line-type-uri+)

(defclass gopher-line ()
  ((line-type-id
    :initarg :line-type-id
    :initform ""
    :accessor line-type-id
    :type     string)
   (username
    :initarg :username
    :initform ""
    :accessor username
    :type     string)
   (selector
    :initarg :selector
    :initform ""
    :accessor selector
    :type     string)
   (host
    :initarg :host
    :initform ""
    :accessor host
    :type     string)
   (port
    :initarg :port
    :initform -1
    :accessor port
    :type     number)))

(defmethod print-object ((object gopher-line) stream)
  (with-accessors ((username username)
                   (selector selector)
                   (host     host)
                   (port     port)) object
    (print-unreadable-object (object stream :type t)
      (format stream
              "username: ~s selector: ~s host: ~s port ~a"
              username selector host port))))

(defmacro gen-selector-class (name)
  `(defclass ,name (gopher-line) ()))

(gen-selector-class line-file)

(gen-selector-class line-dir)

(gen-selector-class line-cso)

(gen-selector-class line-error)

(gen-selector-class line-mac-hex-file)

(gen-selector-class line-dos-archive-file)

(gen-selector-class line-uuencoded-file)

(gen-selector-class line-index-search)

(gen-selector-class line-telnet-session)

(gen-selector-class line-binary-file)

(gen-selector-class line-redundant-server)

(gen-selector-class line-tn3270-session)

(gen-selector-class line-gif-file)

(gen-selector-class line-image-file)

(gen-selector-class line-info)

(gen-selector-class line-uri)

(gen-selector-class line-unknown)

(defun check-line-type (data reference)
  (typep data reference))

(defmacro gen-check-line-predicate (name reference)
  (a:with-gensyms (data)
  `(defun ,(format-fn-symbol t "line-type-~a-p" name) (,data)
     (check-line-type ,data ,reference))))

(gen-check-line-predicate file             'line-file)

(gen-check-line-predicate dir              'line-dir)

(gen-check-line-predicate cso              'line-cso)

(gen-check-line-predicate error            'line-error)

(gen-check-line-predicate mac-hex-file     'line-mac-hex-file)

(gen-check-line-predicate dos-archive-file 'line-dos-archive-file)

(gen-check-line-predicate uuencoded-file   'line-uuencoded-file)

(gen-check-line-predicate index-search     'line-index-search)

(gen-check-line-predicate telnet-session   'line-telnet-session)

(gen-check-line-predicate binary-file      'line-binary-file)

(gen-check-line-predicate redundant-server 'line-redundant-server)

(gen-check-line-predicate tn3270-session   'line-tn3270-session)

(gen-check-line-predicate gif-file         'line-gif-image-file)

(gen-check-line-predicate image-file       'line-image-file)

(gen-check-line-predicate info             'line-info)

(gen-check-line-predicate uri              'line-uri)

(gen-check-line-predicate unknown          'unknown)

(defrule line-separator (and #\Return #\Newline)
  (:constant :line-separator))

(defrule field-separator #\tab
  (:constant :field-separator))

(defrule null-char #\Nul
  (:constant :field-separator))

(defrule unascii (not (or field-separator line-separator null-char))
  (:text t))

(defrule last-line (and #\. line-separator)
  (:constant :last-line))

(defrule line-type unascii
  (:text t))

(defrule red-type (and #\+ #\.)
  (:constant :red-type))

(defrule user-name (* unascii)
  (:text t))

(defrule selector (* unascii)
  (:text t))

(defrule hostname-component (* (not (or field-separator line-separator null-char
                                        #\.)))
  (:text t))

(defrule host (and (* (and hostname-component #\.))
                   hostname-component)
  (:text t))

(defrule digit (character-ranges #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
  (:text t))

(defrule digit-sequence (and digit (* digit))
  (:text t))

(defrule port digit-sequence
  (:function parse-integer))

(defrule dir-entity (and line-type user-name field-separator
                         selector field-separator
                         host field-separator
                         port line-separator)
  (:function (lambda (line)
               (list :type (first line)
                     :user-name (second line)
                     :selector  (fourth line)
                     :host      (sixth  line)
                     :port      (elt    line 7)))))

(defrule menu (and (* dir-entity) (? last-line))
  (:function first))

(defun parse-menu (data)
  (let ((menu (parse 'menu data)))
    (loop for entry in menu
          collect
          (let* ((line-type (getf entry :type))
                 (instance  (cond
                              ((%line-type-file-p line-type)
                               (make-instance 'line-file))
                              ((%line-type-dir-p line-type)
                               (make-instance 'line-dir))
                              ((%line-type-cso-p line-type)
                               (make-instance 'line-cso))
                              ((%line-type-error-p line-type)
                               (make-instance 'line-error))
                              ((%line-type-mac-hex-file-p line-type)
                               (make-instance 'line-mac-hex-file))
                              ((%line-type-dos-archive-file-p line-type)
                               (make-instance 'line-dos-archive-file))
                              ((%line-type-uuencoded-file-p line-type)
                               (make-instance 'line-uuencoded-file))
                              ((%line-type-index-search-p line-type)
                               (make-instance 'line-index-search))
                              ((%line-type-telnet-session-p line-type)
                               (make-instance 'line-telnet-session))
                              ((%line-type-binary-file-p line-type)
                               (make-instance 'line-binary-file))
                              ((%line-type-redundant-server-p line-type)
                               (make-instance 'line-redundant-server))
                              ((%line-type-tn3270-session-p line-type)
                               (make-instance 'line-tn3270-session))
                              ((%line-type-gif-file-p line-type)
                               (make-instance 'line-gif-file))
                              ((%line-type-image-file-p line-type)
                               (make-instance 'line-image-file))
                              ((%line-type-info-p line-type)
                               (make-instance 'line-info))
                              ((%line-type-uri-p line-type)
                               (make-instance 'line-uri))
                              (t
                               (make-instance 'line-unknown)))))
            (setf (line-type-id instance) (getf entry :type)
                  (username     instance) (getf entry :user-name)
                  (selector     instance) (getf entry :selector)
                  (host         instance) (getf entry :host)
                  (port         instance) (getf entry :port))
            instance))))

(defrule text-block (+ (not (and #\Newline #\. #\Return #\Newline)))
  (:text t))

(defrule text-file (and (* text-block) (and #\Newline #\. #\Return #\Newline))
  (:function caar))

(defun parse-text-file (data)
  (parse 'text-file data))

(defrule gopher-url-authority (or (and (+ (not #\:))
                                       #\:
                                       (+ (not #\/))
                                       #\/)
                                  (and (+ (not #\/))
                                       #\/))
  (:function (lambda (a)
               (let* ((host-port a)
                      (host (coerce (first host-port) 'string))
                      (port (if (third host-port)
                                (parse-integer (coerce (third host-port) 'string))
                                70)))
                 (list host port)))))

(defrule gopher-url (and (+ (not #\:))
                         "://"
                         gopher-url-authority
                         (? (and (not #\/)
                                 (& #\/)))
                         (* (character-ranges (#\u0000 #\uffff))))
  (:function (lambda (a)
               (let* ((host-port (third a))
                      (host (coerce (first host-port) 'string))
                      (port (if (third host-port)
                                (parse-integer (coerce (third host-port) 'string))
                                70))
                      (type-path (fourth a))
                      (type      (if (car type-path)
                                     (string (car type-path))
                                     +line-type-dir+))
                      (path      (coerce (fifth a) 'string)))
                 (when (and (string-not-empty-p path)
                            (not (car type-path)))
                   (setf path (strcat "/" path)))
               (list host port path type)))))

(defun parse-iri (iri)
  (let* ((parsed   (parse 'gopher-url iri))
         (host     (first parsed))
         (port     (second parsed))
         (selector (third  parsed))
         (type     (fourth parsed)))
    (values host port type selector)))
