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

(in-package :gemini-parser)

(defrule space (or #\Space #\Tab)
  (:constant nil))

(defrule new-line  #\Newline
  (:constant nil))

(defrule carriage-return  #\Return
  (:constant nil))

(defrule cr-lf (and (? carriage-return) new-line)
  (:constant ""))

(defrule h1-prefix "#"
  (:constant :h1))

(defrule h2-prefix "##"
  (:constant :h2))

(defrule h3-prefix "###"
  (:constant :h3))

(defrule list-bullet "* "
  (:constant :li))

(defrule quote-prefix ">"
  (:constant :quote))

(defrule preformatted-text-tag (and "```"
                                    (* (not cr-lf))
                                    cr-lf)
  (:function (lambda (a)
               (list :pre
                     (list (list :alt (coerce (second a) 'string)))))))


(defrule link-prefix (and "=>"
                          (* space))
  (:constant :a))

(defrule text-line (and (+ (not cr-lf)) cr-lf)
  (:text t))

(defrule link-url (+ (not (or space
                              cr-lf)))
  (:text t))

(defrule link-name (+ (not cr-lf))
  (:text t))

(defrule link (and link-prefix
                   link-url
                   (? (and space
                           link-name))
                   cr-lf)
  (:function (lambda (a)
               (list (first a)
                     (list (list :href (second a)))
                     (second (third a))))))

(defrule h1 (and h1-prefix
                 text-line)
  (:function (lambda (a)
               (list (first a)
                     nil
                     (second a)))))

(defrule h2 (and h2-prefix
                 text-line)
  (:function (lambda (a)
               (list (first a)
                     nil
                     (second a)))))

(defrule h3 (and h3-prefix
                 text-line)
  (:function (lambda (a)
               (list (first a)
                     nil
                     (second a)))))

(defrule list-item (and list-bullet
                        text-line)
  (:function (lambda (a)
               (list (first a)
                     nil
                     (second a)))))

(defrule preformatted-text (and preformatted-text-tag
                                (* (not preformatted-text-tag))
                                preformatted-text-tag)
  (:function (lambda (a) (append (first a)
                                 (list (coerce (second a) 'string))))))

(defrule quote-line (and quote-prefix
                         text-line)
  (:function (lambda (a) (list (first a)
                               nil
                               (second a)))))

(defrule gemini-file (* (or h3
                            h2
                            h1
                            preformatted-text
                            link
                            list-item
                            quote-line
                            text-line
                            cr-lf)))


(define-constant +h1-underline+       #\━  :test #'char=)

(define-constant +h2-underline+       #\─  :test #'char=)

(define-constant +h3-underline+       #\-   :test #'char=)

(define-constant +quote-line-prefix+  #\>  :test #'char=)

(define-constant +bullet-line-prefix+ #\•  :test #'char=)

(defun sexp->text (parsed-gemini)
  (labels ((underlineize (stream text underline-char)
             (let* ((size      (length text))
                    (underline (build-string size underline-char)))
               (format stream "~a~%~a~%" text underline)))
           (trim (a)
             (string-trim '(#\Newline #\Return) a))
           (text-value (node &key (trim t))
             (let ((text (first (html-utils:children node))))
               (if trim
                   (trim text)
                   text))))
    (with-output-to-string (stream)
      (loop for node in parsed-gemini do
           (cond
             ((stringp node)
              (format stream "~a~%" (trim node)))
             ((html-utils:tag= :h1 node)
              (underlineize stream
                            (text-value node)
                            +h1-underline+))
             ((html-utils:tag= :h2 node)
              (underlineize stream
                            (text-value node)
                            +h2-underline+))
             ((html-utils:tag= :h1 node)
              (underlineize stream
                            (text-value node)
                            +h3-underline+))
             ((html-utils:tag= :li node)
              (format stream
                      "~a ~a~%"
                      +bullet-line-prefix+
                      (text-value node)))
             ((html-utils:tag= :quote node)
              (format stream
                      "~a ~a~%"
                      +quote-line-prefix+
                      (text-value node)))
             ((html-utils:tag= :pre node)
              (princ (text-value node)
                     stream))
             ((html-utils:tag= :a node)
              (let ((link-name  (text-value node :trim nil))
                    (link-value (html-utils:attribute-value (html-utils:find-attribute :href
                                                                                       node))))
                (if link-name
                    (format stream "[~a]~%" link-name)
                    (format stream "[~a]~%" link-value)))))))))

