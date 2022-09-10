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

(in-package :html-utils)

(define-constant +tag-link+          "a" :test #'string=)

(define-constant +tag-break+        "br" :test #'string=)

(define-constant +tag-paragraph+     "p" :test #'string=)

(define-constant +tag-div+         "div" :test #'string=)

(define-constant +attribute-url+  "href" :test #'string=)

(define-constant +http-scheme+    "http" :test #'string=)

(defun http-link-iri-p (iri)
  (conditions:with-default-on-error (nil)
    (or (text-utils:string-starts-with-p +http-scheme+ iri)
        (null (uri:scheme (iri:iri-parse iri))))))

(defun make-tag-node (tag attributes value)
  "create a node"
  (if (listp value)
      (append (list tag attributes) value)
      (list tag attributes value)))

(defun tag (node)
  "Given a node returns the tag part"
  (first node))

(defun attributes (node)
  "Given a node returns the attribute part"
  (second node))

(defun attribute-key (attribute)
  "Given an attribute the key part"
  (first attribute))

(defun attribute-value (attribute)
  "Given an attribute the value part"
  (second attribute))

(defun make-attribute (attribute-name attribute-value)
  (list attribute-name attribute-value))

(defun children (node)
  "Return children of this nodes if exists"
  (when (and node
             (listp node)
             (> (length node)
                2))
    (subseq node 2)))

(defun tag= (tag node)
  (string-equal tag (tag node)))

(defun find-attribute (attribute-key node)
  "find attribute on a node"
  (find-if (lambda (attribute)
             (string= attribute-key
                      (attribute-key attribute)))
           (attributes node)))

(defun find-tag (tag node)
  "find tag on a node list, does not descend into children"
  (find-if (lambda (a) (tag= tag a))
           node))

(defun position-tag (tag node)
  "find position of tag on a node list, does not descend into children"
  (position-if (lambda (a) (tag= tag a))
               node))

(defun add-attribute (attribute-name attribute-value node)
  (make-tag-node (tag node)
                 (append (list (make-attribute attribute-name attribute-value))
                         (attributes node))
                 (children node)))

(defun node->link (node)
  (html-utils:attribute-value (html-utils:find-attribute :href node)))

(defun html->text (html &key (add-link-footnotes t) (body-footnotes-separator ""))
  "Transform html to text, note that if `add-link-footnotes` is non nil footnotes that marks html link in the text are added aftere the body of the message

This function uses a library that transform html5 text into s-expressions um the form

'(name (attributes) children*)

Some convenience functions are provided to works with these structures.
"
  (when html
    (let ((root       (append (list :root
                                    nil)
                              (html5-parser:parse-html5-fragment html :dom :xmls)))
          (link-count 0)
          (body       (misc:make-fresh-array 0 #\a 'character nil))
          (footnotes  (misc:make-fresh-array 0 #\a 'character nil)))
      (with-output-to-string (body-stream body)
        (with-output-to-string (footnotes-stream footnotes)
          (format footnotes-stream "~2%")
          (labels ((descend-children (node)
                     (loop for child in (children node) do
                          (descend child)))
                   (descend (node)
                     (when node
                       (cond
                         ((stringp node)
                          (princ node body-stream))
                         ((consp (car node))
                          (descend (car node)))
                         ((tag= +tag-link+ node)
                          (let ((link (find-attribute +attribute-url+ node)))
                            (incf link-count)
                            (if link
                                (format footnotes-stream
                                        "[~a] ~a~%"
                                        link-count
                                        (attribute-value link))
                                (format footnotes-stream
                                        "[~a] ~a~%"
                                        link-count
                                        (_ "No address found")))
                            (descend-children node)
                            (when add-link-footnotes
                              (format body-stream " [~a] " link-count))))
                         ((tag= +tag-break+ node)
                          (format body-stream "~%")
                          (descend-children node))
                         ((or (tag= +tag-paragraph+ node)
                              (tag= +tag-div+       node))
                          (format body-stream "~%")
                          (descend-children node)
                          (format body-stream "~%"))
                         (t
                          (descend-children node))))))
            (descend root)
            (if add-link-footnotes
                (strcat body body-footnotes-separator footnotes)
                body)))))))

(defun extract-shotcodes (file)
  "Extract shotcodes from the file:
   https://github.com/milesj/emojibase/blob/master/packages/generator/src/resources/shortcodes.ts.
   Returns an alist (cons shortcode utf8-emoj)"
  (with-open-file (stream file)
    (flet ((readline ()
             (read-line stream nil nil)))
      (let ((res ()))
        (loop with i = (readline) while i do
             (multiple-value-bind (match-emoji-p registers-emoji)
                 (cl-ppcre:scan-to-strings  "^\\s+// \(.\) " i)
               (when match-emoji-p
                 (let ((emoji (first-elt registers-emoji)))
                   (setf i (readline))
                   (multiple-value-bind (match-shortcode-p registers-shortcode)
                       (cl-ppcre:scan-to-strings "\\['\([^']+\)'\(\\]|,\)" i)
                     (when match-shortcode-p
                       (setf res
                             (acons (format nil ":~a:" (first-elt registers-shortcode))
                                    (format nil "~a" emoji)
                                    res)))))))
             (setf i (readline)))
        res))))
