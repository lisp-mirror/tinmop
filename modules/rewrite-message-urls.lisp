;; tinmop module for rewrite link URLs before opening
;; Copyright © 2020 cage

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

(in-package :modules)

(defparameter *rewriting-link-rules* '()
  "Before  displaying  messages that  module  will  rewrites the  first
  element of each item of this list with the second

Example

   (\"foo\" \"bar\")
      ^^^
      first
              ^^^
              second

will replace 'foo' with 'bar'.

So the whole list is like: '((\"foo\" \"bar\") (\"old\" \"new\") ...)")

(defun rewriting-link-add-rule (from to)
  (push (list from to)
        *rewriting-link-rules*))

(defun rewriting-link-messages-links-rules (old-links)
  (let ((results ()))
    (loop for old-link in old-links do
      (loop named inner
            for rule in *rewriting-link-rules*
            do
               (let ((rewritten-link (cl-ppcre:regex-replace (first rule)
                                                             old-link
                                                             (second rule))))
                 (when (string/= rewritten-link old-link)
                   (push (cons old-link rewritten-link)
                         results)
                   (return-from inner t)))))
    results))

(defun rewriting-link-replace-mapping (mapping text)
  (cl-ppcre:regex-replace-all (cl-ppcre:quote-meta-chars (car mapping))
                              text
                              (cdr mapping)))

(defun rewriting-link-skipped-row-p (row)
  (let* ((original-type     (message-window:row-get-original-object row))
         (skipped-row-types (list 'gemini-parser:pre-line
                                  'gemini-parser:pre-start
                                  'gemini-parser:pre-end
                                  'gemini-parser:vertical-space)))
    (or (message-window:row-invisible-p row)
        (find-if (lambda (a) (typep original-type a)) skipped-row-types))))

(defun rewriting-link-rewrite-row (window index links-mapping)
  (with-accessors ((rows line-oriented-window:rows)) window
    (when rows
      (let* ((row             (elt rows index))
             (original-string (line-oriented-window:normal-text row)))
        (when (not (rewriting-link-skipped-row-p row))
          (let* ((simple-string   (tui:tui-string->chars-string original-string))
                 (replaced-string simple-string))
            (loop for mapping in links-mapping do
              (setf replaced-string
                    (rewriting-link-replace-mapping mapping replaced-string)))
            (setf (line-oriented-window:normal-text row)
                  (tui:apply-coloring original-string replaced-string))
            (setf (elt rows index)
                  (message-window:text->rendered-lines-rows window row))))))))

(defun rewriting-link-message-hook-fn (message-window)
  (multiple-value-bind (x start-visible-index end-visible-index)
      (message-window:visible-rows message-window)
    (declare (ignore x))
    (loop for i from start-visible-index below end-visible-index do
      (rewriting-link-rewrite-row message-window i *rewriting-link-rules*))
    message-window))

(defun rewriting-link-links-window-hook-fn (all-links)
  (let ((links-mapping  (rewriting-link-messages-links-rules all-links))
        (results        ()))
    (loop for link in all-links do
      (let* ((mapping (find-if (lambda (a) (string= link (car a))) links-mapping))
             (mapped  (if mapping
                          (rewriting-link-replace-mapping mapping link)
                          link)))
        (push mapped results)))
    (reverse results)))

(hooks:add-hook 'hooks:*before-rendering-message-text*
                #'rewriting-link-message-hook-fn)

(hooks:add-hook 'hooks:*before-displaying-links-hook*
                #'rewriting-link-links-window-hook-fn)
