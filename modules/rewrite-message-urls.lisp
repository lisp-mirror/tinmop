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

(defparameter *rewriting-link-rules* ()
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

(defun rewriting-link-message-hook-fn (message-window)
  (with-accessors ((source-text message-window:source-text)) message-window
    (let* ((all-links      (text-utils:collect-links source-text))
           (links-mapping  (rewriting-link-messages-links-rules all-links)))
      (loop for mapping in links-mapping do
        (setf source-text
              (cl-ppcre:regex-replace-all (car mapping)
                                          source-text
                                          (cdr mapping)))))))

(defun rewriting-link-links-window-hook-fn (all-links)
  (let ((links-mapping  (rewriting-link-messages-links-rules all-links))
        (results        ()))
    (loop for link in all-links do
      (let* ((mapping (find-if (lambda (a) (string= link (car a))) links-mapping))
             (mapped  (if mapping
                          (cl-ppcre:regex-replace-all (car mapping)
                                                      link
                                                      (cdr mapping))
                          link)))
        (push mapped results)))
    (reverse results)))

(hooks:add-hook 'hooks:*before-prepare-for-rendering-message*
                #'rewriting-link-message-hook-fn)

(hooks:add-hook 'hooks:*before-displaying-links-hook*
                #'rewriting-link-links-window-hook-fn)
