;; tinmop module to expand abbreviations on the command window
;; Copyright © 2021 cage

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

(defparameter *expand-abbrev-rewriting-rules* '(("^\\^g" "gemini://"))
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


(defun expand-abbrev-abbrev-re (abbrev)
  (first abbrev))

(defun expand-abbrev-abbrev-replace (abbrev)
  (second abbrev))

(defparameter *expand-abbrev-actual-rewriting-rules*
  (mapcar (lambda (a) (list (create-scanner (expand-abbrev-abbrev-re a))
                            (expand-abbrev-abbrev-replace a)))
          *expand-abbrev-rewriting-rules*))

(defparameter *expand-abbrev-stop-rewrite* nil)

(defun expand-abbrev-command-hook-fn (command-window)
  (let ((expanded (command-window:command-line command-window)))
    (when (null *expand-abbrev-stop-rewrite*)
      (if (scan "^\\\\." expanded)
          (progn
            (setf *expand-abbrev-stop-rewrite* t)
            (setf expanded (subseq expanded 1)))
          (loop for expansion in *expand-abbrev-actual-rewriting-rules*
                when (scan (expand-abbrev-abbrev-re expansion) expanded)
                  do
                     (setf expanded (regex-replace (expand-abbrev-abbrev-re expansion)
                                                   expanded
                                                   (expand-abbrev-abbrev-replace expansion)))))
      (setf (command-window:command-line command-window) expanded)
      (point-tracker:move-point-to-end command-window expanded)))
  command-window)

(defun expand-abbrev-command-fire-hook (x)
  (declare (ignore x))
  (setf *expand-abbrev-stop-rewrite* nil))

(defun expand-abbrev-command-delete-hook (x)
  (expand-abbrev-command-fire-hook x))

(hooks:add-hook 'hooks:*after-char-to-command-window*
                #'expand-abbrev-command-hook-fn)

(hooks:add-hook 'hooks:*before-fire-string-event-command-window*
                #'expand-abbrev-command-fire-hook)

(hooks:add-hook 'hooks:*after-delete-char-from-command-window*
                #'expand-abbrev-command-delete-hook)
