;; tinmop: an humble gemini and pleroma client
;; Copyright (C) 2018  cage

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

(in-package :complete-window)

(define-constant +starting-item-index+ 0)

(defclass complete-window (suggestions-window)
  ((selected-item-row-index
    :initform +starting-item-index+
    :accessor selected-item-row-index)
   (selected-item-column-index
    :initform +starting-item-index+
    :accessor selected-item-column-index)
   (pagination-info-matched
    :initform nil
    :initarg  :paginated-info-matched
    :accessor paginated-info-matched)
   (foreground-selected-item
    :initform nil
    :initarg  :foreground-selected-item
    :accessor foreground-selected-item)
   (background-selected-item
    :initform nil
    :initarg  :background-selected-item
    :accessor background-selected-item))
  (:documentation "A  window to shows  the possible completion  for an
  user input"))

(defun reset-selected-item (complete-win)
  (setf (selected-item-column-index complete-win) +starting-item-index+
        (selected-item-row-index    complete-win) +starting-item-index+)
  complete-win)

(defmethod calculate ((object complete-window) dt)
  (declare (ignore object dt)))

(defmethod refresh-config :after ((object complete-window))
  (with-accessors ((foreground-selected-item foreground-selected-item)
                   (background-selected-item background-selected-item)) object
    (multiple-value-bind (bg fg)
        (swconf:suggestion-window-selected-item-colors)
      (setf foreground-selected-item fg
            background-selected-item bg)
      object)))

(defmethod update-suggestions ((object complete-window) hint &key &allow-other-keys)
  "List  the   possible  expansion   of  `hint'  using   the  function
`complete:*complete-function*'."
  (flet ((partitions (template data)
           (when data
             (let ((ct 0))
               (loop for page in template
                     collect
                     (loop for column in page
                           collect
                           (loop for row in column
                                 collect
                                 (prog1
                                     (elt data ct)
                                   (incf ct)))))))))
    (with-accessors ((paginated-info         paginated-info)
                     (paginated-info-matched paginated-info-matched)) object
      (multiple-value-bind (candidates common-prefix underline-char-indices)
          (funcall complete:*complete-function* hint)
        (when candidates
          (let* ((max-string-size (max 1 (floor (/ (win-width-no-border object) 2.5))))
                 (truncate-fn        (lambda (batch)
                                       (mapcar (lambda (a)
                                                 (safe-subseq a
                                                              0
                                                              max-string-size))
                                               batch)))
                 (batches (handler-bind ((conditions:out-of-bounds
                                           (lambda (e)
                                             (declare (ignore e))
                                             (invoke-restart 'truncate))))
                            (text-utils:box-fit-multiple-column candidates
                                                                (- (win-width  object) 2)
                                                                (- (win-height object)
                                                                   +box-height-diff+)
                                                                :truncate-restart-fn
                                                                truncate-fn)))
                 (padding-size  (- (length candidates)
                                   (length underline-char-indices)))
                 (padding (when (> padding-size 0)
                            (make-list padding-size :initial-element nil)))
                 (underline-batch (partitions batches (append underline-char-indices padding))))
            (setf paginated-info batches)
            (setf paginated-info-matched underline-batch)
            (values candidates common-prefix underline-batch)))))))

(defmethod draw :after ((object complete-window))
  (with-accessors ((keybindings-tree           keybindings-tree)
                   (paginated-info             paginated-info)
                   (paginated-info-matched     paginated-info-matched)
                   (current-page               current-page)
                   (selected-item-row-index    selected-item-row-index)
                   (selected-item-column-index selected-item-column-index)
                   (foreground-selected-item   foreground-selected-item)
                   (background-selected-item   background-selected-item)) object
    (when-window-shown (object)
      (win-clear object :redraw nil)
      (win-box   object)
      (when paginated-info
        (let ((columns            (elt paginated-info          current-page))
              (indices-matched    (elt paginated-info-matched  current-page))
              (matched-attributes (combine-attributes (attribute-bold)
                                                      (attribute-underline)
                                                      (attribute-reverse))))
          (loop
            for column in columns
            for column-indices in indices-matched
            for column-count from 0
            with column-offset = 1
            do
               (let ((column-size (length (first column))))
                 (loop
                   for row in column
                   for indices-row-underlined in column-indices
                   with row-count = 1 do
                     (let* ((text     (if (and (= row-count    (1+ selected-item-row-index))
                                               (= column-count selected-item-column-index))
                                          (make-tui-string row
                                                           :fgcolor foreground-selected-item
                                                           :bgcolor background-selected-item)
                                          (make-tui-string row)))
                            (tui-text (handler-case
                                          (apply-attributes text
                                                            indices-row-underlined
                                                            matched-attributes)
                                        (error () text))))
                       (print-text object tui-text column-offset row-count))
                     (incf row-count))
                 (incf column-offset column-size)))
          (draw-pagination-info object)))
      (win-refresh object))))

(defun init ()
  "Initialize a complete window"
  (let* ((low-level-window  (make-croatoan-window :border t))
         (high-level-window (make-instance 'complete-window
                                           :croatoan-window low-level-window)))
    (refresh-config high-level-window)
    (win-hide high-level-window)
    high-level-window))
