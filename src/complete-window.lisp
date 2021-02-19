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

(defclass complete-window (suggestions-window)
  ()
  (:documentation "A  window to shows  the possible completion  for an
  user input"))

(defmethod calculate ((object complete-window) dt)
  (declare (ignore object dt)))

(defmethod update-suggestions ((object complete-window) hint &key &allow-other-keys)
  "List  the   possible  expansion   of  `hint'  using   the  function
`complete:*complete-function*'."
  (with-accessors ((paginated-info paginated-info)) object
    (multiple-value-bind (candidates common-prefix)
        (funcall complete:*complete-function* hint)
      (when candidates
        (when-let ((batches (text-utils:box-fit-multiple-column candidates
                                                                (- (win-width  object) 2)
                                                                (- (win-height object)
                                                                   +box-height-diff+))))
          (setf paginated-info batches)
          (values candidates common-prefix))))))

(defmethod draw :after ((object complete-window))
  (with-accessors ((keybindings-tree keybindings-tree)
                   (paginated-info   paginated-info)
                   (current-page     current-page)) object
    (when-window-shown (object)
      (win-clear object :redraw nil)
      (win-box   object)
      (when paginated-info
        (loop
           for column in (elt paginated-info current-page)
           with column-count = 1
           do
             (let ((column-size (length (first column))))
               (loop
                  for row in column
                  with row-count = 1 do
                    (print-text object row column-count row-count)
                    (incf row-count))
               (incf column-count column-size)))
        (draw-pagination-info object))
      (win-refresh object))))

(defun init ()
  "Initialize a complete window"
  (let* ((low-level-window  (make-croatoan-window :border t))
         (high-level-window (make-instance 'complete-window
                                           :croatoan-window low-level-window)))
    (refresh-config high-level-window)
    (win-hide high-level-window)
    high-level-window))
