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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :keybindings-window)

(defclass keybindings-window (suggestions-window tree-holder)
  ((keybindings-tree
    :initform nil
    :initarg  :keybindings-tree
    :accessor keybindings-tree
    :documentation "The keymap"))
  (:documentation "A window that suggests next keys in a keymap"))

(defmethod refresh-config :after ((object keybindings-window))
  (with-croatoan-window (croatoan-window object)
    (refresh-config-colors object swconf:+key-keybindings-window+)
    (refresh-config-sizes  object swconf:+key-keybindings-window+)
    (let ((y (- (win-height *main-window*)
                (win-height object)
                 +command-window-height+)))
    (win-move object 0 y))))

(defmethod calculate ((object keybindings-window) dt)
  (declare (ignore object dt)))

(defun print-suggestion-tree (window)
  "Print a text representation of a tree in `window'"
  (with-accessors ((keybindings-tree keybindings-tree)
                   (paginated-info   paginated-info)
                   (current-page     current-page)) window
    (when-window-shown (window)
      (win-clear window)
      (win-box   window)
      (when paginated-info
        (loop
           for line in (elt paginated-info current-page)
           for row-count from 1 do
             (loop
                for block in line
                with x = 1 do
                  (print-text window block x row-count)
                  (incf x (text-length block))))))))

(defmethod draw :after ((object keybindings-window))
  (labels ((column-size (column)
             (let ((line (first column)))
               (loop for block in line sum (text-length block)))))
    (with-accessors ((keybindings-tree keybindings-tree)
                     (paginated-info   paginated-info)
                     (current-page     current-page)) object
      (when-window-shown (object)
        (win-clear object)
        (win-box   object)
        (when paginated-info
          (loop
             for column in (elt paginated-info current-page)
             with column-count = 1
             do
               (let ((column-size (column-size column)))
                 (loop
                    for row in column
                    with row-count = 1 do
                      (loop
                         for block in row
                         with x = 1 do
                           (print-text object block (+ x column-count) row-count)
                           (incf x (text-length block)))
                      (incf row-count))
                 (incf column-count column-size)))
          (draw-pagination-info object))
        (win-refresh object)))))

(defun build-data-for-print (data)
  (keybindings:humanize-key data))

(defun build-tree-batches (window tree)
  "Split the tree in column to fit the window height and pages to fit window width"
  (with-accessors ((render-arrow-value         render-arrow-value)
                   (render-leaf-value          render-leaf-value)
                   (render-branch-value        render-branch-value)
                   (render-spacer-value        render-spacer-value)
                   (render-vertical-line-value render-vertical-line-value)) window
    (when-let* ((tree-lines (tree->annotated-lines tree
                                                   :print-data-fn   #'build-data-for-print
                                                   :arrow-char      render-arrow-value
                                                   :spacer-child    render-spacer-value
                                                   :child-char      render-branch-value
                                                   :line-char       render-vertical-line-value
                                                   :last-child-char render-leaf-value
                                                   :print-data      t))
                (batches     (handler-bind ((conditions:out-of-bounds
                                              (lambda (e)
                                                (declare (ignore e))
                                                (invoke-restart 'truncate))))
                               (text-utils:box-fit-multiple-column-annotated tree-lines
                                                                             (- (win-width  window) 2)
                                                                             (- (win-height window)
                                                                                +box-height-diff+)))))
      (with-accessors ((tree-color-map tree-color-map)) window
        (let ((colorized-batches (loop for batch in batches collect
                                      (loop for column in batch collect
                                           (loop for line in column collect
                                                (colorize-tree-line line
                                                                    tree-color-map))))))
          colorized-batches)))))

(defmethod update-suggestions ((object keybindings-window) hint
                               &key
                                 (tree nil)
                                 &allow-other-keys)
  "Expand the keybinding tree starting from node `hint' in  slot `keybindings-tree'.

if tree is nil set the slot `keybindings-tree' to `tree'.
"
  (with-accessors ((keybindings-tree keybindings-tree)
                   (paginated-info   paginated-info)
                   (current-page     current-page)) object
    (when tree
      (setf keybindings-tree tree))
    (when hint
      (let ((res (find-keymap-node hint keybindings-tree)))
        (when (typep res 'mtree:m-tree)
          (setf keybindings-tree res)
          (when-let* ((paginated (build-tree-batches object keybindings-tree)))
            (setf paginated-info paginated)
            (setf current-page   0)))
        res))))

(defun init ()
  "Initialize the window"
  (let* ((low-level-window  (make-croatoan-window :border t))
         (high-level-window (make-instance 'keybindings-window
                                           :key-config      swconf:+key-keybindings-window+
                                           :croatoan-window low-level-window)))
    (refresh-config high-level-window)
    (win-hide high-level-window)
    high-level-window))
