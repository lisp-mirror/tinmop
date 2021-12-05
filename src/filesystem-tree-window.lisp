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

(in-package :filesystem-tree-window)

(defun make-node-data (path dirp)
  (list :path path :dirp dirp))

(defun make-root-tree (&optional (path "/"))
  (mtree:make-node (make-node-data path t)))

(defclass filesysten-tree-window (tree-holder
                                  row-oriented-widget
                                  focus-marked-window
                                  title-window)
  ((filesystem-root
    :initform (make-root-tree)
    :initarg  :filesystem-root
    :accessor filesystem-root
    :type     m-tree
    :documentation "The filesystem tree"))
  (:documentation "A window that shows and allow intercating with a hierarchical filesystem"))

(defmethod refresh-config :after ((object filesysten-tree-window))
  (with-croatoan-window (croatoan-window object)
    (refresh-config-colors object swconf:+key-keybindings-window+)
    (refresh-config-sizes  object swconf:+key-keybindings-window+)
    (let ((y (- (win-height *main-window*)
                (win-height object)
                 +command-window-height+)))
    (win-move object 0 y))))

(defmethod calculate :after ((object filesysten-tree-window) dt)
  (declare (ignore object dt)))

(defmethod draw :after ((object filesysten-tree-window)))

(defmacro gen-tree-data-fetcher (name key)
  `(defun ,(misc:format-fn-symbol t "tree-~a" name) (data)
     (getf data ,key)))

(gen-tree-data-fetcher path :path)

(gen-tree-data-fetcher dir-p :dirp)

(gen-tree-data-fetcher rename-to :rename-to)

(gen-tree-data-fetcher delete :delete)

(defun build-data-for-print (data)
  (tree-path data))

(defun treenode->text (data window)
  (declare (ignore window))
  (build-data-for-print data))

(defun treenode->selected-text (data window)
  (tui-string-apply-colors (treenode->text data window)
                           (bgcolor window)
                           (fgcolor window)))

;; (defun expand-local-filesystem-node (matching-node)
;;   (let ((path (tree-path (data matching-node))))

;; (defun expand-treenode (root expand-fn))


(defun build-annotated-tree-rows (window)
  "Split the tree in column to fit the window height and pages to fit window width"
  (with-accessors ((render-arrow-value         render-arrow-value)
                   (render-leaf-value          render-leaf-value)
                   (render-branch-value        render-branch-value)
                   (render-spacer-value        render-spacer-value)
                   (render-vertical-line-value render-vertical-line-value)
                   (filesystem-root            filesystem-root)) window
    (when-let* ((tree-lines (tree->annotated-lines filesystem-root
                                                   :print-data-fn   #'build-data-for-print
                                                   :arrow-char      render-arrow-value
                                                   :spacer-child    render-spacer-value
                                                   :child-char      render-branch-value
                                                   :line-char       render-vertical-line-value
                                                   :last-child-char render-leaf-value
                                                   :print-data      t)))
      (with-accessors ((tree-color-map tree-color-map)) window
        (let ((colorized-rows (loop for line in tree-lines
                                    collect
                                    (colorize-tree-line line tree-color-map))))
          (mapcar (lambda (node-data)
                    (make-instance 'line
                                   :normal-text   (treenode->text          node-data window)
                                   :selected-text (treenode->selected-text node-data window)
                                   :fields        node-data
                                   :normal-bg     (bgcolor window)
                                   :normal-fg     (fgcolor window)
                                   :selected-bg   (fgcolor window)
                                   :selected-fg   (bgcolor window)))
                  colorized-rows))))))

(defun init ()
  "Initialize the window"
  (let* ((low-level-window  (make-croatoan-window :border t))
         (high-level-window (make-instance 'filesysten-tree-window
                                           :croatoan-window low-level-window)))
    (refresh-config high-level-window)
    (win-hide high-level-window)
    high-level-window))
