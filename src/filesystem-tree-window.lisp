;; tinmop: an humble gemini and pleroma client
;; Copyright (C) 2021  cage

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
  (cond
    ((or (fs:backreference-dir-p  path)
         (fs:loopback-reference-dir-p path)
         (not dirp))
     (list :path path :dirp dirp))
    (dirp
     (if (scan "/$" path)
         (list :path path :dirp t)
         (list :path (strcat path "/") :dirp t)))))

(defun make-root-tree (&optional (path "/"))
  (mtree:make-node (make-node-data path t)))

(defclass filesystem-tree-window (wrapper-window
                                  tree-holder
                                  key-config-holder
                                  row-oriented-widget
                                  focus-marked-window
                                  title-window
                                  border-window)
  ((filesystem-root
    :initform (make-root-tree)
    :initarg  :filesystem-root
    :accessor filesystem-root
    :type     m-tree
    :documentation "The filesystem tree")
   (filesystem-expand-function
    :initform  #'expand-local-filesystem-node
    :accessor  filesystem-expand-function
    :type      function)
   (filesystem-rename-function
    :initform  #'rename-local-filesystem-node
    :accessor  filesystem-rename-function
    :type      function)
   (filesystem-delete-function
    :initform  #'delete-local-filesystem-node
    :accessor  filesystem-delete-function
    :type      function)
   (filesystem-create-function
    :initform  #'create-local-filesystem-node
    :accessor  filesystem-create-function
    :type      function)
   (filesystem-download-function
    :initform  #'download-local-filesystem-node
    :accessor  filesystem-download-function
    :type      function)
   (filesystem-upload-function
    :initform  #'upload-local-filesystem-node
    :accessor  filesystem-upload-function
    :type      function))
  (:documentation "A window that shows and allow interacting with a hierarchical filesystem"))

(defmethod refresh-config :after ((object filesystem-tree-window))
  (with-croatoan-window (croatoan-window object)
    (refresh-config-colors object swconf:+key-file-explorer+)
    (refresh-config-sizes  object swconf:+key-file-explorer+)
    (let ((y (- (win-height *main-window*)
                (win-height object)
                 +command-window-height+)))
    (win-move object 0 y))))

(defmethod calculate :after ((object filesystem-tree-window) dt)
  (declare (ignore object dt)))

(defmethod draw :after ((object filesystem-tree-window)))

(defmacro gen-tree-data-fetcher (name key)
  `(defun ,(misc:format-fn-symbol t "tree-~a" name) (data)
     (getf data ,key)))

(gen-tree-data-fetcher path :path)

(gen-tree-data-fetcher dir-p :dirp)

(gen-tree-data-fetcher marked-p :markedp)

(defun build-data-for-print (data)
  (tree-path data))

(defun treenode->text (data window)
  (declare (ignore window))
  (build-data-for-print data))

(defun treenode->selected-text (data window)
  (tui-string-apply-colors (treenode->text data window)
                           (bgcolor window)
                           (fgcolor window)))

(defun expand-local-filesystem-node (matching-node)
  (let ((path (tree-path (data matching-node))))
    (assert path)
    (let* ((children    (mapcar (lambda (a)
                                  (if (not (or (fs:backreference-dir-p a)
                                               (fs:loopback-reference-dir-p a)))
                                      (uri:normalize-path a)
                                      a))
                                (fs:collect-children path)))
           (files       (remove-if #'fs:dirp children))
           (directories (remove-if-not #'fs:dirp children)))
      (when (or files directories)
        (remove-all-children matching-node)
        (loop for directory in directories do
          (add-child matching-node
                     (make-instance 'm-tree :data (make-node-data directory t))))
        (loop for file in files do
          (add-child matching-node
                     (make-instance 'm-tree :data (make-node-data file nil)))))
      matching-node)))

(defun create-local-filesystem-node (path dirp)
  (assert path)
  (if dirp
      (fs:make-directory path)
      (fs:create-a-file path)))

(defun rename-local-filesystem-node (matching-node new-path)
  (let ((path (tree-path (data matching-node))))
    (assert path)
    (fs:rename-a-file path new-path)))

(defun delete-local-filesystem-node (matching-node)
  (let ((path (tree-path (data matching-node))))
    (assert path)
    (fs:recursive-delete path)))

(define-constant +download-buffer+ (expt 2 24)         :test #'=)

(define-constant +octect-type+     '(unsigned-byte 8) :test #'equalp)

(defun download-local-filesystem-node (matching-node
                                       &optional (destination-file (fs:temporary-file)))
  (with-open-file (input-stream (tree-path (data matching-node))
                                :direction    :input
                                :element-type +octect-type+)
    (with-open-file (output-stream destination-file
                                   :direction         :output
                                   :if-exists         :supersede
                                   :if-does-not-exist :create
                                   :element-type      +octect-type+)
      (let* ((buffer (misc:make-array-frame +download-buffer+ 0 '(unsigned-byte 8) t)))
        (loop named write-loop
              for read-so-far = (read-sequence buffer input-stream)
                then (read-sequence buffer input-stream)
              do
                 (write-sequence buffer output-stream :start 0 :end read-so-far)
                 (when (< read-so-far +download-buffer+)
                   (return-from write-loop t))))))
  destination-file)

(defun upload-local-filesystem-node (source-path matching-node)
  (with-open-file (input-stream source-path
                                :direction    :input
                                :element-type +octect-type+)
    (with-open-file (output-stream (tree-path (data matching-node))
                                   :direction         :output
                                   :if-exists         :error
                                   :if-does-not-exist :create
                                   :element-type      +octect-type+)
      (let* ((buffer (misc:make-array-frame +download-buffer+ 0 '(unsigned-byte 8) t)))
        (loop named write-loop
              for read-so-far = (read-sequence buffer input-stream)
                then (read-sequence buffer input-stream)
              do
                 (write-sequence buffer output-stream :start 0 :end read-so-far)
                 (when (< read-so-far +download-buffer+)
                   (return-from write-loop t))))))
  (tree-path (data matching-node)))

(defun %expand-treenode (root path-to-expand expand-fn)
  (when-let ((matching-node (first (mtree:find-child-if root
                                                        (lambda (a)
                                                          (string= (tree-path (data a))
                                                                   path-to-expand))))))
    (funcall expand-fn matching-node)))

(defmethod draw :around ((object filesystem-tree-window))
  (when-window-shown (object)
    (call-next-method)))

(defun %build-annotated-tree-rows (window root-node)
  (with-accessors ((render-arrow-value         render-arrow-value)
                   (render-leaf-value          render-leaf-value)
                   (render-branch-value        render-branch-value)
                   (render-spacer-value        render-spacer-value)
                   (render-vertical-line-value render-vertical-line-value)
                   (filesystem-root            filesystem-root)) window
    (when-let* ((tree-lines (tree->annotated-lines root-node
                                                   :print-data-fn   #'build-data-for-print
                                                   :arrow-char      render-arrow-value
                                                   :spacer-child    render-spacer-value
                                                   :child-char      render-branch-value
                                                   :line-char       render-vertical-line-value
                                                   :last-child-char render-leaf-value
                                                   :print-data      t))
                (tree-data (collect-nodes-data root-node)))
      (with-accessors ((tree-color-map tree-color-map)) window
        (let ((colorized-rows (loop for line in tree-lines
                                    collect
                                    (reduce #'cat-tui-string
                                            (colorize-tree-line line tree-color-map)))))
          (mapcar (lambda (colored-text node-data)
                    (make-instance 'line
                                   :normal-text   colored-text
                                   :selected-text
                                   (tui-string-apply-colors colored-text
                                                            (win-bgcolor window)
                                                            (win-fgcolor window))
                                   :fields        node-data
                                   :normal-bg     (win-bgcolor window)
                                   :normal-fg     (win-fgcolor window)
                                   :selected-bg   (win-fgcolor window)
                                   :selected-fg   (win-bgcolor window)))
                  colorized-rows
                  (reverse tree-data)))))))

(defun build-annotated-tree-rows (window)
  (%build-annotated-tree-rows window (filesystem-root window)))

(defmethod resync-rows-db ((object filesystem-tree-window)
                           &key
                             (redraw t) (selected-path nil))
  (with-accessors ((rows             rows)
                   (selected-line-bg selected-line-bg)
                   (selected-line-fg selected-line-fg)) object
    (line-oriented-window:update-all-rows object
                                          (build-annotated-tree-rows object))
    (when (string-not-empty-p selected-path)
      (when-let ((index (rows-position-if object
                                          (lambda (a)
                                            (string= (tree-path (fields a))
                                                     selected-path)))))
        (select-row object index)))
    (when redraw
      (draw object))
    object))

(defun find-node (root-node matching-path)
  (first (mtree:find-child-if root-node
                              (lambda (a)
                                (string= (tree-path (data a))
                                         matching-path)))))

(defun close-treenode (window node-path)
  (when-let ((matching-node (find-node (filesystem-root window) node-path)))
    (remove-all-children matching-node)
    (win-clear window :redraw nil)
    (resync-rows-db window :redraw t :selected-path node-path)))

(defun jump-to-parent-node (window path)
  (when (fs:backreference-dir-p path)
    (let ((parent-path (fs:parent-dir-path (uri:normalize-path path))))
      (win-clear window :redraw nil)
      (resync-rows-db window :selected-path parent-path :redraw t))))

(defun expand-treenode (window expand-root-path &key (recurse t))
  (with-accessors ((filesystem-root            filesystem-root)
                   (filesystem-expand-function filesystem-expand-function)) window
    (if (or (fs:backreference-dir-p      expand-root-path)
            (fs:loopback-reference-dir-p expand-root-path))
        (jump-to-parent-node window expand-root-path)
        (when-let ((matching-node (first (mtree:find-child-if filesystem-root
                                                              (lambda (a)
                                                                (string= (tree-path (data a))
                                                                         expand-root-path))))))
          (when (tree-dir-p (data matching-node))
            (%expand-treenode filesystem-root expand-root-path filesystem-expand-function)
            (resync-rows-db window :selected-path expand-root-path :redraw nil)
            (when recurse
              (let* ((expanded-tree (%expand-treenode (make-root-tree expand-root-path)
                                                      expand-root-path
                                                      filesystem-expand-function))
                     (expanded-rows (%build-annotated-tree-rows window expanded-tree))

                     (window-width  (usable-window-width window))
                     (max-line-width nil))
                (loop for expanded-row in expanded-rows
                      when (> (text-width (normal-text expanded-row))
                              window-width)
                        do
                           (setf max-line-width expanded-row))
                (when max-line-width
                  (let ((new-root (fs:parent-dir-path (tree-path (fields max-line-width)))))
                    (setf filesystem-root (make-root-tree new-root))
                    (expand-treenode window new-root :recurse nil))))))
          (win-clear window :redraw nil)
          (draw window)))))

(defun rename-treenode (window old-path new-path)
  (when-let* ((root-node     (filesystem-root window))
              (matching-node (find-node root-node old-path))
              (parent-node   (find-node root-node
                                        (fs:parent-dir-path (tree-path (data matching-node))))))
    (funcall (filesystem-rename-function window)
             matching-node
             new-path)
    (remove-all-children parent-node)
    (expand-treenode window (tree-path (data parent-node)))
    (win-clear window :redraw nil)
    (resync-rows-db window :redraw t :selected-path new-path)))

(defun delete-treenode (window path)
  (when-let* ((root-node     (filesystem-root window))
              (matching-node (find-node root-node path))
              (parent-node   (find-node root-node
                                        (fs:parent-dir-path (tree-path (data matching-node))))))
    (funcall (filesystem-delete-function window)
             matching-node)
    (remove-all-children parent-node)
    (expand-treenode window (tree-path (data parent-node)))
    (win-clear window :redraw nil)
    (resync-rows-db window :redraw t :selected-path path)))

(defun create-treenode (window path dirp)
  (when-let* ((root-node     (filesystem-root window))
              (parent-node   (find-node root-node (fs:parent-dir-path path))))
    (funcall (filesystem-create-function window) path dirp)
    (remove-all-children parent-node)
    (expand-treenode window (tree-path (data parent-node)))
    (win-clear window :redraw nil)
    (resync-rows-db window :redraw t :selected-path path)))

(defun download-treenode (window remote-path
                          &optional (destination-file (fs:temporary-file)))
  (when-let* ((root-node     (filesystem-root window))
              (matching-node (find-node root-node remote-path)))
    (funcall (filesystem-download-function window) matching-node destination-file)))

(defun upload-treenode (window source-file remote-path)
  (when-let* ((root-node     (filesystem-root window))
              (matching-node (find-node root-node remote-path))
              (filep         (not (tree-dir-p (data matching-node))))
              (parent-node   (find-node root-node (fs:parent-dir-path remote-path)))
              (parent-path   (tree-path (data parent-node))))
    (funcall (filesystem-download-function window) source-file matching-node)
    (remove-all-children parent-node)
    (expand-treenode window parent-path)
    (win-clear window :redraw nil)
    (resync-rows-db window :redraw t :selected-path parent-path)))

(defmethod draw :after ((object filesystem-tree-window))
  (when-window-shown (object)
    (let* ((window-width  (usable-window-width object))
           (rows          (renderizable-rows-data object))
           (x             (if (uses-border-p object)
                              1
                              0))
           (y-start       (if (uses-border-p object)
                              1
                              0)))
      (loop
        for y from y-start by 1
        for ct from 0
        for row in rows do
          (if (selectedp row)
              (print-text object (text-ellipsis (selected-text row) window-width)
                          x y)
              (print-text object (text-ellipsis (normal-text row) window-width)
                          x y))))))

(defun init (root)
  "Initialize the window"
  (let* ((low-level-window  (make-croatoan-window :border t))
         (high-level-window (make-instance 'filesystem-tree-window
                                           :uses-border-p   t
                                           :title           (_ "File explorer")
                                           :key-config      swconf:+key-keybindings-window+
                                           :keybindings     *filesystem-explorer-keymap*
                                           :croatoan-window low-level-window
                                           :filesystem-root (make-root-tree root))))
    (refresh-config high-level-window)
    (setf *filesystem-explorer-window* high-level-window)
    (resync-rows-db high-level-window :redraw t :selected-path root)
    high-level-window))
