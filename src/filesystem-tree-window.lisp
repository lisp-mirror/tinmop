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
     (list :path path :dirp dirp :markedp nil))
    (dirp
     (if (scan "/$" path)
         (list :path path :dirp t :markedp nil)
         (list :path (strcat path "/") :dirp t :markedp nil)))))

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
    :type      function
    :documentation "A function with the node as parameter. Will modify
    the argument appending its children")
   (filesystem-rename-function
    :initform  #'rename-local-filesystem-node
    :accessor  filesystem-rename-function
    :type      function
    :documentation "A function with two parameters: a node and the new
    name for the path of the matching node")
   (filesystem-delete-function
    :initform  #'delete-local-filesystem-node
    :accessor  filesystem-delete-function
    :type      function
    :documentation "A function with the node as parameter.")
   (filesystem-create-function
    :initform  #'create-local-filesystem-node
    :accessor  filesystem-create-function
    :type      function
    :documentation "A function  with two parameter the  path to create
    and a boolean thah values true if a directory must be created")
   (filesystem-download-function
    :initform  #'download-local-filesystem-node
    :accessor  filesystem-download-function
    :type      function
    :documentation "A  function to download a  remote file, parameters
    are:

    - node (remote file)
    - destination-file (local file, note that
      this   should    be   an   optional   parameter    with   default:
      (make-temporary-file-from-node node).

    Must returns the path of the downloaded file.")
   (filesystem-upload-function
    :initform  #'upload-local-filesystem-node
    :accessor  filesystem-upload-function
    :type      function
    :documentation "A  function to upload a  local file, parameters:
     - source-path  (local path)
     - matching-node (remote directory).")
   (filesystem-query-path-function
    :initform  #'query-local-filesystem-path
    :accessor  filesystem-query-path-function
    :type      function
    :documentation "function with two parameter the path and a feature
    to query Valid  feature values are :size.  Returns  nil if Returns
    nil if the path does not point to an actual file.")
   (filesystem-close-connection-function
    :initform  (constantly t)
    :accessor  filesystem-close-connection-function
    :type      function
    :documentation "function with no parameter to close the connection."))
   (:documentation "A  window that shows  and allow interacting  with a
   hierarchical filesystem"))

(defmethod initialize-instance :after ((object filesystem-tree-window)
                                       &key (handlers-plist nil) &allow-other-keys)
  (when handlers-plist
    (setf (filesystem-expand-function object)
          (getf handlers-plist :filesystem-expand-function)
          (filesystem-expand-function object)
          (getf handlers-plist :filesystem-expand-function)
          (filesystem-rename-function object)
          (getf handlers-plist :filesystem-rename-function)
          (filesystem-delete-function object)
          (getf handlers-plist :filesystem-delete-function)
          (filesystem-create-function object)
          (getf handlers-plist :filesystem-create-function)
          (filesystem-download-function object)
          (getf handlers-plist :filesystem-download-function)
          (filesystem-upload-function object)
          (getf handlers-plist :filesystem-upload-function)
          (filesystem-query-path-function object)
          (getf handlers-plist :filesystem-query-path-function)
          (filesystem-close-connection-function object)
          (getf handlers-plist :filesystem-close-connection-function)))
  object)

(defmethod refresh-config :after ((object filesystem-tree-window))
  (with-croatoan-window (croatoan-window object)
    (refresh-config-colors object swconf:+key-file-explorer+)
    (refresh-config-sizes  object swconf:+key-file-explorer+)
    (when (not command-line:*gemini-full-screen-mode*)
      (let ((height  (- (win-height *main-window*)
                        (win-height *message-window*)))
            (width   (win-width *main-window*)))
        (resize croatoan-window height width)))
    (win-move object 0 0)))

(defmethod calculate :after ((object filesystem-tree-window) dt)
  (declare (ignore object dt)))

(defmacro gen-tree-data-fetcher (name key)
  (let ((fn-name (misc:format-fn-symbol t "tree-~a" name)))
    `(progn
       (defun ,fn-name (data)
         (getf data ,key))
       (defsetf ,fn-name (data) (val)
         `(setf (getf ,data ,,key) ,val)))))

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

(defun query-local-filesystem-path (path what)
  (case what
    (:size
     (and (fs:file-exists-p path)
          (fs:file-size     path)))
    (otherwise
     (_ "not implemented"))))

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

(define-constant +octect-type+     '(unsigned-byte 8) :test #'equalp)

(defun make-temporary-file-from-path (path)
  (let ((extension (fs:get-extension path)))
    (fs:temporary-file :extension extension)))

(defun make-temporary-file-from-node (node)
  (let ((path (tree-path (data node))))
    (make-temporary-file-from-path path)))

(define-constant +download-buffer+ (expt 2 24) :test #'=)

(defun download-local-filesystem-node (matching-node
                                       &optional
                                         (destination-file
                                          (make-temporary-file-from-node matching-node)))
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

(defun upload-local-filesystem-node (source-path destination-path)
  (with-open-file (input-stream source-path
                                :direction    :input
                                :element-type +octect-type+)
    (with-open-file (output-stream destination-path
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
                   (return-from write-loop t)))))))

(defun %expand-treenode (root path-to-expand expand-fn)
  (when-let ((matching-node (find-node root path-to-expand)))
    (funcall expand-fn matching-node)))

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
    (let ((parent-path (uri:normalize-path path)))
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
                          &optional
                            (destination-file (make-temporary-file-from-path remote-path)))
  (when-let* ((root-node     (filesystem-root window))
              (matching-node (find-node root-node remote-path)))
    (funcall (filesystem-download-function window) matching-node destination-file)))

(defun upload-treenode (window source-file remote-path)
  (when-let* ((root-node     (filesystem-root window))
              (parent-node   (find-node root-node (fs:parent-dir-path remote-path)))
              (parent-path   (tree-path (data parent-node))))
    (funcall (filesystem-upload-function window) source-file remote-path)
    (remove-all-children parent-node)
    (expand-treenode window parent-path)
    (win-clear window :redraw nil)
    (resync-rows-db window :redraw t :selected-path remote-path)))

(defun recursive-delete-node (window path)
  (with-accessors ((root-node                  filesystem-root)
                   (filesystem-expand-function filesystem-expand-function)) window
    (let* ((matching-node (find-node root-node path))
           (filep         (not (tree-dir-p (data matching-node)))))
      (if filep
          (delete-treenode window path)
          (when (not (or (fs:loopback-reference-dir-p path)
                         (fs:backreference-dir-p      path)))
            (%expand-treenode root-node
                              (tree-path (data matching-node))
                              filesystem-expand-function)
            (setf matching-node (find-node root-node path))
            (do-children (child matching-node)
              (let ((path-to-recurse (tree-path (data child))))
                (recursive-delete-node window path-to-recurse)))
            (delete-treenode window path))))))

(defun filesystem-query-treenode (window path what)
  (assert (member what '(:size :size-string :permissions :permissions-string)))
  (when-let* ((root-node     (filesystem-root window))
              (matching-node (find-node root-node path)))
    (funcall (filesystem-query-path-function window)
             (tree-path (data matching-node))
             what)))

(defmethod search-row ((object filesystem-tree-window) regex &key (redraw t))
  (handler-case
      (with-accessors ((row-selected-index row-selected-index)) object
        (when-let* ((scanner         (create-scanner regex :case-insensitive-mode nil))
                    (position-header (rows-position-if object
                                                       (lambda (a)
                                                         (scan scanner (tree-path (fields a))))
                                                       :start (1+ row-selected-index))))
          (resync-rows-db object :redraw nil)
          (unselect-all object)
          (select-row object position-header)
          (when redraw
            (win-clear object :redraw nil)
            (draw object))))
    (error ()
      (ui:error-message (_ "Invalid regular expression")))))

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
          (cond
            ((selectedp row)
             (let ((text (tui:copy-tui-string (selected-text row))))
               (print-text object (text-ellipsis text window-width) x y)))
            ((tree-marked-p (fields row))
             (let ((text (tui:copy-tui-string (normal-text row))))
               (print-text object
                           (tui:apply-attributes (text-ellipsis text window-width)
                                                 :all
                                                 (tui:combine-attributes (tui:attribute-reverse)
                                                                         (tui:attribute-bold)))
                           x y)))
            (t
             (let ((text (tui:copy-tui-string (normal-text row))))
               (print-text object (text-ellipsis text window-width) x y))))))))

(defun mark-node (window path &key (toggle t))
  (when-let* ((root-node     (filesystem-root window))
              (matching-node (find-node root-node path)))
    (if toggle
        (setf (tree-marked-p (data matching-node))
              (not (tree-marked-p (data matching-node))))
        (setf (tree-marked-p (data matching-node)) t))))

(defun open-node (window path)
  (when-let* ((root-node     (filesystem-root window))
              (matching-node (find-node root-node path))
              (node-data     (data matching-node))
              (node-path     (tree-path node-data)))
    (if (tree-dir-p node-data)
        (expand-treenode window node-path)
        (let ((downloaded-path (download-treenode window node-path)))
          (os-utils:open-resource-with-external-program downloaded-path nil)))))

(defun edit-node (window path)
  (when-let* ((root-node     (filesystem-root window))
              (matching-node (find-node root-node path))
              (node-data     (data matching-node))
              (node-path     (tree-path node-data)))
    (if (tree-dir-p node-data)
        (expand-treenode window node-path)
        (let ((downloaded-path (download-treenode window node-path)))
          (croatoan:end-screen)
          (os-utils:open-resource-with-external-program downloaded-path nil :open-for-edit t)
          (upload-treenode window
                           downloaded-path
                           node-path)))))

(defun close-connection (window)
  (funcall (filesystem-close-connection-function window)))

(defun init (root &optional (handlers-plist nil))
  "Initialize the window"
  (let* ((low-level-window  (make-croatoan-window :border t))
         (high-level-window (make-instance 'filesystem-tree-window
                                           :uses-border-p   t
                                           :title           (_ "File explorer")
                                           :key-config      swconf:+key-keybindings-window+
                                           :keybindings     *filesystem-explorer-keymap*
                                           :croatoan-window low-level-window
                                           :filesystem-root (make-root-tree root)
                                           :handlers-plist  handlers-plist)))
    (refresh-config high-level-window)
    (setf *filesystem-explorer-window* high-level-window)
    (resync-rows-db high-level-window :redraw t :selected-path root)
    high-level-window))
