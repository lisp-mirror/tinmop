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

(in-package :open-message-link-window)

(defclass open-message-link-window (open-attach-window:open-attach-window) ())

(defmethod refresh-config :after ((object open-message-link-window))
  (open-attach-window:refresh-view-links-window-config object
                                                       swconf:+key-open-message-link-window+))

(defmethod resync-rows-db ((object open-message-link-window) &key
                                                               (redraw t)
                                                               (suggested-message-index nil))
  (with-accessors ((rows             rows)
                   (status-id        open-attach-window:status-id)
                   (selected-line-bg selected-line-bg)
                   (selected-line-fg selected-line-fg)) object
    (flet ((make-rows (links bg fg)
             (mapcar (lambda (link)
                       (make-instance 'line
                                      :normal-text   link
                                      :selected-text link
                                      :normal-bg     bg
                                      :normal-fg     fg
                                      :selected-bg   fg
                                      :selected-fg   bg))
                     links)))
      (let* ((message (db:find-status-id status-id))
             (links   (reverse (text-utils:collect-links (db:row-message-rendered-text message)))))
        (with-croatoan-window (croatoan-window object)
          (when hooks:*before-displaying-links-hook*
            (setf links
                  (hooks:run-hook-compose 'hooks:*before-displaying-links-hook* links)))
          (line-oriented-window:update-all-rows object
                                                (make-rows links
                                                           selected-line-bg
                                                           selected-line-fg))
          (when suggested-message-index
            (select-row object suggested-message-index))
          (when redraw
            (draw object)))))))

(defun init (status-id)
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *open-message-link-window*
          (make-instance 'open-message-link-window
                         :title             (_ "Links")
                         :status-id         status-id
                         :single-row-height 1
                         :uses-border-p     t
                         :keybindings       keybindings:*open-message-link-keymap*
                         :croatoan-window   low-level-window))
    (refresh-config *open-message-link-window*)
    (resync-rows-db *open-message-link-window* :redraw nil)
    (when (not (line-oriented-window:rows-empty-p *open-message-link-window*))
      (select-row *open-message-link-window* 0))
    (draw *open-message-link-window*)
    *open-message-link-window*))

(defun open-message-link (url enqueue)
  (let* ((parsed       (iri:iri-parse url))
         (scheme       (uri:scheme parsed))
         (decoded-path (percent-decode url)))
    (when (and (not enqueue)
               (swconf:close-link-window-after-select-p))
      (ui:close-open-message-link-window))
    (cond
      ((string= gemini-constants:+gemini-scheme+ scheme)
       (db:insert-in-history (ui:gemini-open-url-prompt) url)
       (db:gemlog-mark-as-seen url)
       (gemini-viewer:ensure-just-one-stream-rendering)
       (gemini-viewer:load-gemini-url url
                                      :give-focus-to-message-window nil
                                      :enqueue                      enqueue
                                      :use-cached-file-if-exists    t))
      ((fs:dirp decoded-path)
       (gemini-viewer:load-gemini-url decoded-path :give-focus-to-message-window nil))
      (t
       (os-utils:open-resource-with-external-program decoded-path nil)))))

(defclass open-links-window ()
  ((links
    :initform ()
    :initarg  :links
    :accessor links)))

(defclass open-gemini-document-link-window (focus-marked-window
                                            simple-line-navigation-window
                                            title-window
                                            border-window
                                            open-links-window)
  ())

(defmethod refresh-config :after ((object open-gemini-document-link-window))
  (open-attach-window:refresh-view-links-window-config object
                                                       swconf:+key-open-message-link-window+))

(defmethod resync-rows-db ((object open-gemini-document-link-window)
                           &key
                             (redraw t)
                             (suggested-message-index nil))
  (with-accessors ((rows             rows)
                   (links            links)
                   (selected-line-bg selected-line-bg)
                   (selected-line-fg selected-line-fg)) object
    (when hooks:*before-displaying-links-hook*
      (let ((mapped-links (hooks:run-hook-compose 'hooks:*before-displaying-links-hook*
                                                  (mapcar #'gemini-parser:target links))))
        (loop for mapped-link in mapped-links
              for link in links
              do
                 (setf (gemini-parser:target link) mapped-link))))
    (flet ((make-rows (links bg fg)
             (mapcar (lambda (link)
                       (make-instance 'line
                                      :normal-text   (gemini-parser:target link)
                                      :selected-text (gemini-parser:target link)
                                      :normal-bg     bg
                                      :normal-fg     fg
                                      :selected-bg   fg
                                      :selected-fg   bg))
                     links)))
      (with-croatoan-window (croatoan-window object)
        (line-oriented-window:update-all-rows object
                                              (make-rows links
                                                         selected-line-bg
                                                         selected-line-fg))
        (when suggested-message-index
          (select-row object suggested-message-index))
        (when redraw
          (draw object))))))

(defmethod draw :before ((object open-gemini-document-link-window))
  (with-accessors ((links             links)
                   (uses-border-p     uses-border-p)
                   (single-row-height single-row-height)
                   (top-row-padding   top-row-padding)
                   (new-messages-mark new-messages-mark)
                   (top-rows-slice    top-rows-slice)
                   (bottom-rows-slice bottom-rows-slice)) object
    (let ((y-start (if uses-border-p
                       1
                       0)))
      (renderizable-rows-data object) ; set top and bottom slice
      (win-clear object)
      (with-croatoan-window (croatoan-window object)
        (loop
           for link in (safe-subseq links top-rows-slice bottom-rows-slice)
           for y from (+ y-start top-row-padding) by single-row-height
           for index from top-rows-slice
           do
             (print-text object
                         (format nil "[~a] ~a" index (gemini-parser:name link))
                         1 y
                         :bgcolor (bgcolor croatoan-window)
                         :fgcolor (fgcolor croatoan-window)))))))

(defmethod search-row ((object open-gemini-document-link-window) regex &key (redraw t))
  (handler-case
      (with-accessors ((row-selected-index row-selected-index)
                       (links              links)) object
        (let* ((scanner             (create-scanner regex :case-insensitive-mode t))
               (selected-link       (elt links row-selected-index))
               (selected-text       (gemini-parser:name selected-link))
               (actual-row-starting (if (scan scanner selected-text)
                                        (1+ row-selected-index)
                                        row-selected-index))
               (position-header     (position-if (lambda (a)
                                                 (scan scanner
                                                       (gemini-parser:name a)))
                                                 (safe-subseq (links object)
                                                              actual-row-starting))))
          (call-next-method)    ; search in urls
          (when position-header ; but if an header has been found, it wins
            (unselect-all object)
            (select-row object (+ actual-row-starting position-header))
            (when redraw
              (draw object)))))
    (error ()
      (ui:error-message (_ "Invalid regular expression")))))

(defun init-gemini-links (links)
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *open-message-link-window*
          (make-instance 'open-gemini-document-link-window
                         :top-row-padding        0
                         :top-horizontal-padding 1
                         :title                  (_ "Links")
                         :links                  links
                         :single-row-height      2
                         :uses-border-p          t
                         :keybindings            keybindings:*open-message-link-keymap*
                         :croatoan-window        low-level-window))
    (refresh-config *open-message-link-window*)
    (resync-rows-db *open-message-link-window* :redraw nil)
    (when (not (line-oriented-window:rows-empty-p *open-message-link-window*))
      (select-row *open-message-link-window* 0))
    (draw *open-message-link-window*)
    *open-message-link-window*))

(defun forget-gemini-link-window ()
  (setf (keybindings *message-window*)
        keybindings:*message-keymap*))

(defclass open-chat-document-link-window (focus-marked-window
                                          simple-line-navigation-window
                                          title-window
                                          border-window
                                          open-links-window)
  ())

(defmethod refresh-config :after ((object open-chat-document-link-window))
  (open-attach-window:refresh-view-links-window-config object
                                                       swconf:+key-open-message-link-window+))

(defmethod resync-rows-db ((object open-chat-document-link-window)
                           &key
                             (redraw t)
                             (suggested-message-index nil))
  (with-accessors ((rows             rows)
                   (links            links)
                   (selected-line-bg selected-line-bg)
                   (selected-line-fg selected-line-fg)) object
    (when hooks:*before-displaying-links-hook*
      (setf links
            (hooks:run-hook-compose 'hooks:*before-displaying-links-hook* links)))
    (flet ((make-rows (links bg fg)
             (mapcar (lambda (link)
                       (make-instance 'line
                                      :normal-text   link
                                      :selected-text link
                                      :normal-bg     bg
                                      :normal-fg     fg
                                      :selected-bg   fg
                                      :selected-fg   bg))
                     links)))
      (with-croatoan-window (croatoan-window object)
        (line-oriented-window:update-all-rows object
                                              (make-rows links
                                                         selected-line-bg
                                                         selected-line-fg))
        (when suggested-message-index
          (select-row object suggested-message-index))
        (when redraw
          (draw object))))))

(defun init-chat-links (links)
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *open-message-link-window*
          (make-instance 'open-chat-document-link-window
                         :top-row-padding        0
                         :title                  (_ "Chat attachments")
                         :links                  links
                         :single-row-height      1
                         :uses-border-p          t
                         :keybindings            keybindings:*open-message-link-keymap*
                         :croatoan-window        low-level-window))
    (refresh-config *open-message-link-window*)
    (resync-rows-db *open-message-link-window* :redraw nil)
    (when (not (line-oriented-window:rows-empty-p *open-message-link-window*))
      (select-row *open-message-link-window* 0))
    (draw *open-message-link-window*)
    *open-message-link-window*))

(defun forget-chat-link-window ()
  (setf (keybindings *message-window*)
        keybindings:*message-keymap*))
