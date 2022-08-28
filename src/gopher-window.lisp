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

(in-package :gopher-window)

(defclass gopher-window (wrapper-window
                         focus-marked-window
                         title-window
                         border-window
                         row-oriented-widget)
  ((page-type
    :initarg :page-type
    :initform nil
    :accessor page-type
    :documentation "The type of the page , 0,1,2,i etc.")))

(defmethod refresh-config :after ((object gopher-window))
  (refresh-config-colors object swconf:+key-gopher-window+)
  (let* ((height (win-height *message-window*))
         (width  (win-width *message-window*))
         (x      (win-x *message-window*))
         (y      (win-y *message-window*)))
    (win-resize object width height)
    (win-move   object x y)))

(defmethod draw :after ((object gopher-window))
  (with-accessors ((uses-border-p uses-border-p)) object
    (when-window-shown (object)
      (let ((max-line-size (if uses-border-p
                               (win-width-no-border object)
                               (win-width           object))))
        (let ((rows    (renderizable-rows-data object))
              (x       (if (uses-border-p object)
                           1
                           0))
              (y-start (if (uses-border-p object)
                           1
                           0)))
          (loop
            for y from y-start
            for ct from 0
            for row in rows do
              (let ((tui-text (if (selectedp row)
                                  (tui:apply-attributes (selected-text row)
                                                 :all
                                                 (tui:combine-attributes (tui:attribute-reverse)
                                                                         (tui:attribute-bold)))
                                  (normal-text row))))
                (print-text object (right-pad-text (text-ellipsis tui-text max-line-size)
                                                   max-line-size)
                            x y)))
          (when (> (rows-length object) 0)
            (let* ((current-selected (1+ (row-selected-index object)))
                   (pages-count-line (format nil
                                             (_ "line ~a of ~a")
                                             current-selected
                                             (rows-length object)))
                   (x-count-line     (- (win-width object)
                                        (length pages-count-line)
                                        1))
                   (y-count-line     (1- (win-height object))))
              (print-text object
                          (text-ellipsis pages-count-line (win-width-no-border object))
                          x-count-line
                          y-count-line))))))))

(defgeneric gopher-line->text (line))

(defun %gemline->text-simple (line prefix)
  (let* ((prefix-color     (swconf:config-gopher-line-prefix-foreground))
         (prefix-attribute (swconf:config-gopher-line-prefix-attribute))
         (colorized        (message-window::colorize-lines (gopher-parser:username line)))
         (colorized-prefix (tui:make-tui-string prefix
                                                :attributes prefix-attribute
                                                :fgcolor    prefix-color)))
    (tui:cat-tui-string colorized-prefix
                        (tui:apply-attributes colorized :all (tui:attribute-bold))
                        :color-attributes-contagion nil)))

(defmethod gopher-line->text ((line gopher-parser:line-file))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-text-file)))

(defmethod gopher-line->text ((line gopher-parser:line-dir))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-directory)))

(defmethod gopher-line->text ((line gopher-parser:line-cso))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-unknown)))

(defmethod gopher-line->text ((line gopher-parser:line-error))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-unknown)))

(defmethod gopher-line->text ((line gopher-parser:line-mac-hex-file))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-binary-file)))

(defmethod gopher-line->text ((line gopher-parser:line-dos-archive-file))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-binary-file)))

(defmethod gopher-line->text ((line gopher-parser:line-uuencoded-file))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-binary-file)))

(defmethod gopher-line->text ((line gopher-parser:line-index-search))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-search-index)))

(defmethod gopher-line->text ((line gopher-parser:line-telnet-session))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-unknown)))

(defmethod gopher-line->text ((line gopher-parser:line-binary-file))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-binary-file)))

(defmethod gopher-line->text ((line gopher-parser:line-redundant-server))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-directory)))

(defmethod gopher-line->text ((line gopher-parser:line-tn3270-session))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-unknown)))

(defmethod gopher-line->text ((line gopher-parser:line-gif-file))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-gif-file)))

(defmethod gopher-line->text ((line gopher-parser:line-image-file))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-image-file)))

(defmethod gopher-line->text ((line gopher-parser:line-info))
  (message-window::colorize-lines (gopher-parser:username line)))

(defmethod gopher-line->text ((line gopher-parser:line-uri))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-uri)))

(defmethod gopher-line->text ((line gopher-parser:line-unknown))
  (%gemline->text-simple line (swconf:config-gopher-line-prefix-unknown)))

(defun print-response-rows (window gopher-lines)
  (flet ((make-rows (lines)
           (mapcar (lambda (line)
                     (make-instance 'line
                                    :fields        (list :original-object line)
                                    :normal-text   (gopher-line->text line)
                                    :selected-text (gopher-line->text line)))
                   lines)))
    (line-oriented-window:update-all-rows window (make-rows gopher-lines))))

(defun init ()
  (maybe-close-window *gopher-window*)
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *gopher-window*
          (make-instance 'gopher-window
                         :uses-border-p   t
                         :title           (_ "Gopher menu")
                         :keybindings     keybindings:*gopher-keymap*
                         :key-config      swconf:+key-gopher-window+
                         :croatoan-window low-level-window))
    (refresh-config *gopher-window*)
    (draw *gopher-window*)
    *gopher-window*))

(defun not-link-line-p (line)
  (let ((original-object (message-window:extract-original-object line)))
    (not (or (gopher-parser:line-type-info-p original-object)
             (gopher-parser:line-type-error-p original-object)))))

(defun go-to-next-link ()
  (a:when-let* ((win                *gopher-window*)
                (1+selected-row-pos (1+ (line-oriented-window:row-selected-index win)))
                (link-line-pos      (rows-position-if win
                                                      #'not-link-line-p
                                                      :start 1+selected-row-pos)))
    (line-oriented-window:unselect-all win)
    (line-oriented-window:row-move win (- link-line-pos (1- 1+selected-row-pos)))
    (win-clear win)
    (windows:draw win)))

(defun go-to-previous-link ()
  (a:when-let* ((win              *gopher-window*)
                (selected-row-pos (line-oriented-window:row-selected-index win))
                (link-line-pos    (rows-position-if win
                                                    #'not-link-line-p
                                                    :from-end t
                                                    :end    selected-row-pos)))
    (line-oriented-window:unselect-all win)
    (line-oriented-window:row-move win (- link-line-pos selected-row-pos))
    (win-clear win)
    (windows:draw win)))

(defun make-request (host port type selector)
  (let ((message-win     specials:*message-window*))
    (gemini-viewer:maybe-initialize-metadata message-win)
    (let ((link (format nil "~a://~a:~a/~a/~a"
                        gopher-parser:+gopher-scheme+
                        host
                        port
                        type
                        selector)))
      (gemini-viewer:push-url-to-history message-win link)))
  (cond
    ((gopher-parser::%line-type-dir-p type)
     (let ((data (misc:make-fresh-array 0 :type '(unsigned-int 8))))
       (gopher-client:request host
                              type
                              :port       port
                              :selector   selector
                              :collect-fn (gopher-client:make-collect-fn data))
       (init)
       (ui:focus-to-gopher-window)
       (print-response-rows *gopher-window*
                            (gopher-parser:parse-menu (text-utils:to-s data)))
       (select-row *gopher-window* 0)
       (draw *gopher-window*)))
    ((gopher-parser::%line-type-file-p type)
     (win-close *gopher-window*)
     (let ((data (misc:make-fresh-array 0 :type '(unsigned-int 8))))
       (gopher-client:request host
                              type
                              :port       port
                              :selector   selector
                              :collect-fn (gopher-client:make-collect-fn data))
       (let* ((text      (to-s data))
              (raw-lines (split-lines (gopher-parser:parse-text-file text)))
              (lines     (mapcar (lambda (a)
                                   (message-window:text->rendered-lines-rows *message-window*
                                                                             a))
                                 raw-lines)))
         (line-oriented-window:update-all-rows *message-window* (a:flatten lines))
         (draw *message-window*)
         (ui:focus-to-message-window))))
    (t
     (fs:with-anaphoric-temp-file (stream)
       (gopher-client:request host
                              type
                              :port       port
                              :selector   selector
                              :collect-fn (lambda (buffer)
                                            (write-sequence buffer stream)))
       (finish-output stream)
       (os-utils:open-resource-with-external-program filesystem-utils:temp-file
                                                     nil)))))

(defun open-menu-link ()
  (a:when-let* ((win          *gopher-window*)
                (selected-row (selected-row win))
                (line         (message-window:extract-original-object selected-row)))
    (with-accessors ((line-type-id gopher-parser:line-type-id)
                     (selector     gopher-parser:selector)
                     (host         gopher-parser:host)
                     (port         gopher-parser:port)) line
      (make-request host port line-type-id selector))))
