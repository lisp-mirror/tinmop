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

(in-package :message-window)

(defclass message-window (wrapper-window
                          row-oriented-widget
                          focus-marked-window
                          title-window)
  ((source-text
    :initform nil
    :initarg  :source-text
    :reader   source-text)
   (line-position-mark
    :initform (make-tui-string "0")
    :initarg  :line-position-mark
    :accessor line-position-mark)
   (metadata
    :initform nil
    :initarg  :metadata
    :accessor metadata)))

(defun gemini-window-p ()
  (gemini-viewer:gemini-metadata-p (message-window:metadata specials:*message-window*)))

(defun display-gemini-text-p (window)
  (eq (keybindings window)
      keybindings:*gemini-message-keymap*))

(defun display-chat-p (window)
  (eq (keybindings window)
      keybindings:*chat-message-keymap*))

(defun prepare-for-display-status-mode (window)
  (when (not (or (display-gemini-text-p window)
                 (display-chat-p        window)))
    (setf (keybindings window)
          keybindings:*message-keymap*)))

(defmethod (setf source-text) (new-text (object message-window))
  (setf (slot-value object 'source-text) new-text)
  (handler-bind ((conditions:out-of-bounds
                  (lambda (e)
                    (invoke-restart 'ignore-selecting-action e))))
    (prepare-for-rendering object)))

(defun refresh-line-mark-config (window)
  (multiple-value-bind (mark-value mark-fg mark-bg)
      (swconf:message-window-line-mark-values)
    (setf (line-position-mark window)
          (make-tui-string mark-value
                           :fgcolor mark-fg
                           :bgcolor mark-bg))))

(defmethod refresh-config :after ((object message-window))
  (refresh-config-colors object swconf:+key-message-window+)
  (refresh-line-mark-config object)
  (let* ((thread-window-width   (win-width  *thread-window*))
         (thread-window-height  (win-height *thread-window*))
         (command-window-height (win-height *command-window*))
         (main-window-height    (win-height *main-window*))
         (height                (- main-window-height
                                   command-window-height
                                   thread-window-height))
         (width                 thread-window-width)
         (x                     (win-x *thread-window*))
         (y                     (+ (win-y *thread-window*)
                                   thread-window-height)))
    (win-resize object width height)
    (win-move   object x y)))

(defmethod calculate ((object message-window) dt)
  (declare (ignore object dt)))

(defun draw-text (window)
  (with-accessors ((rows               rows)
                   (row-selected-index row-selected-index)) window
    (let ((actual-rows (safe-subseq rows row-selected-index)))
      (loop
         for line in actual-rows
         for y from  1  below (win-height-no-border window) do
           (let ((text-line (normal-text line)))
           (when (string-not-empty-p text-line)
             (print-text window text-line 1 y)))))))

(defun draw-buffer-line-mark (window)
  (with-accessors ((rows                 rows)
                   (row-selected-index   row-selected-index)
                   (line-position-mark line-position-mark)) window
    (let* ((height     (1- (win-height-no-border window)))
           (rows-count (- (length rows) height))
           (fraction   (/ row-selected-index
                          (max 1 rows-count)))
           (mark-y   (1+ (truncate (* fraction height))))
           (mark-x   (1- (win-width window))))
      (print-text window line-position-mark mark-x mark-y))))

(defmethod draw ((object message-window))
  (when-window-shown (object)
    (win-clear object :redraw nil)
    (win-box object)
    (draw-text object)
    (when (source-text object)
      (draw-buffer-line-mark object))
    (call-next-method)))

(defgeneric prepare-for-rendering (object &key (jump-to-first-row)))

(defgeneric append-source-text (object text
                                &key prepare-for-rendering jump-to-first-row))

(defgeneric scroll-down  (object &optional amount))

(defgeneric scroll-up    (object &optional amount))

(defgeneric scroll-end   (object))

(defgeneric scroll-begin (object))

(defgeneric scroll-next-page     (object))

(defgeneric scroll-previous-page (object))

(defgeneric search-regex (object regex))

(defmethod prepare-for-rendering ((object message-window) &key (jump-to-first-row t))
  (flet ((fit-lines (lines)
           (let ((res ()))
             (loop for line in lines do
                  (if (string-empty-p line)
                      (push nil res)
                      (loop
                         for fitted-line in
                           (flush-left-mono-text (split-words line)
                                                 (win-width-no-border object))
                         do
                           (push fitted-line res))))
             (reverse res))))
    (with-accessors ((source-text source-text)) object
      (when hooks:*before-prepare-for-rendering-message*
        (hooks:run-hook 'hooks:*before-prepare-for-rendering-message* object))
      (let* ((lines        (split-lines source-text))
             (fitted-lines (fit-lines lines))
             (color-re     (swconf:color-regexps))
             (new-rows     (loop for line in fitted-lines collect
                                (let ((res line))
                                  (loop for re in color-re do
                                       (setf res (colorize-line res re)))
                                  (colorized-line->tui-string res)))))
        (setf (rows object)
              (mapcar (lambda (text-line)
                        (make-instance 'line
                                       :normal-text text-line))
                      new-rows))
        (when jump-to-first-row
          (select-row object 0))
        object))))

(defmethod append-source-text ((object message-window) text
                               &key
                                 (prepare-for-rendering nil)
                                 (jump-to-first-row     nil))
  (with-slots (source-text) object
    (setf source-text (strcat source-text text))
    (when prepare-for-rendering
      (prepare-for-rendering object :jump-to-first-row jump-to-first-row))))

(defun offset-to-move-end (win)
  (with-accessors ((rows                 rows)
                   (row-selected-index   row-selected-index)) win
    (let ((win-height (win-height-no-border win)))
      (- (- (length rows)
            (- win-height 1))
         row-selected-index))))

(defun scroll-end-reached-p (win)
  (with-accessors ((rows                 rows)
                   (row-selected-index   row-selected-index)) win
    (let* ((win-height (win-height-no-border win))
           (rows-left  (- (length rows) row-selected-index)))
      (< rows-left
         win-height))))

(defmethod scroll-down ((object message-window) &optional (amount 1))
    (when (not (or (scroll-end-reached-p object)
                   (= (row-move object amount)
                      0)))
    (draw object)))

(defmethod scroll-up   ((object message-window) &optional (amount 1))
  (when (/= (row-move object (- amount))
            0)
    (draw object)))

(defmethod scroll-end ((object message-window))
  (with-accessors ((rows                 rows)
                   (row-selected-index   row-selected-index)) object
    (let ((offset (offset-to-move-end object)))
      (when (/= (row-move object offset)
                0)
        (draw object)))))

(defmethod scroll-begin  ((object message-window))
  (with-accessors ((rows                 rows)
                   (row-selected-index   row-selected-index)) object
    (when (/= (row-move object (- row-selected-index))
              0)
      (draw object))))

(defmethod scroll-next-page ((object message-window))
  (with-accessors ((rows                 rows)
                   (row-selected-index   row-selected-index)) object
    (let ((actual-window-height (win-height-no-border object)))
      (when (and (> (- (length rows)
                  row-selected-index)
                    actual-window-height)
                 (/= (row-move object actual-window-height)
                     0))
          (draw object)))))

(defmethod scroll-previous-page ((object message-window))
  (when (/= (row-move object (- (win-height-no-border object)))
            0)
    (draw object)))

(defun first-line->string (window)
  (with-accessors ((rows                 rows)
                   (row-selected-index   row-selected-index)) window
    (tui-string->chars-string (normal-text (elt rows row-selected-index)))))

(defmethod search-regex ((object message-window) regex)
  (with-accessors ((rows                 rows)
                   (row-selected-index   row-selected-index)) object
    (let ((line-found (position-if (lambda (a)
                                     (scan regex
                                           (tui-string->chars-string (normal-text a))))
                                   rows
                                   :start (min (1+ row-selected-index)
                                               (length rows)))))
      (when line-found
        (row-move object (- line-found row-selected-index))
        (draw object)
        (let ((line (first-line->string object)))
          (labels ((highlight (&optional (start-scan 0))
                     (multiple-value-bind (start end)
                         (scan regex line :start start-scan)
                       (when start
                         (let ((mask (make-tui-string (subseq line start end)
                                                      :fgcolor (win-bgcolor object)
                                                      :bgcolor (win-fgcolor object))))
                           (print-text object mask (1+ start) 1)
                           (highlight end))))))
            (highlight)))))))

(defun init ()
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *message-window*
          (make-instance 'message-window
                         :title           (_ "Messages")
                         :keybindings     keybindings:*message-keymap*
                         :key-config      swconf:+key-message-window+
                         :croatoan-window low-level-window))
    (refresh-config *message-window*)
    (draw *message-window*)
    *message-window*))
