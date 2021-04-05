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
  ((support-text
     :initform nil
     :initarg  :support-text
     :reader   support-text)
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

(defmethod (setf support-text) (new-text (object message-window))
  (setf (slot-value object 'support-text) new-text)
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
    (when (rows object)
      (draw-buffer-line-mark object))
    (call-next-method)))

(defgeneric prepare-for-rendering (object &key (jump-to-first-row)))

(defgeneric append-support-text (object text
                                &key prepare-for-rendering jump-to-first-row))

(defgeneric scroll-down  (object &optional amount))

(defgeneric scroll-up    (object &optional amount))

(defgeneric scroll-end   (object))

(defgeneric scroll-begin (object))

(defgeneric scroll-next-page     (object))

(defgeneric scroll-previous-page (object))

(defgeneric search-regex (object regex))

(defgeneric text->rendered-lines-rows (window text))

(defmethod text->rendered-lines-rows (window (text gemini-parser:pre-start))
  (make-instance 'line
                 :normal-text (make-tui-string "")))

(defmethod text->rendered-lines-rows (window (text gemini-parser:pre-end))
  (make-instance 'line
                 :normal-text (make-tui-string "")))

(defmethod text->rendered-lines-rows (window (text list))
  (flatten (loop for i in text
                 collect
                 (text->rendered-lines-rows window i))))

(defmethod text->rendered-lines-rows (window (text complex-string))
  (make-instance 'line
                 :normal-text text))

(defun colorize-lines (lines)
    (let ((color-re (swconf:color-regexps)))
      (loop for line in lines
            collect
            (let ((res line))
              (loop for re in color-re do
                (setf res (colorize-line res re)))
              (colorized-line->tui-string res)))))

(defmethod text->rendered-lines-rows (window (text gemini-parser:quoted-lines))
  (let ((colorized-lines (colorize-lines (gemini-parser:lines text))))
    (loop for i in colorized-lines
          collect
          (make-instance 'line
                         :normal-text i))))

(defmethod text->rendered-lines-rows (window (text string))
  (labels ((fit-lines (lines)
             (let ((res ()))
               (loop for line in lines do
                 (if (string-empty-p line)
                     (push nil res)
                     (loop
                       for fitted-line
                         in (flush-left-mono-text (split-words line)
                                                  (win-width-no-border window))
                       do
                          (push fitted-line res))))
               (reverse res))))
    (if (string= text (format nil "~%"))
        (make-instance 'line
                       :normal-text (make-tui-string ""))
        (let* ((lines        (split-lines text))
               (fitted-lines (fit-lines lines))
               (new-rows     (colorize-lines fitted-lines)))
          (mapcar (lambda (text-line)
                    (make-instance 'line
                                   :normal-text text-line))
                  new-rows)))))

(defmethod text->rendered-lines-rows (window (text null))
  (make-instance 'line
                 :normal-text ""))

(defmethod prepare-for-rendering ((object message-window) &key (jump-to-first-row t))
  (with-accessors ((support-text support-text)) object
    (when hooks:*before-prepare-for-rendering-message*
      (hooks:run-hook 'hooks:*before-prepare-for-rendering-message* object))
    (setf (rows object)
          (text->rendered-lines-rows object support-text))
    (when jump-to-first-row
      (select-row object 0))
    object))

(defmethod append-support-text ((object message-window) text
                               &key
                                 (prepare-for-rendering nil)
                                 (jump-to-first-row     nil))
  (with-slots (support-text) object
    (setf support-text (strcat support-text text))
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
    (let ((complex (normal-text (elt rows row-selected-index))))
      (values (tui-string->chars-string complex)
              complex))))

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
        (multiple-value-bind (first-window-line-simple first-window-line-complex)
            (first-line->string object)
          (labels ((highlight (&optional (start-scan 0))
                     (multiple-value-bind (start end)
                         (scan regex first-window-line-simple :start start-scan)
                       (when start
                         (let* ((mask   (make-tui-string (subseq first-window-line-simple
                                                                 start end)
                                                         :fgcolor (win-bgcolor object)
                                                         :bgcolor (win-fgcolor object)))
                                (prefix (tui-string-subseq first-window-line-complex
                                                           0
                                                           start))
                                (new-prefix (cat-tui-string prefix mask)))
                           (print-text object new-prefix 1 1)
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

(defgeneric viewport-width (object))

(defmethod viewport-width ((object message-window))
  (windows:win-width-no-border object))
