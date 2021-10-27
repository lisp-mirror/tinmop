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

(define-constant +row-invisible-field-key+      :invisible      :test #'eq)

(define-constant +row-vertical-space-field-key+ :vertical-space :test #'eq)

(defclass message-window (wrapper-window
                          row-oriented-widget
                          focus-marked-window
                          title-window)
  ((line-position-mark
    :initform (make-tui-string "0")
    :initarg  :line-position-mark
    :accessor line-position-mark)
   (metadata
    :initform nil
    :initarg  :metadata
    :accessor metadata)
   (adjust-rows-strategy
    :initform #'adjust-rows-noop
    :initarg  :adjust-rows-strategy
    :accessor adjust-rows-strategy)
   (text-starting-column
    :initform 0
    :initarg  :text-starting-column
    :accessor text-starting-column)))

(defgeneric prepare-for-rendering (object text-data &key jump-to-first-row))

(defgeneric scroll-down  (object &optional amount))

(defgeneric scroll-up    (object &optional amount))

(defgeneric scroll-left  (object &optional amount))

(defgeneric scroll-right (object &optional amount))

(defgeneric scroll-end   (object))

(defgeneric scroll-begin (object))

(defgeneric scroll-next-page     (object))

(defgeneric scroll-previous-page (object))

(defgeneric search-regex (object regex))

(defgeneric jump-to-group-id (object gid-looking-for))

(defgeneric text->rendered-lines-rows (window text))

(defgeneric colorize-lines (object))

(defgeneric viewport-width (object))

(defgeneric generate-toc (object))

(defun gemini-window-p* (window)
  (gemini-viewer:gemini-metadata-p (message-window:metadata window)))

(defun gemini-window-p ()
  (gemini-window-p* specials:*message-window*))

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

(defun visible-rows (window)
  (with-accessors ((row-selected-index row-selected-index)) window
    (let* ((start  (max 0 row-selected-index))
           (end    (+ start
                      (win-height-no-border window)))
           (rows   (rows-safe-subseq window
                                     row-selected-index
                                     :end end)))
      (values rows
              start
              (length rows)))))

(defun draw-text (window)
  (when hooks:*before-rendering-message-text*
    (hooks:run-hook 'hooks:*before-rendering-message-text* window))
  (with-accessors ((row-selected-index   row-selected-index)
                   (text-starting-column text-starting-column)) window
    (let ((visible-rows                       (visible-rows window))
          (window-width                       (win-width-no-border window))
          (content-available-on-the-left-mark (swconf:left-arrow)))
      (when hooks:*before-rendering-message-visible-rows*
        (hooks:run-hook 'hooks:*before-rendering-message-visible-rows* visible-rows window))
      (loop for line in visible-rows
            for y from  1
            do
               (cond
                 ;; testing invisibility should  never returns true as
                 ;; the method `row'  is specialized on message-window
                 ;; and  always removes  from the  rows the  invisible
                 ;; ones.
                 ((row-invisible-p line)
                  (decf y))
                 ((not (row-vertical-space-p line))
                  (let* ((text-line         (remove-corrupting-utf8-chars (normal-text line)))
                         (text-length       (text-length text-line))
                         (truncate-at       (- window-width
                                               text-length
                                               text-starting-column))
                         (truncatep         (< truncate-at 0))
                         (actual-text-line  (cond
                                              ((= text-length 0)
                                               "")
                                              ((>= text-starting-column text-length)
                                               content-available-on-the-left-mark)
                                              (truncatep
                                               (tui-string-subseq text-line
                                                                  text-starting-column
                                                                  (min text-length
                                                                       (+ window-width
                                                                          text-starting-column))))
                                              (t
                                               (tui-string-subseq text-line
                                                                  text-starting-column
                                                                  nil)))))
                    (print-text window actual-text-line 1 y))))))))

(defun draw-buffer-line-mark (window)
  (with-accessors ((rows                 rows)
                   (row-selected-index   row-selected-index)
                   (line-position-mark line-position-mark)) window
    (let* ((height     (1- (win-height-no-border window)))
           (rows-count (- (rows-length window) height))
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
    (when (not (line-oriented-window:rows-empty-p object))
      (draw-buffer-line-mark object))
    (call-next-method)))

(let ((index     0)
      (frames   -1)
      (slowness 15))
  (declare (fixnum index frames slowness))
  (defun draw-downloading-animation (window)
    (declare (optimize (debug 0) (speed 2)))
    (let* ((animation-frames (swconf:gemini-downloading-animation))
           (max              (length (the list animation-frames))))
      (print-text window
                  (elt animation-frames index)
                  0 0
                  :attributes (attribute-bold))
      (incf frames)
      (when (= (rem frames slowness)
               0)
        (setf index (rem (1+ index) max))))))

(defgeneric row-add-original-object (lines original-object))

(defmethod row-add-original-object ((lines line) original-object)
  (push original-object
        (fields lines))
  (push :original-object
        (fields lines))
  lines)

(defmethod row-add-original-object ((lines list) original-object)
  (mapcar (lambda (a) (row-add-original-object a original-object))
          lines)
  lines)

(defun row-get-original-object (line)
  (getf (fields line) :original-object))

(defun row-add-group-id (line group-id)
  (push group-id
        (fields line))
  (push :group-id
        (fields line))
  line)

(defun row-get-group-id (line)
  (getf (fields line) :group-id))

(defun make-render-vspace-row (&optional (original-object
                                          (make-instance 'gemini-parser:vertical-space)))
  (let ((res (make-instance 'line
                 :normal-text (make-tui-string (format nil "~%"))
                 :fields      (list +row-vertical-space-field-key+ 1))))
    (row-add-original-object res original-object)
    (row-add-group-id res (gemini-parser:group-id original-object))
    res))

(defun row-vertical-space-p (row)
  (getf (fields row) +row-vertical-space-field-key+))

(defun make-invisible-row (original-object &optional (text ""))
  (let ((res (make-instance 'line
                            :fields      (list +row-invisible-field-key+ t)
                            :normal-text (if (typep text 'croatoan:complex-string)
                                             text
                                             (make-tui-string text)))))
    (row-add-original-object res original-object)
    res))

(defun row-pre-start-p (row)
  (typep (row-get-original-object row)
         'gemini-parser:pre-start))

(defun row-preformatted-p (row)
  (typep (row-get-original-object row)
         'gemini-parser:pre-line))

(defun row-invisible-p (row)
  (getf (fields row) +row-invisible-field-key+))

(defun row-visible-p (row)
  (not (row-invisible-p row)))

(defun row-set-invisible (row)
  (with-accessors ((fields fields)) row
    (when (not (row-invisible-p row))
      (push t fields)
      (push +row-invisible-field-key+ fields))
    row))

(defun row-set-visible (row)
  (setf (fields row) (remove-from-plist (fields row) +row-invisible-field-key+))
  row)

(defmacro with-map-update-raw-rows ((window function) &body body)
  (with-gensyms (new-rows)
    `(let ((,new-rows (rows-map-raw ,window
                                    ,function)))
       ,@body
       (update-all-rows ,window ,new-rows)
       (draw ,window))))

(defun row-hide-preformatted (message-window)
  (with-map-update-raw-rows (message-window
                             (lambda (a)
                               (when (row-preformatted-p a)
                                 (row-set-invisible a))
                               a))))

(defun row-show-pre-start (message-window)
  (with-map-update-raw-rows (message-window
                             (lambda (a)
                               (when (row-pre-start-p a)
                                 (row-set-visible a))
                               a))))

(defun row-show-preformatted (message-window)
  (with-map-update-raw-rows (message-window
                             (lambda (a)
                               (when (row-preformatted-p a)
                                 (row-set-visible a))
                               a))))

(defun row-hide-pre-start (message-window)
  (with-map-update-raw-rows (message-window
                             (lambda (a)
                               (when (row-pre-start-p a)
                                 (row-set-invisible a))
                               a))))

(let ((pre-visible-p t))

  (defun set-default-preformatted-visibility (visibility)
    (setf pre-visible-p visibility))

  (defun get-default-preformatted-visibility ()
    pre-visible-p)

  (defun toggle-default-preformatted-visibility ()
    (setf pre-visible-p (not pre-visible-p)))

  (defun toggle-preformatted-block (message-window)
    (if pre-visible-p
        (progn
          (row-hide-preformatted message-window)
          (row-show-pre-start    message-window))
        (progn
          (row-show-preformatted message-window)
          (row-hide-pre-start    message-window)))
    (toggle-default-preformatted-visibility)
    message-window))

(defmethod text->rendered-lines-rows (window (text gemini-parser:vertical-space))
  (make-render-vspace-row text))

(defmethod text->rendered-lines-rows (window (text gemini-parser:pre-start))
  (make-invisible-row text (gemini-parser:alt-text text)))

(defmethod text->rendered-lines-rows (window (text gemini-parser:pre-end))
  (make-invisible-row text))

(defmethod text->rendered-lines-rows (window (text gemini-parser:pre-line))
  (let ((res (make-instance 'line
                            :normal-text
                            (reduce #'tui:cat-complex-string
                                    (text->rendered-lines-rows window (gemini-parser:lines text)))
                            :fields      (list :alt-text        (gemini-parser:alt-text text)
                                               :group-id        (gemini-parser:group-id text)))))
    (row-add-original-object res text)
    res)) ; even if row-add-original-object returns the modified line explicit returns for clarity

(defmethod text->rendered-lines-rows (window (text list))
  (flatten (loop for i in text
                 collect
                 (text->rendered-lines-rows window i))))

(defmethod text->rendered-lines-rows (window (text complex-string))
  text)

(defgeneric collect-lines-from-ir (object window &key &allow-other-keys))

(defmethod collect-lines-from-ir ((object gemini-parser:with-lines) (window message-window)
                                  &key
                                    (width (win-width-no-border window))
                                    (empty-line-transform-fn (lambda () (make-render-vspace-row)))
                                  &allow-other-keys)
  (let ((colorized-lines (colorize-lines (%fit-lines window
                                                     (gemini-parser:lines object)
                                                     :width width
                                                     :empty-line-transform-fn
                                                     empty-line-transform-fn))))
    (loop for text in colorized-lines
          collect
          (let ((res-line (text->line text))
                (group-id (gemini-parser:group-id object)))
            (row-add-original-object res-line object)
            (row-add-group-id res-line group-id)
            res-line))))

(defmethod text->rendered-lines-rows (window (text gemini-parser:quoted-lines))
  (let* ((rows (collect-lines-from-ir text window
                                      :width (1- (win-width-no-border window))
                                      :empty-line-transform-fn (lambda () ""))))
    (loop for row in rows do
      (setf (normal-text row)
            (colorize-lines (strcat (gemini-parser::prefix text)
                                    (tui-string->chars-string (normal-text row))))))
    rows))

(defmethod text->rendered-lines-rows (window (text gemini-parser:header-line))
  (collect-lines-from-ir text window))

(defmethod text->rendered-lines-rows (window (text gemini-parser:unordered-list-line))
  (collect-lines-from-ir text window))

(defmethod text->rendered-lines-rows (window (text gemini-parser:link-line))
  (let ((res (make-instance 'line :normal-text (gemini-parser:link-text text))))
    (row-add-original-object res text)
    (row-add-group-id res (gemini-parser:group-id text))
    res)) ; even if row-add-original-object returns the modified line explicit returns for clarity

(defun %fit-text (window text)
  (let ((lines (split-lines text)))
    (%fit-lines window lines)))

(defun %fit-lines (window lines
                   &key
                     (width (win-width-no-border window))
                     (empty-line-transform-fn (lambda () (make-render-vspace-row))))
  (let ((res   ()))
    (loop for line in lines do
      (cond
        ((string-empty-p (trim-blanks line))
         (push (funcall empty-line-transform-fn) res))
        (t
         (loop for fitted-line in (flush-left-mono-text (split-words line) width)  do
           (push fitted-line res)))))
    (reverse res)))

(defgeneric text->line (object))

(defmethod text->line ((object line))
  object)

(defmethod text->line (object)
  (make-instance 'line
                 :normal-text object))

(defmethod text->rendered-lines-rows (window (text string))
  (let* ((fitted-lines (%fit-text window text))
         (new-rows     (colorize-lines fitted-lines)))
    (mapcar #'text->line new-rows)))

(defmethod text->rendered-lines-rows (window (text gemini-parser:simple-line))
  (let* ((fitted-lines (%fit-text window (gemini-parser:text-line text)))
         (new-rows     (colorize-lines fitted-lines)))
    (mapcar (lambda (text-line)
              (let ((res (text->line text-line)))
                (row-add-original-object res text)
                (row-add-group-id        res (gemini-parser:group-id text))))
            new-rows)))

(defun remove-invisible-rows (rows)
  (remove-if #'row-invisible-p rows))

(defmethod text->rendered-lines-rows (window (text line))
  text)

(defmethod rows ((object message-window))
   (with-slots (rows) object
     (remove-invisible-rows rows)))

(defmethod colorize-lines ((object line))
  object)

(defmethod colorize-lines ((object complex-string))
  (make-instance 'line :normal-text object))

(defmethod colorize-lines ((object string))
  (let ((color-re (swconf:color-regexps))
        (res object))
    (loop for re in color-re do
      (setf res (colorize-line res re)))
    (colorized-line->tui-string res)))

(defmethod colorize-lines ((object list))
  (loop for line in object
        collect
        (colorize-lines line)))

(defmethod viewport-width ((object message-window))
  (windows:win-width-no-border object))

(defmethod prepare-for-rendering ((object message-window) text-data &key (jump-to-first-row t))
  (update-all-rows object (text->rendered-lines-rows object text-data))
  (when jump-to-first-row
    (select-row object 0))
  object)

(defun offset-to-move-end (win)
  (with-accessors ((rows                 rows)
                   (row-selected-index   row-selected-index)) win
    (let ((win-height (win-height-no-border win)))
      (- (- (rows-length win)
            (- win-height 1))
         row-selected-index))))

(defun scroll-end-reached-p (win)
  (with-accessors ((rows                 rows)
                   (row-selected-index   row-selected-index)) win
    (let* ((win-height (win-height-no-border win))
           (rows-left  (- (rows-length win) row-selected-index)))
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

(defmethod scroll-left ((object message-window) &optional (amount 1))
  (with-accessors ((text-starting-column text-starting-column)) object
    (when (> text-starting-column 0)
      (decf text-starting-column amount)
      (draw object))))

(defmethod scroll-right  ((object message-window) &optional (amount 1))
  (with-accessors ((text-starting-column text-starting-column)) object
    (incf text-starting-column amount)
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
      (when (and (> (- (rows-length object)
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
  (with-accessors ((row-selected-index   row-selected-index)) window
    (let ((complex (normal-text (rows-elt window row-selected-index))))
      (values (tui-string->chars-string complex)
              complex))))

(defmethod search-regex ((object message-window) regex)
  (with-accessors ((row-selected-index   row-selected-index)) object
    (let* ((selected-row                 (selected-row object))
           (selected-text                (normal-text selected-row))
           (actual-row-starting          (if (scan regex
                                                   (tui-string->chars-string selected-text))
                                             (1+ row-selected-index)
                                             row-selected-index))
           (line-found (rows-position-if object
                                         (lambda (a)
                                           (scan regex
                                                 (tui-string->chars-string (normal-text a))))
                                         :start (clamp actual-row-starting
                                                       0
                                                       (rows-length object))))
           (replacements-strings ()))
      (when line-found
        (progn
          (row-move object (- line-found row-selected-index))
          (draw object)
          (multiple-value-bind (first-window-line-simple first-window-line-complex)
              (first-line->string object)
            (labels ((calc-highlight (&optional (start-scan 0))
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
                             (push new-prefix replacements-strings)
                             (calc-highlight end)))))
                     (highlight ()
                       (loop for replacement in replacements-strings do
                         (print-text object replacement 1 1))))
              (calc-highlight)
              (highlight))))))))

(defmethod jump-to-group-id ((object message-window) gid-looking-for)
  (when-let ((line-found (rows-position-if object
                                           (lambda (a)
                                             (when-let ((header (row-get-original-object a)))
                                               (when (typep header 'gemini-parser:header-line)
                                                 (let ((gid (gemini-parser:group-id header)))
                                                   (= gid gid-looking-for)))))
                                           :start 0)))
    (select-row object 0)
    (row-move object line-found)
    (draw object)))

(defmethod generate-gemini-toc ((object message-window))
  (let* ((toc-number    (make-list gemini-parser:+max-header-level+ :initial-element 0))
         (current-gid   -1)
         (all-headers   (remove-if-not (lambda (a)
                                         (typep (row-get-original-object a)
                                                'gemini-parser:header-line))
                                       (rows object)))
         (toc          (loop for row in all-headers
                             collect
                             (let* ((header (row-get-original-object row))
                                    (level  (gemini-parser:level header))
                                    (gid    (gemini-parser:group-id header)))
                               (when (/= gid current-gid)
                                 (setf current-gid gid)
                                 (incf (elt toc-number (1- level)))
                                 (loop for i from level below (length toc-number) do
                                   (setf (elt toc-number i) 0))
                                 (loop for i from (- level 2 ) downto 0
                                       when (= (elt toc-number i) 0) do
                                         (setf (elt toc-number i) 1))
                                 (list :header (first (gemini-parser:lines header))
                                       :group-id gid
                                       :number   (subseq toc-number
                                                         0
                                                         level)))))))
    (remove-if #'null toc)))

(defun gemini-toc-header (fields)
  (getf fields :header))

(defun gemini-toc-max-number-length (toc)
  (* 2
     (num:find-max (mapcar (lambda (a) (length (getf a :number)))
                           toc))))

(defun gemini-toc-number (fields toc)
  (let ((raw               (mapcar #'to-s (getf fields :number)))
        (max-number-length (gemini-toc-max-number-length toc)))
    (right-padding (join-with-strings raw ".")
                   max-number-length
                   :padding-char (swconf:gemini-toc-padding-char))))

(defun gemini-toc-entry (fields toc)
  (format nil "~a ~a" (gemini-toc-number fields toc) (gemini-toc-header fields)))

(defun gemini-toc-group-id (fields)
  (getf fields :group-id))

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
