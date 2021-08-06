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

(in-package :command-window)

(defclass command-window (wrapper-window point-tracker)
  ((command-line
    :initform ()
    :initarg  :command-line
    :accessor command-line
    :documentation "A list of keys so far inserted by the user, or the input string, depending on the mode.")
   (echo-character
    :initform nil
    :initarg  :echo-character
    :accessor echo-character
    :documentation "If  non nil  print a number  of copies  (equals to
  length  of  slot  'command-line'  of  this  string  instead  of  the
  command-line itself")
   (error-message
    :initform nil
    :initarg  :error-message
    :accessor error-message
    :documentation "Error message to be printed")
   (error-message-bg
    :initform nil
    :initarg  :error-message-bg
    :accessor error-message-bg
    :documentation "Error message background color")
   (error-message-fg
    :initform nil
    :initarg  :error-message-fg
    :accessor error-message-fg
    :documentation "Error message foreground color")
   (error-message-attributes
    :initform nil
    :initarg  :error-message-attributes
    :accessor error-message-attributes
    :documentation "Error message attributes (bold, blink etc.)")
   (info-message
    :initform nil
    :initarg  :info-message
    :accessor info-message
    :documentation "Information message to be printed")
   (info-message-bg
    :initform nil
    :initarg  :info-message-bg
    :accessor info-message-bg
    :documentation "Info message background color")
   (info-message-fg
    :initform nil
    :initarg  :info-message-fg
    :accessor info-message-fg
    :documentation "Info message foreground color")
   (info-message-attributes
    :initform nil
    :initarg  :info-message-attributes
    :accessor info-message-attributes
    :documentation "Info message attributes (bold, blink etc.)")
   (commands-separator
    :initform " "
    :initarg  :commands-separator
    :accessor commands-separator
    :documentation "The text printed to separates each key in command")
   (suggestions-win
    :initform nil
    :initarg  :suggestions-win
    :accessor suggestions-win
    :documentation  "The windows  that  print  contect stuccesions  to
    user (e.g. autocomplete path")
   (history-position
    :initarg  :history-position
    :accessor history-position
    :documentation "Current positions in the history of commands")
   (event-to-answer
    :initform nil
    :initarg  :event-to-answer
    :accessor event-to-answer
    :documentation "This is the event that was triggered by a function
    that instruct  the command window to  ask user for an  input. This
    event  is inpected  to  get the  prompt and,  after  the input  is
    complete, a  slot is  setted with such  input, then  another event
    `user-input-string-event' is generated to  notify (via a condition
    variable)  the thread  that generated  `event-to-answer' the  fact
    that the input is complete.")
   (input-mode
    :initform :keybinding
    :initarg  :input-mode
    :accessor input-mode
    :documentation "The mode of accepting  input for this window.  Can
be either `:keybinding' or `:string'.  the former for key command the latter for free input (e.g filepath, username, etc")))

(defmethod initialize-instance :after ((object command-window) &key &allow-other-keys)
  (with-accessors ((command-line       command-line)
                   (commands-separator commands-separator)
                   (error-message      error-message)
                   (history-position   history-position)
                   (prompt             prompt)
                   (suggestions-win    suggestions-win)) object
    ;; poor man cache...
    (setf specials:*keybindings-suggestions-window* (keybindings-window:init))
    (setf specials:*strings-suggestions-window*     (complete-window:init))
    (set-keybinding-mode object)
    object))

(defun set-history-most-recent (window prompt)
  (with-accessors ((command-line       command-line)
                   (history-position   history-position)) window
    (setf history-position
          (1+ (db:most-recent-history-id prompt)))))

(defmethod refresh-config :after ((object command-window))
  (with-accessors ((error-message-bg         error-message-bg)
                   (error-message-fg         error-message-fg)
                   (error-message-attributes error-message-attributes)
                   (info-message-bg          info-message-bg)
                   (info-message-fg          info-message-fg)
                   (info-message-attributes  info-message-attributes)) object
    (let* ((w (win-width *main-window*))
           (h +command-window-height+)
           (x 0)
           (y (1- (win-height *main-window*))))
      (refresh-config-colors object swconf:+key-command-window+)
      (multiple-value-bind (bg fg value)
          (swconf:command-separator-config-values)
        (multiple-value-bind (error-bg error-fg error-attributes)
            (swconf:command-error-message-colors)
          (multiple-value-bind (info-bg info-fg info-attributes)
              (swconf:command-info-message-colors)
          (setf error-message-bg error-bg)
          (setf error-message-fg error-fg)
          (setf error-message-attributes error-attributes)
          (setf info-message-bg info-bg)
          (setf info-message-fg info-fg)
          (setf info-message-attributes info-attributes)
          (setf (point-fg object) (win-bgcolor object))
          (setf (point-bg object) (win-fgcolor object))
          (setf (commands-separator object)
                (make-tui-string value
                                 :fgcolor fg
                                 :bgcolor bg))
          (win-resize object w h)
          (win-move   object x y)
          object))))))

(defmethod calculate ((object command-window) dt)
  (with-accessors ((suggestions-win suggestions-win)) object
    (when suggestions-win
      (calculate suggestions-win dt))))

(defun draw-string-mode (win)
  "Draw window `win' accepting strings"
  (with-accessors ((command-line   command-line)
                   (point-position point-position)
                   (point-bg       point-bg)
                   (point-fg       point-fg)
                   (prompt         prompt)
                   (echo-character echo-character)) win
    (flet ((print-echo-character ()
             (let ((echoed (with-output-to-string (stream)
                             (loop repeat (length command-line) do
                                  (princ echo-character stream)))))
               (print-text win echoed (length prompt) 0))))
      (let* ((length-cmd-line     (length command-line))
             (no-prompt-point-pos (no-prompt-point-pos win))
             (cursor-value    (if (and (> length-cmd-line 0)
                                       (< no-prompt-point-pos
                                          length-cmd-line))
                                  (elt command-line no-prompt-point-pos)
                                  #\Space)))
        (print-text win prompt 0 0)
        (when command-line
          (if echo-character
              (print-echo-character)
              (print-text win command-line (length prompt) 0)))
        (print-text win
                    cursor-value
                    point-position
                    0
                    :fgcolor point-fg
                    :bgcolor point-bg)))))

(defmethod draw ((object command-window))
  (with-accessors ((command-line             command-line)
                   (commands-separator       commands-separator)
                   (error-message-bg         error-message-bg)
                   (error-message-fg         error-message-fg)
                   (error-message-attributes error-message-attributes)
                   (error-message            error-message)
                   (info-message-bg          info-message-bg)
                   (info-message-fg          info-message-fg)
                   (info-message             info-message)
                   (info-message-attributes  info-message-attributes)
                   (suggestions-win          suggestions-win)) object
    (when suggestions-win
      (draw suggestions-win))
    (win-clear object :redraw nil)
    (cond
      (error-message
       (print-text object error-message 0 0
                   :bgcolor    error-message-bg
                   :fgcolor    error-message-fg
                   :attributes error-message-attributes))
      (info-message
       (print-text object info-message 0 0
                   :bgcolor    info-message-bg
                   :fgcolor    info-message-fg
                   :attributes info-message-attributes))
      (t
        (if (keybindings-mode-p object)
            (when command-line
              (let ((advance 0))
                (loop for (command . rest) on command-line while rest do
                     (print-text object command advance 0)
                     (incf advance (length command))
                     (print-text object commands-separator advance 0)
                     (incf advance (text-length commands-separator)))
                (print-text object (last-elt command-line) advance 0)))
            (draw-string-mode object))))
    (win-refresh object)))

(defgeneric enqueue-command (object command decode-key))

(defgeneric complete-at-point (object))

(defgeneric show-candidate-completion (object))

(defgeneric add-error-message (object message))

(defgeneric add-info-message (object message))

(defgeneric remove-messages (object))

(defun manage-command-event (command-window event)
  "Intercept UI events in keybinding mode"
  (with-accessors ((command-line    command-line)
                   (suggestions-win suggestions-win)) command-window
    ;; some envents should by intercepted by command window
    (cond
      ((eq :control-left event) ; suggestion win pagination
       (move-suggestion-page-left command-window))
      ((eq :control-right event) ; suggestion win pagination
       (move-suggestion-page-right command-window))
      ((eq :backspace event) ; delete last command or char
       (setf command-line (safe-all-but-last-elt command-line))
       (when-let ((last-command (safe-last-elt command-line)))
         (setf command-line (safe-all-but-last-elt command-line))
         (enqueue-command command-window last-command nil)))
      (t
       (enqueue-command command-window event t)))))

(defun update-suggestions (window key-decoded)
  "Update suggestion window"
  (with-accessors ((command-line    command-line)
                   (suggestions-win suggestions-win)) window
    ;; if command-line is  not null we are in the middle of a command
    ;; so no need to update the slot of suggestion-win with a new tree
    (if command-line
        (suggestions-window:update-suggestions suggestions-win
                                               key-decoded
                                               :tree nil)
        (let* ((focused-keybindings (main-window:focused-keybindings specials:*main-window*))
               (found-subtree (and focused-keybindings
                                   (suggestions-window:update-suggestions suggestions-win
                                                                          key-decoded
                                                                          :tree
                                                                          focused-keybindings))))
          (or found-subtree
              (suggestions-window:update-suggestions suggestions-win
                                                     key-decoded
                                                     :tree *global-keymap*))))))

(defmethod enqueue-command ((object command-window) command decode-key-p)
  "Enqueue and  process, if possible `command` object, if decode-key
  is not null decode key to something more human readable."
  (with-accessors ((command-line    command-line)
                   (info-message    info-message)
                   (error-message   error-message)
                   (suggestions-win suggestions-win)) object
    (when (null suggestions-win)
      (setf suggestions-win (keybindings-window:init)))
    (win-show suggestions-win)
    (let* ((key-decoded   (if decode-key-p
                              (decode-key-event command)
                              command))
           (found-subtree (update-suggestions object key-decoded)))
      (remove-messages object)
      (cond
        ((null found-subtree)
         (let ((missing-command (format nil "~s" (lcat command-line
                                                       (list key-decoded)))))
           (restart-case
               (error 'conditions:command-not-found
                      :command missing-command)
             (print-error (e)
               (declare (ignore e))
               (win-hide suggestions-win)
               (setf suggestions-win nil)
               (setf command-line nil)
               (setf error-message
                     (format nil
                             (_ "Error: command ~a not found")
                             missing-command))))))
        ((functionp found-subtree)
         (win-hide suggestions-win)
         (setf suggestions-win nil)
         (setf command-line nil)
         (funcall found-subtree))
        (t
         (setf command-line (reverse command-line))
         (push key-decoded  command-line)
         (setf command-line (reverse command-line))))))
  object)

(defmethod complete-at-point ((object command-window))
  "Complete input at point (string mode only)"
  (with-accessors ((command-line    command-line)
                   (suggestions-win suggestions-win)) object
    (when (null suggestions-win)
      (setf suggestions-win (complete-window:init)))
    (win-show suggestions-win)
    (multiple-value-bind (candidates common-prefix)
        (suggestions-window:update-suggestions suggestions-win
                                               command-line)
      (if candidates
          (if (null common-prefix)
              (progn
                (insert-selected-suggestion object)
                (suggestions-window:update-suggestions suggestions-win
                                                       command-line)
                (reset-selected-suggestion-index object)
                (setf (suggestions-window:current-page suggestions-win) 0))
              (progn
                (when (length= candidates 1)
                  (win-hide suggestions-win))
                (insert-selected-suggestion object)))
          (win-hide suggestions-win))))
  object)

(defmethod show-candidate-completion ((object command-window))
  (with-accessors ((command-line    command-line)
                   (suggestions-win suggestions-win)) object
    (when (null suggestions-win)
      (setf suggestions-win (complete-window:init)))
    (let ((candidates (suggestions-window:update-suggestions suggestions-win
                                                             command-line)))
      (if candidates
          (win-show suggestions-win)
          (win-hide suggestions-win)))))

(defmethod add-error-message ((object command-window) message)
  (setf (error-message object) message)
  (draw object))

(defmethod add-info-message ((object command-window) message)
  (setf (info-message object) message)
  (draw object))

(defmethod remove-messages ((object command-window))
  "Remove info and error messages that this window holds"
  (setf (info-message object) nil)
  (setf (error-message object) nil))

(defun move-suggestion-page (win offset)
  "Paginate win (suggestion window) by offset, will not go past the number of pages."
  (with-accessors ((suggestions-win suggestions-win)) win
    (when suggestions-win
      (with-accessors ((current-page   suggestions-window:current-page)
                       (paginated-info suggestions-window:paginated-info)) suggestions-win

        (setf current-page (clamp (+ offset current-page)
                                  0
                                  (1- (length paginated-info))))))))

(defun move-suggestion-page-left (win)
  (move-suggestion-page win -1))

(defun move-suggestion-page-right (win)
  (move-suggestion-page win 1))

(defun select-suggestion (win offset)
  "Paginate win (suggestion window) by offset, will not go past the number of pages."
  (with-accessors ((suggestions-win suggestions-win)) win
    (when suggestions-win
      (with-accessors ((current-page   suggestions-window:current-page)
                       (paginated-info suggestions-window:paginated-info)
                       (selected-item-row-index    complete-window:selected-item-row-index)
                       (selected-item-column-index complete-window:selected-item-column-index))
          suggestions-win
        (incf selected-item-row-index offset)
        (let* ((columns       (elt paginated-info current-page))
               (columns-count (length columns))
               (column        (elt columns selected-item-column-index))
               (rows-count    (length column)))
          (cond
            ((< selected-item-row-index 0)
             (decf selected-item-column-index)
             (when (< selected-item-column-index 0)
               (setf selected-item-column-index
                     (1- (length columns))))
             (let* ((previous-column      (elt columns selected-item-column-index))
                    (previous-column-size (length previous-column)))
               (setf selected-item-row-index (1- previous-column-size))))
            ((>= selected-item-row-index rows-count)
             (setf selected-item-row-index complete-window:+starting-item-index+)
             (setf selected-item-column-index
                   (+ complete-window:+starting-item-index+
                      (rem (1+ selected-item-column-index) columns-count))))))))))

(defun select-suggestion-next (win)
  (select-suggestion win 1))

(defun select-suggestion-previous (win)
  (select-suggestion win -1))

(defun suggested-selection (win)
  (with-accessors ((suggestions-win suggestions-win)) win
    (when suggestions-win
      (with-accessors ((current-page   suggestions-window:current-page)
                       (paginated-info suggestions-window:paginated-info)
                       (selected-item-row-index    complete-window::selected-item-row-index)
                       (selected-item-column-index complete-window::selected-item-column-index))
          suggestions-win
        (when-let* ((columns     (elt paginated-info current-page))
                    (column      (elt columns selected-item-column-index))
                    (suggestion  (trim-blanks (elt column  selected-item-row-index))))
          suggestion)))))

(defun reset-selected-suggestion-index (win)
  (with-accessors ((suggestions-win suggestions-win)) win
    (complete-window:reset-selected-item suggestions-win))
  win)

(defun insert-selected-suggestion (win)
  (with-accessors ((suggestions-win suggestions-win)
                   (command-line    command-line)) win
    (when suggestions-win
      (let ((suggestion (suggested-selection win)))
        (setf command-line suggestion)
        (move-point-to-end win command-line))))
  win)

(defun fire-user-input-event (win)
  "Generates an event to notify that  the user inserted an input on the
command line."
  (with-accessors ((event-to-answer event-to-answer)
                   (command-line    command-line))   win
    (assert event-to-answer)
    (assert (typep event-to-answer
                   'program-events:program-event))
    (let ((input-done-event (make-instance 'program-events:user-input-string-event
                                           :payload
                                           (program-events:payload event-to-answer)
                                           :lock
                                           (program-events:lock event-to-answer)
                                           :condition-variable
                                           (program-events:condition-variable event-to-answer))))
      (setf (box:dunbox (program-events:payload input-done-event))
            command-line)
      (program-events:push-event input-done-event))))

(defun manage-string-event (command-window event)
  "Manage UI events when `command-window` is in string mode"
  (with-accessors ((command-line     command-line)
                   (error-message    error-message)
                   (info-message     info-message)
                   (prompt           prompt)
                   (history-position history-position)
                   (suggestions-win  suggestions-win)
                   (point-position   point-position)) command-window
    (flet ((set-history (new-id new-input)
             (when (and new-id
                        new-input)
               (setf history-position new-id)
               (setf command-line new-input)))
           (insert-in-history (prompt command-line)
             (db:insert-in-history prompt command-line)
             (set-history-most-recent command-window prompt)))
      (remove-messages command-window)
      (cond
        ((string= (decode-key-event event) "^K")
         (setf command-line (safe-subseq command-line 0 (no-prompt-point-pos command-window))))
        ((eq :alt-left event)
         (move-suggestion-page-left command-window))
        ((eq :alt-right event)
         (move-suggestion-page-right command-window))
        ((eq :backspace event)
         (setf command-line (delete-at-point command-window command-line :direction :left))
         (show-candidate-completion command-window))
        ((eq :dc event)
         (setf command-line (delete-at-point command-window command-line :direction :right))
         (show-candidate-completion command-window))
        ((eq :left event)
         (move-point-left command-window))
        ((eq :right event)
         (move-point-right command-window (length command-line)))
        ((eq :end event)
         (move-point-to-end command-window command-line))
        ((eq :home event)
         (move-point-to-start command-window))
        ((eq :up event)
         (if (win-shown-p suggestions-win)
             (select-suggestion-previous command-window)
             (multiple-value-bind (new-id new-input)
                 (db:previous-in-history history-position prompt)
               (set-history new-id new-input))))
        ((eq :down event)
         (if (win-shown-p suggestions-win)
             (select-suggestion-next command-window)
             (multiple-value-bind (new-id new-input)
                 (db:next-in-history history-position prompt)
               (set-history new-id new-input))))
        ((characterp event)
         (cond
           ((char= #\Newline event)
            (insert-in-history prompt command-line)
            (fire-user-input-event command-window)
            (setf command-line nil)
            (move-point-to-start command-window)
            (set-keybinding-mode command-window))
           ((char= #\Tab event)
            (complete-at-point command-window))
           (t
            (if (null suggestions-win)
                (setf suggestions-win (complete-window:init))
                (complete-window:reset-selected-item suggestions-win))
            (win-show suggestions-win)
            (setf command-line
                  (insert-at-point command-window event command-line))
            (when 'hooks:*after-char-to-command-window*
              (hooks:run-hook 'hooks:*after-char-to-command-window*
                              command-window))
            (show-candidate-completion command-window)))))))
  command-window)

(defun set-input-mode (win mode suggestions-cached-win)
  "Set win (command window) mode: keybindings or string mode"
  (assert (member mode '(:keybinding :string)))
  (with-accessors ((suggestions-win suggestions-win)
                   (input-mode      input-mode)) win
    (setf input-mode mode)
    (when suggestions-win
      (win-hide suggestions-win))
    (setf suggestions-win suggestions-cached-win)))

(defmacro gen-set-mode-function (fn-name mode suggestions-cached-win)
  `(defun ,(format-fn-symbol t "set-~a-mode" fn-name) (win)
     (set-input-mode win ,mode ,suggestions-cached-win)))

(gen-set-mode-function keybinding :keybinding  specials:*keybindings-suggestions-window*)

(gen-set-mode-function string     :string      specials:*strings-suggestions-window*)

(defun keybindings-mode-p (win)
  "Non nil if win is in keybings mode"
  (eq (input-mode win)
      :keybinding))

(defun manage-event (event)
  "Manage UI event,  these are not program events but  events fired by
the curses library (croatoan)"
  (if (keybindings-mode-p   *command-window*)
      (manage-command-event *command-window* event)
      (manage-string-event  *command-window* event))
  (draw *command-window*))

(defun init ()
  "Initialize the window"
  (with-croatoan-window (croatoan-main-window *main-window*)
    (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
      (setf *command-window*
            (make-instance 'command-window
                           :croatoan-window low-level-window))
      (refresh-config *command-window*)
      *command-window*)))
