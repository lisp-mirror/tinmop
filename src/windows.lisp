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

(in-package :windows)

(defclass key-config-holder ()
  ((key-config
    :initform nil
    :initarg :key-config
    :accessor key-config)))

(defclass tree-holder ()
  ((tree-color-map
    :initform ()
    :initarg  :tree-color-map
    :accessor tree-color-map)
   (render-arrow-value
    :initform ">"
    :initarg  :render-arrow-value
    :accessor render-arrow-value)
   (render-leaf-value
    :initform "-"
    :initarg  :render-leaf-value
    :accessor render-leaf-value)
   (render-branch-value
    :initform "+"
    :initarg  :render-branch-value
    :accessor render-branch-value)
   (render-spacer-value
    :initform "-"
    :initarg  :render-spacer-value
    :accessor render-spacer-value)
   (render-vertical-line-value
    :initform "|"
    :initarg  :render-vertical-line-value
    :accessor render-vertical-line-value)))

(defun refresh-config-color-map (window config-win-key)
  (with-accessors ((tree-color-map tree-color-map)) window
    (setf tree-color-map
          (swconf:make-tree-colormap config-win-key))))

(defun refresh-config-tree-rendering-values (window config-win-key)
  (with-accessors ((render-arrow-value         render-arrow-value)
                   (render-leaf-value          render-leaf-value)
                   (render-branch-value        render-branch-value)
                   (render-spacer-value        render-spacer-value)
                   (render-vertical-line-value render-vertical-line-value)) window
    (multiple-value-bind (arrow leaf branch spacer vertical-line)
        (swconf:tree-config-rendering-values config-win-key)
      (setf render-arrow-value         arrow
            render-leaf-value          leaf
            render-branch-value        branch
            render-spacer-value        spacer
            render-vertical-line-value vertical-line))))

(defmethod refresh-config :after ((object tree-holder))
  (with-accessors ((key-config key-config)) object
    (refresh-config-color-map object key-config)
    (refresh-config-tree-rendering-values object key-config)))

(defparameter *window-stack* (make-instance 'stack))

(defclass wrapper-window (m-tree key-config-holder)
  ((croatoan-window
    :initform nil
    :initarg  :croatoan-window
    :accessor croatoan-window
    :documentation "The lowlevel (ncurses) window")
   (keybindings
    :initform nil
    :initarg  :keybindings
    :accessor keybindings
    :documentation "The keymap associated to this window"))
  (:documentation "This is the parent of all the windows in this program"))

(defmethod initialize-instance :after ((object wrapper-window) &key &allow-other-keys)
  (stack-push *window-stack* object)
  (win-show object))

(defmethod print-object ((object wrapper-window) stream)
  (print-unreadable-object (object stream :type t :identity nil)))

(defmacro with-croatoan-window ((slot window) &body body)
  `(with-accessors ((,slot croatoan-window)) ,window
     ,@body))

(defmacro when-window-shown ((window &key (min-valid-height 2) (min-valid-width 2)) &body body)
  (with-gensyms (height width)
    `(when ,window
       (let ((,height (if (window-uses-border-p ,window)
                          (win-height-no-border ,window)
                          (win-height           ,window)))
             (,width  (if (window-uses-border-p ,window)
                          (win-width-no-border  ,window)
                          (win-width            ,window))))
         (when (and (win-shown-p ,window)
                    (> ,height ,min-valid-height)
                    (> ,width  ,min-valid-width))
           ,@body)))))

(defun win-clear (window &key (redraw t))
  "Clear window content"
  (clear (croatoan-window window) :target :window :redraw redraw))

(defmacro gen-simple-win->croatoan-specialized-wrapper (fn-name &optional (prefix nil))
  "Generate micro  wrapper for simple curses  library function (window
height, position and so on)"
  (with-gensyms (window inner)
    `(defun ,(format-fn-symbol t "~@[~a-~]~a" prefix fn-name) (,window)
       (with-croatoan-window (,inner ,window)
         (,fn-name ,inner)))))

(gen-simple-win->croatoan-specialized-wrapper width   win)

(gen-simple-win->croatoan-specialized-wrapper height  win)

(gen-simple-win->croatoan-specialized-wrapper box     win)

(gen-simple-win->croatoan-specialized-wrapper bgcolor win)

(gen-simple-win->croatoan-specialized-wrapper fgcolor win)

(gen-simple-win->croatoan-specialized-wrapper refresh win)

(gen-simple-win->croatoan-specialized-wrapper touch  win)

(defun menu-select (window)
  (with-croatoan-window (croatoan-window window)
    (prog1
        (select croatoan-window)
      (win-close window))))

(defun win-visible-p (win)
  (with-croatoan-window (croatoan-window win)
    (visiblep croatoan-window)))

(defun win-close (window)
  (with-croatoan-window (croatoan-window window)
    (stack-remove-element *window-stack* window)
    (close croatoan-window)))

(defun win-raise-to-top (window)
  (stack-raise-to-top *window-stack* window))

(defun win-width-no-border (win)
  (- (win-width win)
     2))

(defun win-height-no-border (win)
  (- (win-height win)
     2))

(defun win-x (win)
  (with-croatoan-window (inner-window win)
    (second (widget-position inner-window))))

(defun win-y (win)
  (with-croatoan-window (inner-window win)
    (first (widget-position inner-window))))

(defmacro with-window-width ((win w) &body body)
  `(let ((,w (win-width  ,win)))
     ,@body))

(defmacro with-window-height ((win h) &body body)
  `(let ((,h (win-height  ,win)))
     ,@body))

(defmacro with-window-sizes ((win w h) &body body)
  `(with-window-width (,win ,w)
     (with-window-height (,win ,h)
     ,@body)))

(defun calc-center-on-window-width (win message)
  (with-window-width (win w)
    (let ((win-center     (truncate (/ w 2)))
          (message-center (truncate (/ (length message) 2))))
      (- win-center message-center))))

(defun calc-bottom-of-window-height (win &key (has-border-p nil))
  (with-window-height (win h)
    (if has-border-p
        (- h 2)
        (- h 1))))

(defun win-move-cursor (window x y &key relative)
  "Wrapper of croatoan:move-window"
  (with-croatoan-window (inner window)
    (move inner y x :relative relative)))

(defun win-move-cursor-direction (window direction &optional (n 1))
  "Wrapper for croatoan:move-direction"
  (with-croatoan-window (inner window)
    (move-direction inner direction n)))

(defun win-move (window x y &key relative)
  "Wrapper for croatoan:move-window"
  (with-croatoan-window (inner window)
    (move-window inner y x :relative relative)))

(defun win-resize (window width height)
  "Wrapper for croatoan:resize"
  (with-croatoan-window (inner window)
    (resize inner height width)))

(defun win-show (window)
  "Show a window (must be stacked, see croatoan)"
  (with-croatoan-window (inner window)
    (setf (visiblep inner) t)))

(defun win-hide (window)
  "Hide a window (must be stacked, see croatoan)"
  (with-croatoan-window (inner window)
    (setf (visiblep inner) nil)))

(defun win-shown-p (window)
  "Show a window (must be stacked, see croatoan)"
  (with-croatoan-window (inner window)
    (visiblep inner)))

(defun win-set-background (window bg)
  "Set window background
   - window an instance of 'wrapper-window';
   - bg the returns value of 'tui-utils:make-win-background'"
  (with-croatoan-window (inner window)
    (setf (background inner) bg)))

(defgeneric print-text (object text x y &key &allow-other-keys)
  (:documentation "Print text on object (usually a window)"))

(defgeneric refresh-config (object)
  (:documentation "This  function will reload the configuration (from
  files) and reshape/redraw `object' accordly"))

(defgeneric calculate (object dt)
  (:documentation "Do something as dt time passed"))

(defgeneric draw (object)
  (:documentation "Draw object"))

(defmethod refresh-config (object)
  object)

(defmethod refresh-config ((object null))
  object)

(defmethod  print-text ((object wrapper-window) (text string) x y
                        &key
                          (attributes nil)
                          (fgcolor    nil)
                          (bgcolor    nil)
                          &allow-other-keys)
  (print-text object
              (make-tui-string text
                               :attributes attributes
                               :fgcolor    fgcolor
                               :bgcolor    bgcolor)
              x y))

(defmethod print-text ((object wrapper-window) (text complex-string) x y
                       &key &allow-other-keys)
  (add (croatoan-window object) text :x x :y y))

(defmethod print-text ((object wrapper-window) (text character) x y
                       &key
                         (attributes nil)
                         (fgcolor    nil)
                         (bgcolor    nil)
                         &allow-other-keys)
  (add (croatoan-window object)
       (string text)
       :x          x
       :y          y
       :attributes attributes
       :bgcolor    bgcolor
       :fgcolor    fgcolor))

(defmethod print-text ((object wrapper-window) (text list) x y &key &allow-other-keys)
  (loop
     for block in text
     with current-x = x do
       (add (croatoan-window object)
            block
            :x current-x
            :y y)
       (incf current-x (text-length block)))
  object)

(defmethod print-text ((object wrapper-window) text x y &key &allow-other-keys)
  (print-text object (to-s text) x y))

(defmethod calculate (object dt)
  (declare (ignore object dt))
  t)

(defmethod calculate ((object null) dt)
  (declare (ignore object dt))
  t)

(defmethod draw (object)
  (declare (ignore object))
  t)

(defmethod draw ((object null))
  (declare (ignore object))
  t)

(defun calculate-all (dt)
  (do-stack-element (window *window-stack*)
    (when (win-visible-p window)
      (win-touch window)
      (mark-for-refresh (croatoan-window window)))
    (calculate window dt))
  (refresh-marked))

(defun draw-all ()
  (do-stack-element (window *window-stack*)
    (when (win-visible-p window)
      (win-clear window)
      (draw window))))

(defun refresh-config-all ()
  (refresh-config *main-window*)
  (refresh-config *thread-window*)
  (refresh-config *message-window*)
  (refresh-config *tags-window*)
  (refresh-config *conversations-window*)
  (refresh-config *command-window*)
  (refresh-config *send-message-window*)
  (refresh-config *chats-list-window*)
  (refresh-config *gemini-toc-window*))

(defun cursor-show ()
  (setf (cursor-visible-p (croatoan-window *main-window*)) t))

(defun cursor-hide ()
  (setf (cursor-visible-p (croatoan-window *main-window*)) nil))

(defun refresh-config-colors (window conf-key)
  (let ((bg (swconf:win-bg conf-key))
        (fg (swconf:win-fg conf-key)))
    (with-croatoan-window (croatoan-window window)
      (setf (background croatoan-window)
            (tui:make-win-background bg))
      (setf (bgcolor    croatoan-window) bg)
      (setf (fgcolor    croatoan-window) fg))
    window))

(defun refresh-config-sizes (window conf-key)
  (let ((raw-height (swconf:win-height conf-key))
        (raw-width  (swconf:win-width  conf-key)))
    (with-croatoan-window (croatoan-window window)
      (resize croatoan-window
              (main-window:parse-subwin-h raw-height)
              (main-window:parse-subwin-w raw-width))
      window)))

(defun add-flush-left-text (window message y-start
                            &key
                              (bgcolor         nil)
                              (fgcolor         nil)
                              (attributes      nil)
                              (process-line-fn #'identity)
                              (has-border-p    nil)
                              (padding         1))
  "Add fitted lines generated from  `message' to `window' starting from row `y-start'.

Note that the window will expand its height to accomodate the text.

`process-line-fn` is  a function (default #'identity)  that is applied
to  each   line  of  processed   text  (not  the   original  message),
`has-border-p` if non nil will pust  padding in the windth of the text
to  take into  account  the border.  You can  add  extra padding  with
`padding' (default: 1)."
  (let* ((actual-y-start  (if has-border-p
                              (1+ y-start)
                              y-start))
         (actual-x        (if has-border-p
                              (1+ padding)
                              padding))
         (width           (if has-border-p
                              (- (win-width window) padding 2)
                              (- (win-width window) padding)))
         (words           (text-utils:split-words message))
         (message-lines   (text-utils:flush-left-mono-text words width))
         (height-lines    (length message-lines))
         (actual-window-h (calc-bottom-of-window-height window
                                                        :has-border-p has-border-p))
         (expand-height-p (> height-lines
                             actual-window-h)))
    (when expand-height-p
      (win-resize window
                  (win-width window)
                  (+ (win-height window)
                     (- height-lines
                        actual-window-h))))
    (loop
       for line in message-lines
       for y from actual-y-start do
         (print-text window
                     (funcall process-line-fn line)
                     actual-x
                     y
                     :bgcolor    bgcolor
                     :fgcolor    fgcolor
                     :attributes attributes))))

(defun make-blocking-message-dialog (screen parent title message-lines bg fg)
  "Make a  dialog that block  all other threads, `message-lines'  is a
list of strings (the text lines)."
  (let* ((low-level-window (make-blocking-croatoan-window))
         (window           (make-instance 'wrapper-window
                                          :parent          parent
                                          :croatoan-window low-level-window))
         (win-w            (max (+ 4 (length title))
                                (+ 2 (tui:find-max-line-width message-lines))))
         (win-h            (+ 2 (length message-lines)))
         (x                (truncate (- (/ (win-width screen) 2)
                                        (/ win-w 2))))
         (y                (truncate (- (/ (win-height screen) 2)
                                        (/ win-h 2)))))
    (setf (background low-level-window)
          (tui:make-win-background bg))
    (setf (bgcolor low-level-window) bg)
    (setf (fgcolor low-level-window) fg)
    (win-resize window win-w win-h)
    (win-move   window x y)
    (win-box window)
    (print-text window title 2 0)
    (loop
       for line in message-lines
       for y from 1 do
         (print-text window line 1 y))
    (win-refresh window)
    (get-char low-level-window)
    (win-close window)))

(defun make-dialog (parent title message color-pair
                    &optional (buttons nil)
                      (append-ok-button t))
  (let* ((lines              (text-utils:split-lines message))
         (max-line-size      (text-utils:find-max-line-length lines))
         (actual-buttons     (if append-ok-button
                                 (append (list +menu-button-ok+)
                                         buttons)
                                 buttons))
         (max-button-size    (text-utils:find-max-line-length actual-buttons))
         (max-message-height (- (win-height-no-border parent)
                                4))
         (message            (join-with-strings lines (format nil "~%")))
         (dialog-window      (make-instance 'dialog-window
                                            :stacked              nil
                                            :center               t
                                            :message-text         message
                                            :input-blocking       t
                                            :current-item-mark    ""
                                            :color-pair           color-pair
                                            :width                (min (+ max-line-size 4)
                                                                       (- (win-width parent)
                                                                          4))
                                            :border               t
                                            :enable-function-keys t
                                            :name                 title
                                            :title                t
                                            :max-item-length      (min (+ max-button-size 4)
                                                                       (- (win-width parent)
                                                                          4))
                                            :message-height       (min (1+ (length lines))
                                                                       max-message-height)
                                            :items                actual-buttons)))
    (make-instance 'wrapper-window
                   :croatoan-window dialog-window)))

(defun make-error-message-dialog (parent title message
                                  &optional
                                    (buttons nil)
                                    (append-ok-button t))
  (let ((bg (swconf:win-bg swconf:+key-error-dialog+))
        (fg (swconf:win-fg swconf:+key-error-dialog+)))
    (make-dialog parent title message (list fg bg) buttons append-ok-button)))

(defun make-info-message-dialog (parent title message
                                 &optional
                                   (buttons nil)
                                   (append-ok-button t))
  (let ((bg (swconf:win-bg swconf:+key-info-dialog+))
        (fg (swconf:win-fg swconf:+key-info-dialog+)))
    (make-dialog parent title message (list fg bg) buttons append-ok-button)))

(defun make-simple-style (foreground          background
                          selected-foreground selected-background)
  "Croatoan (ncurses) use a style to apply colors on a windows and its
content, this function generate a styele object (a plist) suitable for
the library."
  (list :foreground
        (list :fgcolor foreground          :bgcolor background)
        :background
        (list :fgcolor foreground          :bgcolor background)
        :selected-foreground
        (list :fgcolor selected-foreground :bgcolor selected-background)
        :selected-background
        (list :fgcolor selected-foreground :bgcolor selected-background)))

(defun style-class->list (style)
  (make-simple-style (swconf:input-foreground    style)
                     (swconf:input-background    style)
                     (swconf:selected-foreground style)
                     (swconf:selected-background style)))

(defun make-input-dialog (screen parent message)
  "A  dialog window  with  a  single input  field,  returns the  input
insetred by the user"
  (with-croatoan-window (screen-low-level screen)
    (let* ((theme-style      (swconf:form-style swconf:+key-input-dialog+))
           (style-form       (style-class->list theme-style))
           (fg               (swconf:foreground theme-style))
           (bg               (swconf:background theme-style))
           (window-width     (truncate (/ (win-width parent)
                                          4)))
           (window-height    (truncate (/ (win-height parent)
                                          4)))
           (field-width      (- window-width 4))
           (window-position  (list (truncate (- (/ (win-height parent) 2)
                                                (/ window-height 2)))
                                   (truncate (- (/ (win-width  parent) 2)
                                                (/ window-width 2)))))
           (button-cancel    (make-instance 'button
                                            :name     :b-cancel
                                            :title    (_ "Cancel")
                                            :position (list (truncate (1+ (* window-height
                                                                             3/4)))
                                                            2)))
           (button-accept    (make-instance 'button
                                            :name     :b-accept
                                            :title    (_ "OK")
                                            :position (list (truncate (* window-height 3/4))
                                                            2)))
           (field            (make-instance 'field
                                            :position (list (truncate (* window-height 1/2))
                                                            2)
                                            :width    field-width))
           (low-level-window (make-instance 'form-window
                                            :stacked        nil
                                            :input-blocking t
                                            :width          window-width
                                            :height         window-height
                                            :position       window-position
                                            :insert-mode    t
                                            :border         t
                                            :elements       (list field
                                                                  button-accept
                                                                  button-cancel)
                                            :style          (list 'field  style-form
                                                                  'button style-form
                                                                  'label  style-form)))
           (window           (make-instance 'wrapper-window
                                            :croatoan-window low-level-window)))
      (win-set-background window (make-win-background bg :color-fg fg))
      (add-flush-left-text window message 2 :has-border-p t)
      (win-refresh        window)
      (setf (callback button-accept) 'accept)
      (setf (callback button-cancel) 'cancel)
      (setf (cursor-visible-p screen-low-level) t)
      (let ((res (croatoan:edit low-level-window)))
        (setf (cursor-visible-p screen-low-level) nil)
        (win-close window)
        (and res
             (value field))))))

(defun make-checklist-dialog (screen parent title options)
  (with-croatoan-window (screen-low-level screen)
    (let* ((theme-style      (swconf:form-style swconf:+key-input-dialog+))
           (fg               (swconf:foreground theme-style))
           (bg               (swconf:background theme-style))
           (window-width     (truncate (/ (win-width parent)
                                          4)))
           (window-height    (truncate (/ (win-height parent)
                                          4)))
           (window-position  (list (truncate (- (/ (win-height parent) 2)
                                                (/ window-height 2)))
                                   (truncate (- (/ (win-width  parent) 2)
                                                (/ window-width 2)))))
           (button-cancel    (make-instance 'button
                                            :name     :b-cancel
                                            :title    (_ "Cancel")
                                            :position (list (truncate (1+ (* window-height
                                                                             3/4)))
                                                            2)))
           (button-accept    (make-instance 'button
                                            :name     :b-accept
                                            :title    (_ "OK")
                                            :position (list (truncate (* window-height 3/4))
                                                            2)))
           (low-level-window (make-instance 'menu-window
                                            :width                window-width
                                            :height               window-height
                                            :items                options
                                            :position             window-position
                                            :title                title
                                            :border               t
                                            :input-blocking       t
                                            :enable-function-keys t
                                            :menu-type            :checklist
                                            :color-pair           (list fg bg)))
            (window           (make-instance 'wrapper-window
                                             :croatoan-window low-level-window)))
      (win-set-background window (make-win-background bg :color-fg fg))
      (win-refresh        window)
      (setf (callback button-accept) 'accept)
      (setf (callback button-cancel) 'cancel)
      (setf (cursor-visible-p screen-low-level) t)
      (let ((results (select low-level-window)))
        (win-close window)
        (win-clear screen)
        (draw-all)
        (and results
             (mapcar #'value results))))))

(defclass focus-marked-window ()
  ((in-focus
    :initform nil
    :initarg  :in-focus
    :reader   in-focus-p
    :writer   (setf in-focus)
    :documentation "Sets check if this windows got focus")
   (focus-mark
    :initform "*"
    :initarg  :focus-mark
    :accessor focus-mark
    :documentation "the visual text to mark the focused window"))
  (:documentation "This is a window that can be marked visually when gets focus"))

(defmethod refresh-config :after ((object focus-marked-window))
  (multiple-value-bind (bg fg value)
      (swconf:config-win-focus-mark)
    (setf (focus-mark object)
          (make-tui-string value :fgcolor fg :bgcolor bg))))

(defmethod draw :after ((object focus-marked-window))
  (with-accessors ((in-focus-p   in-focus-p)
                   (focus-mark focus-mark)) object
    (when (in-focus-p object)
      (print-text object focus-mark 0 0))))

(defclass border-window ()
  ((uses-border-p
    :initform nil
    :initarg  :uses-border-p
    :reader   uses-border-p))
  (:documentation "This is a window that has a border."))

(defun window-uses-border-p (window)
  (and window
       (typep window 'border-window)
       (uses-border-p window)))

(defmethod draw :after ((object border-window))
  (when (uses-border-p object)
    (win-box object)))

(defclass title-window ()
  ((title
    :initform ""
    :initarg  :title
    :accessor title
    :documentation "The actual title")
   (title-padding-left
    :initform " "
    :initarg  :title-padding-left
    :accessor title-padding-left
    :documentation "left padding text for title")
   (left-stopper
    :initform ""
    :initarg  :left-stopper
    :accessor left-stopper
    :documentation "The text before the actual title")
   (right-stopper
    :initform ""
    :initarg  :right-stopper
    :accessor right-stopper
    :documentation "The text after the actual title"))
  (:documentation "This is a window that dplays a title

 ---border--- left-stopper title-padding-left right-stopper ---border---"))

(defmethod refresh-config :after ((object title-window))
  (multiple-value-bind (left-mark right-mark padding)
      (swconf:window-titles-ends)
    (with-accessors ((left-stopper       left-stopper)
                     (right-stopper      right-stopper)
                     (title-padding-left title-padding-left)) object
      (setf title-padding-left padding)
      (setf left-stopper       left-mark)
      (setf right-stopper      right-mark)))
  object)

(defmethod draw :after ((object title-window))
  (with-accessors ((left-stopper       left-stopper)
                   (right-stopper      right-stopper)
                   (title-padding-left title-padding-left)
                   (title              title)) object
    (print-text object left-stopper        title-padding-left 0)
    (print-text object title               nil                nil)
    (print-text object right-stopper       nil                nil)))
