;; tinmop: an humble gemini and pleroma client
;; Copyright (C) 2020,2021  cage

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

(in-package :line-oriented-window)

(defclass line ()
  ((selected-fg
    :initform :black
    :initarg  :selected-fg
    :accessor selected-fg
    :documentation "The foreground color for a selected line")
   (selected-bg
    :initform :cyan
    :initarg  :selected-bg
    :accessor selected-bg
    :documentation "The background color for a selected line")
   (normal-fg
    :initform :cyan
    :initarg  :normal-fg
    :accessor normal-fg
    :documentation "The foreground color for a line")
   (normal-bg
    :initform :black
    :initarg  :normal-bg
    :accessor normal-bg
    :documentation "The background color for a line")
   (normal-text
    :initform (make-tui-string "...")
    :initarg  :normal-text
    :accessor normal-text
    :documentation "The actual not selected text")
   (selected-text
    :initform (make-tui-string "...")
    :initarg  :selected-text
    :accessor selected-text
    :documentation "The actual selected text")
   (deleted-text
    :initform (make-tui-string "...")
    :initarg  :deleted-text
    :accessor deleted-text
    :documentation "The actual deleted text ")
   (fields
    :initform ()
    :initarg  :fields
    :accessor fields
    :documentation "A generic plist of useful informations for the window")
   (index
    :initform 0
    :initarg  :index
    :accessor index
    :documentation "The index of this line in the window")
   (selected
    :initform nil
    :initarg  :selected
    :reader   selectedp
    :writer (setf selected)
    :documentation "Non nil if this line is selected state"))
  (:documentation "This class represents a single line in a row-oriented-widget"))

(defmethod print-object ((object line) stream)
  (format stream "line: ~s" (tui-string->chars-string (normal-text object))))

(defclass row-oriented-widget ()
  ((rows
    :initform ()
    :initarg  :rows
    :accessor rows
    :documentation "The rows of data for this widget")
   (row-selected-index
    :initform 0
    :initarg  :row-selected-index
    :accessor row-selected-index
    :documentation "The index of the selected row")
   (single-row-height
    :initform 1
    :initarg  :single-row-height
    :accessor single-row-height
    :documentation "The height of a row (in character)")
   (top-row-padding
    :initform 0
    :initarg  :top-row-padding
    :accessor top-row-padding
    :documentation "the  padding from  the top of  the window  and the
    position where to draw the first line")
   (current-row-index
    :initform 0
    :initarg  :current-row-index
    :accessor current-row-index
    :documentation "The active line index")
   (y-current-row
    :initform 0
    :initarg  :y-current-row
    :accessor y-current-row
    :documentation "The active line position")
   (top-rows-slice
    :initform 0
    :initarg  :top-rows-slice
    :accessor top-rows-slice
    :documentation "The start index of the visible rows")
   (bottom-rows-slice
    :initform 0
    :initarg  :bottom-rows-slice
    :accessor bottom-rows-slice
    :documentation "The start index of the visible rows"))
  (:documentation "A widget that holds a selectable list of lines"))

(defmethod initialize-instance :after ((object row-oriented-widget) &key &allow-other-keys)
  (with-accessors ((top-row-padding top-row-padding)
                   (y-current-row   y-current-row)) object
    (setf y-current-row top-row-padding)))

(defmethod (setf top-row-padding) ((object row-oriented-widget) new-padding)
  (setf (slot-value object 'top-row-padding) new-padding)
  (setf (slot-value object 'y-current-row)   new-padding)
  object)

(defgeneric renderizable-rows-data (object))

(defgeneric unselect-all (object))

(defgeneric select-row (object index))

(defgeneric select-first-row (object))

(defgeneric selected-row (object))

(defgeneric selected-row-fields (object))

(defgeneric selected-row-delete (object))

(defgeneric search-row (object regex &key redraw))

(defgeneric row-move (object amount)
  (:documentation "Move selected line of 'amount'. 'Amount' can be
  an integer number, if positive increase the position of the selected
  line otherwise decrease, relative to current index position.
  The value is clamped at range [ 0, '(length (rows object)))' ).
  This function return the number of positions acually moved"))

(defgeneric update-all-rows (object new-rows))

(defgeneric append-new-rows (object new-rows))

(defgeneric map-rows (object function &key &allow-other-keys))

(defgeneric rows-map-raw (object function &key &allow-other-keys))

(defgeneric rows-length (object &key &allow-other-keys))

(defgeneric rows-empty-p (object &key &allow-other-keys))

(defgeneric rows-remove-if (object function &key &allow-other-keys))

(defgeneric rows-safe-subseq (object start &key end &allow-other-keys))

(defgeneric rows-elt (object index &key &allow-other-keys))

(defgeneric rows-last-elt (object &key &allow-other-keys))

(defgeneric rows-first-elt (object &key &allow-other-keys))

(defgeneric rows-position-if (object predicate &key from-end start end key &allow-other-keys))

(defmethod renderizable-rows-data ((object row-oriented-widget))
  "Cut from all the lines a slice that that fits into the widget"
  (with-accessors ((top-row-padding    top-row-padding)
                   (current-row-index  current-row-index)
                   (row-selected-index row-selected-index)
                   (single-row-height  single-row-height)
                   (top-rows-slice     top-rows-slice)
                   (bottom-rows-slice  bottom-rows-slice)
                   (rows               rows))  object
    (let* ((window-height       (if (uses-border-p object)
                                    (win-height-no-border object)
                                    (win-height           object)))
           (available-rows      (truncate (/ (- window-height top-row-padding)
                                             single-row-height)))
           (selected-top-offset (rem row-selected-index available-rows))
           (top                 (- row-selected-index selected-top-offset))
           (bottom              (+ row-selected-index
                                   (- available-rows selected-top-offset))))
      (setf top-rows-slice    top
            bottom-rows-slice bottom)
      (values (safe-subseq rows top bottom)
              selected-top-offset))))

(defmethod unselect-all ((object row-oriented-widget))
  (loop for row in (rows object) do
       (setf (selected row) nil))
  object)

(defmethod select-row ((object row-oriented-widget) (index number))
  (with-accessors ((rows               rows)
                   (row-selected-index row-selected-index)) object
    (restart-case
        (if (or (<  index 0)
                (>= index (length rows)))
            (error 'conditions:out-of-bounds :idx index :seq rows)
            (progn
              (setf row-selected-index index)
              (setf (selected (elt rows index)) t)))
      (ignore-selecting-action (e)
        (declare (ignore e))
        nil)
      (set-default-index (e)
        (declare (ignore e))
        (setf row-selected-index 0)))
    object))

(defmethod select-first-row ((object row-oriented-widget))
  (select-row object 0))

(defun adjust-rows-select-first (window)
  (when (rows window)
    (select-first-row window)))

(defun adjust-rows-select-last (window)
  (with-accessors ((rows rows)) window
    (when rows
      (let ((height (win-height-no-border window)))
        (when (not (< (rows-length window) height))
          (select-row window (- (rows-length window)
                                height))))))
  window)

(defmethod adjust-selected-rows ((object row-oriented-widget) (strategy function))
  (with-accessors ((row-selected-index row-selected-index)) object
    (when (< row-selected-index
             (rows-length object))
      (funcall strategy object)))
  object)

(defmethod selected-row ((object row-oriented-widget))
  "Return the current selected row"
  (with-accessors ((rows               rows)
                   (row-selected-index row-selected-index)) object
    (when rows
      (if (<=  0
               row-selected-index
               (1- (length rows)))
          (elt rows row-selected-index)
          nil))))

(defmethod selected-row-fields ((object row-oriented-widget))
  "Return the fields current selected row"
  (when-let ((selected-row (selected-row object)))
    (fields selected-row)))

(defmethod selected-row-delete ((object row-oriented-widget))
  "delete the selected row"
  (with-accessors ((rows               rows)
                   (row-selected-index row-selected-index)) object
    (let ((last-was-removed-p (= row-selected-index
                                 (1- (length rows)))))
      (setf rows (remove-if #'selectedp rows))
      (if rows
          (if last-was-removed-p
              (select-row object (1- row-selected-index))
              (select-row object row-selected-index))
          (setf row-selected-index 0)))
    object))

(defmethod row-move ((object row-oriented-widget) amount)
  "Navigate the lines, move the  selected row by `amount', returns the
actual of  rows moved. This can  be different from `amount'  if moving
this exact quantity would go beyond the length or rows or zero."
  (with-accessors ((rows               rows)
                   (row-selected-index row-selected-index)) object
    (if (and rows
             row-selected-index
             (/= 0 amount))
        (let* ((desired-amount (+ amount row-selected-index))
               (actual-amount  (if (< amount 0)
                                   (max (- desired-amount
                                           row-selected-index)
                                        (- row-selected-index))
                                   (- (min desired-amount
                                           (1- (length rows)))
                                      row-selected-index))))
          (select-row object (+ row-selected-index
                                actual-amount))
          actual-amount)
        0)))

(defmethod search-row ((object row-oriented-widget) regex &key (redraw t))
  (handler-case
      (with-accessors ((row-selected-index row-selected-index)) object
        (let* ((scanner        (create-scanner regex :case-insensitive-mode t))
               (selected-row                 (selected-row object))
               (selected-text                (normal-text selected-row))
               (actual-row-starting          (if (scan scanner selected-text)
                                                 (1+ row-selected-index)
                                                 row-selected-index))
               (position-found (position-if (lambda (a)
                                              (if (selectedp a)
                                                  (scan scanner (selected-text a))
                                                  (scan scanner (normal-text   a))))
                                            (safe-subseq (rows object)
                                                         actual-row-starting))))
          (when position-found
            (unselect-all object)
            (select-row object (+ actual-row-starting position-found))
            (when redraw
              (draw object))
            position-found)))
    (error ()
      (ui:error-message (_ "Invalid regular expression")))))

(defmethod update-all-rows ((object row-oriented-widget) (new-rows sequence))
  (setf (rows object) new-rows))

(defmethod append-new-rows ((object row-oriented-widget) (new-rows sequence))
  (with-slots (rows) object
    (let ((reversed-old-rows (reverse rows)))
      (loop for new-row in new-rows do
        (push new-row reversed-old-rows))
      (setf rows (reverse reversed-old-rows)))))

(defmethod append-new-rows ((object row-oriented-widget) (new-rows line))
  (append-new-rows object (list new-rows)))

(defmethod map-rows ((object row-oriented-widget) (function function)
                     &key &allow-other-keys)
  (mapcar function (rows object)))

(defmethod rows-map-raw ((object row-oriented-widget) (function function)
                     &key &allow-other-keys)
  (mapcar function (slot-value object 'rows)))

(defmethod rows-length ((object row-oriented-widget) &key &allow-other-keys)
  (length (rows object)))

(defmethod rows-empty-p ((object row-oriented-widget) &key &allow-other-keys)
  (not (rows object)))

(defmethod rows-remove-if ((object row-oriented-widget) (function function) &key &allow-other-keys)
  (remove-if function (rows object)))

(defmethod rows-safe-subseq ((object row-oriented-widget) start
                             &key (end nil) &allow-other-keys)
  (safe-subseq (rows object) start end))

(defmethod rows-elt ((object row-oriented-widget) index &key &allow-other-keys)
  (elt (rows object) index))

(defmethod rows-last-elt ((object row-oriented-widget) &key &allow-other-keys)
  (last-elt (rows object)))

(defmethod rows-first-elt ((object row-oriented-widget) &key &allow-other-keys)
  (first-elt (rows object)))

(defmethod rows-position-if ((object row-oriented-widget) (predicate function)
                             &key from-end start end key &allow-other-keys)
  (position-if predicate
               (rows object)
               :from-end from-end
               :start    start
               :end      end
               :key      key))

(defun rows->text-rows (window &key (accessor-fn #'normal-text))
  (let ((*blanks* '(#\Newline)))
    (map-rows window
              (lambda (a)
                (trim-blanks (tui:tui-string->chars-string (funcall accessor-fn a)))))))

(defun rows->text (window &key (accessor-fn #'normal-text))
  (join-with-strings (rows->text-rows window :accessor-fn accessor-fn)
                     (format nil "~%")))

(defclass simple-line-navigation-window (wrapper-window row-oriented-widget border-window)
  ((selected-line-bg
    :initform :blue
    :initarg  :selected-line-bg
    :accessor selected-line-bg
    :documentation "The background color for a selected line")
   (selected-line-fg
    :initform :red
    :initarg  :selected-line-fg
    :accessor selected-line-fg
    :documentation "The foreground color for a selected line")
   ;; (line
   ;;  :initform :red
   ;;  :initarg  :selected-line-fg
   ;;  :accessor selected-line-fg
   ;;  :documentation "The foreground color for a selected line")
   (top-horizontal-padding
    :initform 0
    :initarg  :top-horizontal-padding
    :accessor top-horizontal-padding
    :documentation "The vertical padding (from top) of each single row"))
  (:documentation "A window that displays a navigable list of objects"))

(defmethod draw :after ((object simple-line-navigation-window))
  (with-accessors ((uses-border-p          uses-border-p)
                   (single-row-height      single-row-height)
                   (top-row-padding        top-row-padding)
                   (top-horizontal-padding top-horizontal-padding)) object
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
            for y from (+ y-start
                          top-horizontal-padding
                          top-row-padding)
            by single-row-height
            for ct from 0
            for row in rows do
              (if (selectedp row)
                  (print-text object
                              (right-pad-text (text-ellipsis (selected-text row)
                                                             max-line-size)
                                              max-line-size)
                              x y
                              :bgcolor (selected-bg row)
                              :fgcolor (selected-fg row))
                  (print-text object
                              (right-pad-text (text-ellipsis (normal-text row)
                                                             max-line-size)
                                              max-line-size)
                              x y
                              :bgcolor (normal-bg row)
                              :fgcolor (normal-fg row)))))))))

(defgeneric resync-rows-db (object &key redraw)
  (:documentation "Synchronize information table slot of `object` with
  table in the database, if `redraw`  is not nil redraw the object, if
  possible"))

(defun make-blocking-list-dialog-window (screen all-fields text-lines callback
                                         &optional (title (_ "Information")))
  "Draw a  window with a scrollable list of entries, pressing enter
will fire the `callback' function (with the selected field from `all-fields'
 and text from `text-line'. This window is fitten into `screen' sizes."
  (assert (>= (length all-fields)
              (length text-lines)))
  (let* ((low-level-window  (make-blocking-croatoan-window :enable-function-keys t))
         (window-width      (max (+ 4
                                    (length title))
                                 (+ (find-max-line-width text-lines)
                                    2)))
         (window-height     (min (truncate (* 0.9 (win-height screen)))
                                 (max (length text-lines)
                                      (+ +min-shown-win-height+
                                         2))))
         (window-x          (truncate (- (* 0.5 (win-width screen))
                                         (* 0.5 window-width))))
         (window-y          (truncate (- (* 0.5 (win-height screen))
                                         (* 0.5 window-height))))
         (bg                (swconf:win-bg swconf:+key-info-dialog+))
         (fg                (swconf:win-fg swconf:+key-info-dialog+))
         (high-level-window (make-instance 'simple-line-navigation-window
                                           :single-row-height 1
                                           :uses-border-p     t
                                           :croatoan-window   low-level-window)))
    (flet ((draw ()
             (win-clear high-level-window :redraw nil)
             (draw high-level-window)
             (win-box high-level-window)
             (print-text high-level-window title 2 0)))
      (setf (background low-level-window)
            (tui:make-win-background bg))
      (setf (fgcolor low-level-window)
            fg)
      (win-resize high-level-window window-width window-height)
      (win-move high-level-window window-x window-y)
      (setf (rows high-level-window)
            (loop
               for text in text-lines
               for fields in all-fields
               collect
                 (make-instance 'line
                                :fields        fields
                                :normal-text   text
                                :selected-text text
                                :normal-bg     bg
                                :normal-fg     fg
                                :selected-bg   fg
                                :selected-fg   bg)))
      (select-row high-level-window 0)
      (draw)
      (loop named inner
         for c = (get-wide-event low-level-window)
         while (string/= c "q")
         do
           (cond
             ((string= c :up)
              (unselect-all high-level-window)
              (row-move high-level-window -1))
             ((string= c :down)
              (unselect-all high-level-window)
              (row-move high-level-window 1))
             ((string= c #\Newline)
              (let ((selected-fields (selected-row-fields high-level-window))
                    (selected-text   (selected-text (selected-row high-level-window))))
                (funcall callback selected-text selected-fields))))
           (draw))
      (win-close high-level-window))))
