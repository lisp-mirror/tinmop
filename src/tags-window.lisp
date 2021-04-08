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

(in-package :tags-window)

(defclass tags-window (focus-marked-window
                       simple-line-navigation-window
                       title-window
                       border-window)
  ((new-messages-mark
    :initarg  :new-messages-mark
    :initform ()
    :accessor new-messages-mark)
   (histogram-fg
    :initarg  :histogram-fg
    :initform ()
    :accessor histogram-fg)))

(defmethod refresh-config :after ((object tags-window))
  (with-accessors ((croatoan-window   croatoan-window)
                   (histogram-fg      histogram-fg)
                   (selected-line-bg  selected-line-bg)
                   (selected-line-fg  selected-line-fg)
                   (new-messages-mark new-messages-mark)) object
    (let* ((theme-style       (swconf:form-style swconf:+key-tags-window+))
           (fg                (swconf:foreground          theme-style))
           (bg                (swconf:background          theme-style))
           (selected-fg       (swconf:selected-foreground theme-style))
           (selected-bg       (swconf:selected-background theme-style))
           (new-message-value (swconf:tags-new-message-mark))
           (width             (- (win-width *main-window*)
                                 (win-width *thread-window*)))
           (raw-height        (swconf:win-height swconf:+key-tags-window+))
           (height            (main-window:parse-subwin-h raw-height))
           (y                 0)
           (x                 0))
      (setf selected-line-fg selected-fg)
      (setf selected-line-bg selected-bg)
      (setf new-messages-mark new-message-value)
      (setf histogram-fg (swconf:tags-histogram-foreground))
      (setf (background croatoan-window)
            (tui:make-win-background bg))
      (setf (bgcolor croatoan-window) bg)
      (setf (fgcolor croatoan-window) fg)
      (win-resize object width height)
      (win-move object x y)
      object)))

(defmethod draw :before ((object tags-window))
  (with-accessors ((rows              rows)
                   (histogram-fg      histogram-fg)
                   (single-row-height single-row-height)
                   (top-row-padding   top-row-padding)
                   (new-messages-mark new-messages-mark)) object
    (win-clear object)
    (with-croatoan-window (croatoan-window object)
      (let ((histogram-width (truncate (* 2/3 (win-width-no-border object)))))
        (loop
           for y from (+ 2 top-row-padding) by single-row-height
           for row-fields  in (map-rows object #'fields) do
             (let* ((histogram-data            (fields-histogram row-fields))
                    (length-histogram-data     (length histogram-data))
                    (histogram-visualized-data (safe-subseq histogram-data
                                                            (- length-histogram-data
                                                               histogram-width)
                                                            length-histogram-data))
                    (histogram                 (cl-spark:spark histogram-visualized-data))
                    (got-new-messages-p        (getf row-fields :got-new-message-p)))
               (print-text object
                           histogram
                           1 y
                           :bgcolor (bgcolor croatoan-window)
                           :fgcolor histogram-fg)
               (when got-new-messages-p
                 (print-text object new-messages-mark nil nil
                             :bgcolor (bgcolor croatoan-window)
                             :fgcolor histogram-fg))))))))

(defmethod resync-rows-db ((object tags-window) &key (redraw t) (suggested-message-index nil))
  (with-accessors ((rows              rows)
                   (selected-line-bg  selected-line-bg)
                   (selected-line-fg  selected-line-fg)) object
    (flet ((make-rows (line-fields bg fg)
             (mapcar (lambda (fields)
                       (let ((text (db:tag->folder-name (fields-tag fields))))
                         (make-instance 'line
                                        :fields        fields
                                        :normal-text   text
                                        :selected-text text
                                        :normal-bg     bg
                                        :normal-fg     fg
                                        :selected-bg   fg
                                        :selected-fg   bg)))
                     line-fields)))
      (let ((line-fields (make-tag-line-fields)))
        (line-oriented-window:update-all-rows object
                                              (make-rows line-fields
                                                         selected-line-bg
                                                         selected-line-fg))
        (when suggested-message-index
          (select-row object suggested-message-index))
        (when redraw
          (draw object))))))

(defun fields-tag (fields)
  (getf fields :tag))

(defun fields-histogram (fields)
  (getf fields :histogram))

(defun make-tag-line-fields ()
  (let* ((all-tags-name  (db:all-subscribed-tags-name :sort-data t))
         (all-tags       (db:all-subscribed-tags      :sort-data t))
         (all-histograms (loop for tag-name in all-tags-name collect
                              (db:tag-histogram tag-name))))
    (loop
       for tag in all-tags
       for histogram in all-histograms collect
         (list :tag               (db:row-id                  tag)
               :got-new-message-p (db:row-tag-got-new-message tag)
               :histogram         histogram))))

(defun init ()
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *tags-window*
          (make-instance 'tags-window
                         :title             (_ "Subscribed tags")
                         :single-row-height 3
                         :uses-border-p     t
                         :keybindings       keybindings:*tags-keymap*
                         :croatoan-window   low-level-window))
    (refresh-config *tags-window*)
    (resync-rows-db *tags-window* :redraw nil)
    (when (not (line-oriented-window:rows-empty-p *tags-window*))
      (select-row *tags-window* 0))
    (draw *tags-window*)
    *tags-window*))
