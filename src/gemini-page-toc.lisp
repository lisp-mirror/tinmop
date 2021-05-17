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

(in-package :gemini-page-toc)

(defclass gemini-toc-window (focus-marked-window
                             simple-line-navigation-window
                             title-window
                             border-window)
  ((gemini-window
    :initform specials:*message-window*
    :initarg  gemini-window
    :accessor gemini-window)))

(defmethod refresh-config :after ((object gemini-toc-window))
  (with-accessors ((croatoan-window   croatoan-window)
                   (histogram-fg      histogram-fg)
                   (selected-line-bg  selected-line-bg)
                   (selected-line-fg  selected-line-fg)
                   (new-messages-mark new-messages-mark)) object
    (let* ((theme-style (swconf:form-style swconf:+key-gemini-toc-window+))
           (fg          (swconf:foreground          theme-style))
           (bg          (swconf:background          theme-style))
           (selected-fg (swconf:selected-foreground theme-style))
           (selected-bg (swconf:selected-background theme-style))
           (width       (- (win-width *main-window*)
                           (win-width *thread-window*)))
           (raw-height  (swconf:win-height swconf:+key-gemini-toc-window+))
           (height      (- (main-window:parse-subwin-h raw-height)
                           (win-height *command-window*)))
           (y           0)
           (x           0))
      (setf selected-line-fg selected-fg)
      (setf selected-line-bg selected-bg)
      (setf (background croatoan-window) (tui:make-win-background bg))
      (setf (bgcolor croatoan-window) bg)
      (setf (fgcolor croatoan-window) fg)
      (win-resize object width height)
      (win-move object x y)
      object)))

(defmethod resync-rows-db ((object gemini-toc-window) &key (redraw t) (suggested-message-index nil))
  (with-accessors ((rows              rows)
                   (selected-line-bg  selected-line-bg)
                   (selected-line-fg  selected-line-fg)
                   (gemini-window     gemini-window)) object
    (flet ((make-rows (toc bg fg)
             (mapcar (lambda (fields)
                       (let ((text (message-window:gemini-toc-entry fields toc)))
                         (make-instance 'line
                                        :fields        fields
                                        :normal-text   text
                                        :selected-text text
                                        :normal-bg     bg
                                        :normal-fg     fg
                                        :selected-bg   fg
                                        :selected-fg   bg)))
                     toc)))
      (let ((toc (message-window:generate-gemini-toc gemini-window)))
        (line-oriented-window:update-all-rows object
                                              (make-rows toc
                                                         selected-line-bg
                                                         selected-line-fg))
        (when suggested-message-index
          (select-row object suggested-message-index))
        (when redraw
          (win-clear object)
          (draw object))))))

(defun open-toc-window (gemini-window)
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *gemini-toc-window*
          (make-instance 'gemini-toc-window
                         :title             (_ "Table of contents")
                         :uses-border-p     t
                         :keybindings       keybindings:*gemini-toc-keymap*
                         :croatoan-window   low-level-window
                         :gemini-window     gemini-window))
    (refresh-config *gemini-toc-window*)
    (resync-rows-db *gemini-toc-window* :redraw nil)
    (when (not (line-oriented-window:rows-empty-p *gemini-toc-window*))
      (select-row *gemini-toc-window* 0))
    (draw *gemini-toc-window*)
    *gemini-toc-window*))
