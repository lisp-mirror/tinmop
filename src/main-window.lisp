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

(in-package :main-window)

(defclass main-window (wrapper-window)
  ((focused-window
    :initform nil
    :initarg  :focused-window
    :accessor focused-window
    :documentation "The window  with the focus, only a  window can get
    the focus at the same time"))
  (:documentation "the main window AKA the screen"))

(defmethod refresh-config :after ((object main-window))
  (refresh-config-colors object swconf:+key-main-window+))

(defmethod calculate ((object main-window) dt)
  (do-children (child object)
    (calculate child dt)))

(defmethod draw ((object main-window))
  (do-children (child object)
    (draw child)))

(defgeneric focused-keybindings (object))

(defmethod focused-keybindings ((object main-window))
  "Return the keymap of the window with focus"
  (with-accessors ((focused-window focused-window)) object
    (when focused-window
      (when-let ((keybindings (keybindings focused-window)))
        keybindings))))

(defun init ()
  "Initialize the screen"
  (let ((screen (make-screen)))
    (setf *main-window*
          (make-instance 'main-window
                         :keybindings     keybindings:*global-keymap*
                         :key-config      swconf:+key-main-window+
                         :croatoan-window screen))
    (refresh-config *main-window*)
    *main-window*))

(defun parse-subwin-size (size-as-string main-window-size)
  "Parse a window size, size is a fraction of the main window size"
  (let* ((raw (num:safe-parse-number size-as-string
                                     :fix-fn (lambda (e)
                                               (declare (ignore e))
                                               main-window-size))))
    (cond
      ((integerp raw)
       raw)
      (t
       (truncate (* raw main-window-size))))))

(defun parse-subwin-w (w-as-string)
  "Parse a window width, `w-as-string' a fraction of the main window width"
  (parse-subwin-size w-as-string (win-width *main-window*)))

(defun parse-subwin-h (h-as-string)
  "Parse a window height, `h-as-string' a fraction of the main window height"
  (parse-subwin-size h-as-string (win-height *main-window*)))
