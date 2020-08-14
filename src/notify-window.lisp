;; tinmop: an humble mastodon client
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
;; along with this program.
;; If not, see [[http://www.gnu.org/licenses/][http://www.gnu.org/licenses/]].

(in-package :notify-window)

(define-constant +notify-win-background-color+ '(:yellow :blue)
  :test #'equalp)

(define-constant +notify-win-background+ (make-instance 'complex-char
                                                        :simple-char #\Space
                                                        :color-pair  +notify-win-background-color+
                                                        :attributes  nil)
  :test #'complex-char=)

(defclass notify-window (wrapper-window)
  ((life
    :initform 10.0
    :initarg  :life
    :accessor life)
   (pending
    :initform 0
    :initarg  :pending
    :accessor pending)))

(defun notify-window-p (thing)
  (typep thing 'notify-window))

(defun notification-terminated-p (notification-window)
  (< (life notification-window) 0.0))

(defun notification-alive-p (notification-window)
  (not (notification-terminated-p notification-window)))

(defmethod refresh-config :after ((object notify-window))
  (refresh-config-colors object swconf:+key-notify-window+))

(defmethod calculate ((object notify-window) dt)
  (with-accessors ((life life)) object
    (when (notification-terminated-p object)
      (let ((remove-win-event (make-instance 'program-events:remove-notify-user-event
                                             :priority program-events:+maximum-event-priority+
                                             :payload  object)))
        (win-close object)
        (program-events:push-event remove-win-event)))
    (decf (life object) dt)))

(defmethod draw ((object notify-window))
  (declare (ignore object)))

(defgeneric draw-pending (object))

(defmethod draw-pending ((object notify-window))
  (with-accessors ((pending pending)) object
    (when (and (> pending 0)
               (notification-alive-p object))
      (print-text object
                  (format nil (n_ "~a pending"
                                  "~a pending"
                                  pending)
                          pending)
                  2
                  (1- (win-height object))))))

(defun force-error-colors (window)
  (with-croatoan-window (croatoan-window window)
    (setf (background croatoan-window)
          (tui:make-win-background :red))
    (setf (bgcolor    croatoan-window) :red)
    (setf (fgcolor    croatoan-window) :yellow))
  window)

(defun make-notification-window (message life &key (pending 0) (hidep nil) (notify-error nil))
  (let* ((low-level-window  (make-croatoan-window :draw-border t))
         (high-level-window (make-instance 'notify-window
                                           :life            life
                                           :pending         pending
                                           :croatoan-window low-level-window)))
    (refresh-config high-level-window)
    (when notify-error
      (force-error-colors high-level-window))
    (let ((win-w (truncate (* 1/6 (win-width  *main-window*))))
          (win-h (truncate (* 1/8 (win-height *main-window*)))))
      (win-resize high-level-window win-w win-h)
      ;; add-flush-left-text will expand window's height if needed
      (add-flush-left-text high-level-window
                           message 0
                           :has-border-p t
                           :attributes   (attribute-bold))
      (win-raise-to-top high-level-window)
      (win-move high-level-window 1 1)
      (win-box high-level-window)
      (when hidep
        (win-hide high-level-window))
      (mtree:add-child specials:*main-window* high-level-window)
      high-level-window)))
