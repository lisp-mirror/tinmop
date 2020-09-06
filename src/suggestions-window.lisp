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

(in-package :suggestions-window)

(defclass suggestions-window (wrapper-window)
  ((paginated-info
    :initform nil
    :initarg  :paginated-info
    :accessor paginated-info)
   (current-page
    :initform 0
    :initarg  :current-page
    :accessor current-page)))

(defun draw-pagination-info (win)
  (with-accessors ((paginated-info paginated-info)
                   (current-page   current-page)) win
    (let* ((msg   (format nil
                          (_ "Page ~a of ~a")
                          (1+ current-page)
                          (length paginated-info)))
           (msg-x (calc-center-on-window-width  win msg))
           (msg-y (calc-bottom-of-window-height win)))
      (print-text win msg msg-x msg-y))))

(defmethod refresh-config :after ((object suggestions-window))
  (with-croatoan-window (croatoan-window object)
    (refresh-config-colors object swconf:+key-suggestions-window+)
    (refresh-config-sizes  object swconf:+key-suggestions-window+)
    (let ((y (- (win-height *main-window*)
                (win-height object)
                +command-window-height+)))
      (win-move object 0 y))))

(defmethod draw ((object suggestions-window))
  (win-raise-to-top object))

(defgeneric update-suggestions (object hint &key &allow-other-keys))
