;; tinmop module for utility move command in thread window
;; Copyright © 2020 cage

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

(in-package :modules)

(defun open-next ()
  "Go to the next message and open it"
  (ui:thread-go-down)
  (ui:thread-open-selected-message))

(defun open-previous ()
  "Go to the previous message and open it"
  (ui:thread-go-up)
  (ui:thread-open-selected-message))

(defun delete-and-move-next ()
  (ui:thread-mark-delete-selected-message)
  (ui:thread-open-selected-message))

(defun delete-and-move-previous ()
  (ui:thread-mark-delete-selected-message)
  (ui:thread-open-selected-message))

(define-key "right" #'open-next                *thread-keymap*)

(define-key "left"  #'open-previous            *thread-keymap*)

(define-key "d"     #'delete-and-move-next     *thread-keymap*)

(define-key "M-u"   #'delete-and-move-previous *thread-keymap*)
