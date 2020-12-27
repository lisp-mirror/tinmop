;; Tinmop module to share the link pointing to the current gemini page
;; on pleroma.
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

(defun gemini-window-p ()
  (gemini-viewer:gemini-metadata-p (message-window:metadata specials:*message-window*)))

(defun share-gemini-link ()
  "Share the link pointing to the current gemini page on pleroma."
  (if (gemini-window-p)
      (let* ((metadata    (message-window:metadata specials:*message-window*))
             (link        (last-elt (gemini-viewer:gemini-metadata-history metadata)))
             (source      (gemini-viewer:gemini-metadata-source-file metadata))
             (source-head (with-input-from-string (stream source)
                            (read-line stream nil "...")))
             (headline    (format nil "~a~2%~a~%" source-head link)))
        (compose-message :message-header-text headline))
      (error-message (_ "The window is not displaying a gemini document"))))

(define-key "M-x s l" #'share-gemini-link *gemini-message-keymap*)
