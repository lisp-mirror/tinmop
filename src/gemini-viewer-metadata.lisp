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
;; along with this program.
;; If not, see [[http://www.gnu.org/licenses/][http://www.gnu.org/licenses/]].

(in-package :gemini-viewer)

(defstruct gemini-metadata
  (links)
  (history)
  (source-file))

(defgeneric append-metadata-link (object link))

(defgeneric append-metadata-source (object source-text))

(defmethod append-metadata-link ((object gemini-metadata) link)
  (setf (gemini-metadata-links object)
        (append (gemini-metadata-links object)
                link)))

(defmethod append-metadata-source ((object gemini-metadata) source-file)
  (setf (gemini-metadata-source-file object)
        (strcat (gemini-metadata-source-file object)
                source-file))
  object)

(defun add-url-to-history (window url)
  (let* ((metadata   (message-window:metadata window))
         (history    (reverse (gemini-metadata-history metadata)))
         (last-entry (safe-last-elt (gemini-metadata-history metadata))))
    (when (string/= last-entry
                    url)
      (setf (gemini-metadata-history metadata)
            (reverse (push url history))))
    window))

(defun maybe-initialize-metadata (window)
  (when (not (gemini-metadata-p (message-window:metadata window)))
    (setf (message-window:metadata window)
          (make-gemini-metadata)))
  (message-window:metadata window))

(defun current-gemini-url ()
  (when (message-window:gemini-window-p)
    (let* ((metadata (message-window:metadata specials:*message-window*))
           (link     (last-elt (gemini-viewer:gemini-metadata-history metadata))))
      link)))
