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

(in-package :modules)

(defun load-sys-module (path &key (not-found-signal-error t))
  (a:when-let ((file (if not-found-signal-error
                         (get-sys-config-file path)
                         (ignore-errors (get-sys-config-file path)))))
    (load file :verbose nil :print nil)))

(defun load-module (path &key (not-found-signal-error t))
  (flet ((%load (file)
           (load file :verbose nil :print nil)))
    (let ((config-file (conditions:with-default-on-error (nil)
                         (get-config-file path)))
          (data-file   (conditions:with-default-on-error (nil)
                         (get-data-file path))))
      (cond
        (config-file
         (%load config-file))
        (data-file
         (%load data-file))
        (t
         (let ((error-message (format nil
                                      (_ "Unrecoverable error: file ~a not found in any of the directory ~a ~a ~a ~a")
                                      path
                                      +sys-data-dir+
                                      +sys-conf-dir+
                                      (home-datadir)
                                      (home-confdir))))
           (if not-found-signal-error
               (error error-message)
               (values nil error-message))))))))
