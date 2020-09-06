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

(in-package :os-utils)

(alexandria:define-constant +proc-file-system+
    (concatenate 'string filesystem-utils:*directory-sep*
                 "proc")
  :test #'string=)

(alexandria:define-constant +proc-cpuinfo+
    (concatenate 'string +proc-file-system+ filesystem-utils:*directory-sep*
                 "cpuinfo")
  :test #'string=)

(declaim (ftype (function () fixnum) cpu-number))

(defun cpu-number ()
  #+windows (the fixnum 1)
  #-windows
  (with-open-file (stream +proc-cpuinfo+ :direction :input
                            :if-does-not-exist :error)
    (do ((line (read-line stream nil nil) (read-line stream nil nil))
         (cpu-count 0))
        ((not line) (the fixnum cpu-count))
      (when (cl-ppcre:scan "^processor" line)
        (incf cpu-count)))))

(defun xdg-open (file)
  (uiop:launch-program (format nil "xdg-open '~a'" file)
                       :output nil))

(defun getenv (name)
  (nix:getenv name))

(defun default-temp-dir ()
  (or (os-utils:getenv "TMPDIR")
      "/tmp/"))

(defun external-editor ()
  (let ((error-message
         (_ "No editor found, please configure the 'editor' directive in your configuration file"))
        (editor (or (swconf:external-editor)
                    (getenv "EDITOR"))))
    (if (null editor)
        (error error-message)
        (let ((space (cl-ppcre:scan "\\s" editor)))
          (if space
              (let ((exe  (subseq editor 0 space))
                    (args (subseq editor (1+ space))))
                (values exe args))
              (values editor nil))))))

(defun open-with-editor (file)
  (multiple-value-bind (exe args)
      (external-editor)
    (sb-ext:run-program exe
                        (append (list args)
                                (list file))
                        :search t
                        :wait   t
                        :input  t
                        :output t
                        :error  t)))

(defun exit-program (&optional (exit-code 0))
  (uiop:quit exit-code))

(defun user-cache-dir (&rest more)
  (fs:pathname->namestring (apply #'uiop:xdg-cache-home
                                  (append (list +program-name+) more))))

(defun cached-file-path (filename)
  (text-utils:strcat (user-cache-dir) fs:*directory-sep* filename))
