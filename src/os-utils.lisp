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

(alexandria:define-constant +ssl-cert-name+ "cert.pem" :test #'string=)

(alexandria:define-constant +ssl-key-name+  "key" :test #'string=)

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
  (let ((cmd-line (format nil "~a '~a'" +xdg-open-bin+ file)))
    #+debug-mode
    (misc:dbg "xdg-open: ~a" cmd-line)
    (uiop:launch-program cmd-line :output nil)))

(defun getenv (name)
  (nix:getenv name))

(defun default-temp-dir ()
  (or (os-utils:getenv "TMPDIR")
      "/tmp/"))

(defun pwd ()
  (os-utils:getenv "PWD"))

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
    (let ((actual-args (if args
                           (text-utils:split-words args)
                           nil)))
      (sb-ext:run-program exe
                          (append actual-args
                                  (list file))
                          :search t
                          :wait   t
                          :input  t
                          :output t
                          :error  t))))

(defun exit-program (&optional (exit-code 0))
  (uiop:quit exit-code))

(defun user-cache-dir (&rest more)
  (fs:pathname->namestring (apply #'uiop:xdg-cache-home
                                  (append (list +program-name+) more))))

(defun cached-file-path (filename)
  (text-utils:strcat (user-cache-dir) fs:*directory-sep* filename))

(defun generate-ssl-certificate (outdir)
  (let* ((cert-file (text-utils:strcat outdir fs:*directory-sep* +ssl-cert-name+))
         (key-file  (text-utils:strcat outdir fs:*directory-sep* +ssl-key-name+))
         (cmd-args  (format nil
                            (text-utils:strcat "req -new -nodes -x509 -days 365 -batch "
                                               "-keyout ~a -outform PEM -out ~a")
                            key-file
                            cert-file)))
    (sb-ext:run-program +openssl-bin+
                        (text-utils:split-words cmd-args)
                        :output nil
                        :error  :output)
    (values cert-file key-file)))

(defun send-to-pipe (data program-and-args)
  (croatoan:end-screen)
  (with-input-from-string (stream data)
    (let ((command-line-splitted (text-utils:split-words program-and-args)))
      (sb-ext:run-program (first command-line-splitted)
                          (rest command-line-splitted)
                          :search t
                          :wait   t
                          :input  stream
                          :output t
                          :error  t))))

(defun open-link-with-program (program-and-args link)
  (let* ((command-line-splitted (text-utils:split-words program-and-args))
         (program               (first command-line-splitted))
         (args                  (append (rest command-line-splitted)
                                        (list link))))
    (sb-ext:run-program program
                        args
                        :search t
                        :wait nil
                        :output nil
                        :error  :output)))

(defun open-resource-with-external-program (resource give-focus-to-message-window)
  (let ((program (swconf:link-regex->program-to-use resource)))
    (if program
        (if (swconf:use-tinmop-as-external-program-p program)
            (gemini-viewer:load-gemini-url resource
                                           :give-focus-to-message-window
                                           give-focus-to-message-window)
            (os-utils:open-link-with-program program resource))
        (os-utils:xdg-open resource))))
