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
  (let* ((editor (or (swconf:external-editor)
                     (and (text-utils:string-not-empty-p (getenv "VISUAL"))
                          (getenv "VISUAL"))
                     (and (text-utils:string-not-empty-p (getenv "EDITOR"))
                          (getenv "EDITOR"))
                     constants:+standard-editor+))
         (space (cl-ppcre:scan "\\s" editor)))
    (if space
        (let ((exe  (subseq editor 0 space))
              (args (subseq editor (1+ space))))
          (values exe args))
        (values editor nil))))

(defun run-external-program (program args
                             &key
                               (wait t)
                               search
                               input
                               output
                               (error :output)
                               #+sbcl (if-output-exists :supersede)
                               #+sbcl (if-error-exists :supersede))
  (declare (ignorable search))
  #+ecl (ext:run-program program
                         args
                         :input  input
                         :output output
                         :error  error
                         :wait   wait)
  #+sbcl (sb-ext:run-program program
                             args
                             :wait   wait
                             :search search
                             :input  input
                             :output output
                             :error  error
                             :if-output-exists if-output-exists
                             :if-error-exists if-error-exists))

(defun process-exit-code (process)
  #+ecl  (nth-value 1 (ext:external-process-status process))
  #+sbcl (sb-ext:process-exit-code process))

(defun process-exit-success-p (process)
  (= (process-exit-code process) 0))

(defun open-with-editor (file)
  (multiple-value-bind (exe args)
      (external-editor)
    (let ((actual-args (if args
                           (text-utils:split-words args)
                           nil)))
      (run-external-program exe
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
                            (text-utils:strcat "req -new -nodes -x509 -days 365 -subj / "
                                               "-keyout ~a -outform PEM -out ~a")
                            key-file
                            cert-file)))
    (run-external-program +openssl-bin+
                          (text-utils:split-words cmd-args)
                          :output nil
                          :error  :output)
    (values cert-file key-file)))

(defun send-to-pipe (data program-and-args)
  (croatoan:end-screen)
  (with-input-from-string (stream data)
    (let ((command-line-splitted (text-utils:split-words program-and-args)))
      (run-external-program (first command-line-splitted)
                            (rest command-line-splitted)
                            :search t
                            :wait   t
                            :input  stream
                            :output t
                            :error  t))))

(defun open-link-with-program (program-and-args link &key (wait nil))
  (let* ((command-line-splitted (text-utils:split-words program-and-args))
         (program               (first command-line-splitted))
         (args                  (append (rest command-line-splitted)
                                        (list link))))
    (run-external-program program
                          args
                          :search t
                          :wait   wait
                          :output nil
                          :error  :output)))

(defun open-resource-with-external-program (resource give-focus-to-message-window
                                            &key (open-for-edit nil))
  (flet ((edit (file)
           (croatoan:end-screen)
           (os-utils:open-with-editor file)))
    (let ((program (swconf:link-regex->program-to-use resource)))
      (if program
          (cond
            ((swconf:use-editor-as-external-program-p program)
             (edit resource))
            ((swconf:use-tinmop-as-external-program-p program)
             (if open-for-edit
                 (edit resource)
                 (gemini-viewer:load-gemini-url resource
                                                :give-focus-to-message-window
                                                give-focus-to-message-window)))
            (t
             (os-utils:open-link-with-program program resource :wait open-for-edit)))
          (if open-for-edit
              (error (_ "No program defined in configuration file to edit this kind of files."))
              (os-utils:xdg-open resource))))))

(defun unzip-file (zip-file destination-dir)
  (cond
    ((not (fs:file-exists-p zip-file))
     (error (format nil (_ "File ~s does not exists"))))
    ((not (fs:directory-exists-p destination-dir))
     (error (format nil (_ "Destination directory ~s does not exists"))))
    (t
     (run-external-program +unzip-bin+
                           (list "-o" zip-file "-d" destination-dir)
                           :search t
                           :wait   t
                           :output nil
                           :error  :output))))

(defun unzip-single-file (zip-file file-entry)
  (with-output-to-string (stream)
    (let* ((process   (run-external-program +unzip-bin+
                                            (list "-p" zip-file file-entry)
                                            :search t
                                            :wait   t
                                            :output stream
                                            :error  :output)))
      (when (not (process-exit-success-p process))
        (error (format nil "File ~s extraction from ~s failed" file-entry zip-file))))))

(defun copy-to-clipboard (text)
  (trivial-clipboard:text text))
