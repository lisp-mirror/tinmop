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

(in-package :command-line)

(defun print-version ()
  (format t (_ "~a version ~a~%") +program-name+ +program-version+))

(defmacro gen-opts ()
  `(opts:define-opts
     (:name :help
            :description               (_ "Print help and exit")
            :short                     #\h
            :long                      "help")
     (:name :version
            :description               (_ "Print program information and exit")
            :short                     #\v
            :long                      "version")
     (:name :folder
            :description               (_ "Starting folder")
            :short                     #\f
            :arg-parser                #'identity
            :meta-var                  (_ "FOLDER-NAME")
            :long                      "folder")
     (:name :timeline
            :description               (_ "Starting timeline")
            :short                     #\t
            :meta-var                  (_ "TIMELINE-NAME")
            :arg-parser                #'identity
            :long                      "timeline")
     (:name :update-timeline
            :description               (_ "Update timeline")
            :short                     #\u
            :long                      "update-timeline")
     (:name :reset-timeline-pagination
            :description               (_ "Reset the timeline pagination")
            :short                     #\R
            :long                      "reset-timeline-pagination")
     (:name :check-follows-requests
            :description               (_ "Check follows requests")
            :short                     #\c
            :long                      "check-follows-requests")
     (:name :execute
            :description               (_ "Execute script")
            :short                     #\e
            :arg-parser                #'identity
            :meta-var                  (_ "SCRIPT-FILE")
            :long                      "execute-script")
     (:name :notify-mentions
            :description               (_ "Notify messages that mentions the user")
            :short                     #\m
            :long                      "notify-mentions")
     (:name :open-gemini-url
            :description               (_ "Open gemini url")
            :short                     #\o
            :arg-parser                #'identity
            :long                      "open-gemini-url")
     (:name :gemini-full-screen-mode
            :description               (_ "Start as gemini client only.")
            :short                     #\G
            :long                      "gemini-client-only")
     (:name :load-module
            :description               (_ "Load a module")
            :short                     #\M
            :meta-var                  (_ "MODULE-FILE")
            :arg-parser                #'identity
            :long                      "load-module")
     (:name :print-lisp-dependencies
            :description               "Download lisp libraries (useful for packaging only)."
            :short                     #\X
            :long                      "lisp-dependencies-uris")))

(defparameter *start-folder*                       nil)

(defparameter *start-timeline*                     nil)

(defparameter *update-timeline*                    nil)

(defparameter *script-file*                        nil)

(defparameter *module-file*                        nil)

(defparameter *check-follow-requests*              nil)

(defparameter *reset-timeline-pagination*          nil)

(defparameter *notify-mentions*                    nil)

(defparameter *gemini-url*                         nil)

(defparameter *update-timeline-climb-message-tree* nil)

(defparameter *gemini-full-screen-mode*            nil)

(defparameter *print-lisp-dependencies*            nil)

(defun exit-on-error (e)
  (format *error-output* "~a~%" e)
  (os-utils:exit-program 1))

(defmacro set-option-variable (options option-name option-variable)
  (with-gensyms (option-value)
    `(let ((,option-value (getf ,options ,option-name)))
       (when ,option-value
         (setf ,option-variable ,option-value)))))

(defun manage-opts ()
  (handler-bind ((opts:unknown-option          #'exit-on-error)
                 (opts:missing-arg             #'exit-on-error)
                 (opts:missing-required-option #'exit-on-error))
    (gen-opts)
    (let ((options (opts:get-opts)))
      (when (getf options :help)
        (print-version)
        (opts:describe :usage-of                +program-name+
                       :usage-of-label          (_ "Usage")
                       :available-options-label (_ "Available options"))
        (os-utils:exit-program))
      (when (getf options :version)
        (print-version)
        (os-utils:exit-program))
      (set-option-variable options :folder                     *start-folder*)
      (set-option-variable options :open-gemini-url            *gemini-url*)
      (set-option-variable options :timeline                   *start-timeline*)
      (set-option-variable options :reset-timeline-pagination  *reset-timeline-pagination*)
      (set-option-variable options :update-timeline            *update-timeline*)
      (set-option-variable options :execute                    *script-file*)
      (set-option-variable options :load-module                *module-file*)
      (set-option-variable options :check-follows-requests     *check-follow-requests*)
      (set-option-variable options :gemini-full-screen-mode    *gemini-full-screen-mode*)
      (set-option-variable options :notify-mentions            *notify-mentions*)
      (set-option-variable options :print-lisp-dependencies    *print-lisp-dependencies*))))
