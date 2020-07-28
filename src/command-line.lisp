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

(in-package :command-line)

(defun print-version ()
  (format t (_ "~a version ~a~%") +program-name+ +program-version+))

(defmacro gen-opts ()
  `(opts:define-opts
     (:name                  :help
                             :description               (_ "Print help and exit")
                             :short                     #\h
                             :long                      "help")
     (:name                  :version
                             :description               (_ "Print program information and exit")
                             :short                     #\v
                             :long                      "version")
     (:name                  :folder
                             :description               (_ "Starting folder")
                             :short                     #\f
                             :arg-parser                #'identity
                             :meta-var                  (_ "FOLDER-NAME")
                             :long                      "folder")
     (:name                  :timeline
                             :description               (_ "Starting timeline")
                             :short                     #\t
                             :meta-var                  (_ "TIMELINE-NAME")
                             :arg-parser                #'identity
                             :long                      "timeline")
     (:name                  :update-timeline
                             :description               (_ "Update timeline")
                             :short                     #\u
                             :long                      "update-timeline")
     (:name                  :reset-timeline-pagination
                             :description               (_ "Reset the timeline pagination")
                             :short                     #\R
                             :long                      "reset-timeline-pagination")
     (:name                  :check-follows-requests
                             :description               (_ "Check follows requests")
                             :short                     #\c
                             :long                      "check-follows-requests")
     (:name                  :execute
                             :description               (_ "Execute script")
                             :short                     #\e
                             :arg-parser                #'identity
                             :meta-var                  (_ "SCRIPT-FILE")
                             :long                      "execute-script")
     (:name                  :notify-mentions
                             :description               (_ "Notify messages that mentions the user")
                             :short                     #\m
                             :long                      "notify-mentions")
     (:name                  :open-gemini-url
                             :description               (_ "Open gemini url")
                             :short                     #\o
                             :arg-parser                #'identity
                             :long                      "open-gemini-url")))

(defparameter *start-folder*                       nil)

(defparameter *start-timeline*                     nil)

(defparameter *update-timeline*                    nil)

(defparameter *script-file*                        nil)

(defparameter *check-follow-requests*              nil)

(defparameter *reset-timeline-pagination*          nil)

(defparameter *notify-mentions*                    nil)

(defparameter *gemini-url*                         nil)

(defparameter *update-timeline-climb-message-tree* nil)

(defun exit-on-error (e)
  (format *error-output* "~a~%" e)
  (os-utils:exit-program 1))

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
      (when (getf options :folder)
        (setf *start-folder* (getf options :folder)))
      (when (getf options :open-gemini-url)
        (setf *gemini-url* (getf options :open-gemini-url)))
      (when (getf options :timeline)
        (setf *start-timeline* (getf options :timeline)))
      (when (getf options :reset-timeline-pagination)
        (setf *reset-timeline-pagination* t))
      (when (getf options :update-timeline)
        (setf *update-timeline* t))
      (when (getf options :execute)
        (setf *script-file* (getf options :execute)))
      (when (getf options :check-follows-requests)
        (setf *check-follow-requests* (getf options :check-follows-requests)))
      (when (getf options :notify-mentions)
        (setf *notify-mentions* (getf options :check-follows-requests))))))
