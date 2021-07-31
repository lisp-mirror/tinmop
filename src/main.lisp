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

(in-package :main)

(defparameter *time* 0.0)

(defparameter *ticks* 0)

(define-constant +dt+ (/ 1 +fps+) :test #'=)

(defun incf-dt ()
  (incf *time* +dt+))

(defun incf-ticks ()
  (incf *ticks*))

(defun setup-bindings ()
  "This is where an  UI event is bound to a function  the event nil is
the event  that is fired  when no input  from user (key  pressed mouse
etc.) happened"
  (windows:with-croatoan-window (croatoan-window specials:*main-window*)
    (bind croatoan-window
          :resize
          (lambda (w event)
            (declare (ignore w event))
            (windows:refresh-config-all)
            (windows:draw-all)))
    (bind croatoan-window
          t
          (lambda (w event)
            (declare (ignore w))
            (incf-dt)
            (handler-bind ((conditions:command-not-found
                            (lambda (e)
                              (invoke-restart 'command-window:print-error e))))
              (command-window:manage-event event))))
    ;; this is the main thread
    (bind croatoan-window
          nil
          (lambda (w e)
            (declare (ignore w e))
            (incf-dt)
            (incf-ticks)
            (scheduled-events:run-scheduled-events *ticks*)
            (when (not (program-events:stop-event-dispatching-p))
              (program-events:dispatch-program-events))
            (windows:calculate-all +dt+)))))

(defun init-i18n ()
  "Initialize i18n machinery"
  (handler-bind ((error
                  (lambda (e)
                    (declare (ignore e))
                    (invoke-restart 'cl-i18n:return-empty-translation-table))))
    (setf cl-i18n:*translation-file-root* +catalog-dir+)
    (cl-i18n:load-language +text-domain+ :locale (cl-i18n:find-locale))))

(defun init-db ()
  "Initialize the database"
  (db-utils:with-ready-database (:connect t)))

(defun change-folder ()
  "Change folder, used in requests of a command line switch"
  (let ((refresh-event (make-instance 'program-events:refresh-thread-windows-event
                                      :new-folder command-line:*start-folder*))
        (folder-exists-p (db:folder-exists-p command-line:*start-folder*)))
    (if folder-exists-p
        (program-events:push-event refresh-event)
        (ui:error-message (format nil
                                  (_ "Folder ~s does not exists")
                                  command-line:*start-folder*)))))

(defun change-timeline ()
  "Change timeline, used in requests of a command line switch"
  (let* ((refresh-event (make-instance 'program-events:refresh-thread-windows-event
                                       :new-timeline command-line:*start-timeline*)))
    (program-events:push-event refresh-event)))

(defun reset-timeline-pagination ()
  (ui:reset-timeline-pagination))

(defun load-configuration-files ()
  (format t (_ "Loading configuration file ~a~%") swconf:+shared-conf-filename+)
  (handler-case
      (multiple-value-bind (x configuration-file-path)
          (swconf:load-config-file swconf:+shared-conf-filename+)
        (declare (ignore x))
        (swconf:perform-missing-value-check configuration-file-path))
    (error (e)
      (format *error-output* "~a~%" e)
      (os-utils:exit-program 1)))
  (handler-bind ((error
                   #'(lambda (e)
                     (format *error-output*
                             (_ "Non fatal error~%~a~%Tinmop will add an empty file for you. This file will be enough to use the program as a gemini client but to connect to pleroma the file must be properly filled.~%Consult the manpage ~a(1) for more details")
                             e
                             +program-name+)
                     (invoke-restart 'res:create-empty-in-home))))
    (swconf:load-config-file swconf:+conf-filename+)))

(defun shared-init ()
  (load-configuration-files)
  (init-db))

(defun init ()
  "Initialize the program"
  (shared-init)
  (db-utils:with-ready-database (:connect nil)
    (complete:initialize-complete-username-cache)
    (handler-case
        (modules:load-module +starting-init-file+)
      (error (e)
        (croatoan:end-screen)
        (format *error-output* "~a~%" e)
        (os-utils:exit-program 1)))
    ;; init main window for first...
    (main-window:init)
    (keybindings-window:init)
    (command-window:init)
    (thread-window:init)
    ;; the size  of message  and tag  window depends  from the  sizes of
    ;; thread-window  and  command window,  so  the  first two  must  be
    ;; initialized after the latter
    (message-window:init)
    (tags-window:init)
    (gemini-client:init-default-gemini-theme specials:*message-window*)
    (conversations-window:init)
    (setup-bindings)
    ;; ... and init-keyboard-mapping-for last
    (keybindings:init-keyboard-mapping)
    (ui:focus-to-thread-window)
    ;; now init the client
    (client:init)
    (client:authorize)
    (when command-line:*module-file*
      (handler-case
          (modules:load-module command-line:*module-file*)
        (error ()
          (ui:notify (format nil
                             (_ "Unable to load module ~a")
                             command-line:*module-file*)
                     :as-error t))))
    (if command-line:*gemini-url*
        (gemini-viewer:load-gemini-url command-line:*gemini-url*)
        (progn
          (let ((program-events:*process-events-immediately* t))
            (when command-line:*start-timeline*
              (change-timeline))
            (when command-line:*start-folder*
              (change-folder)))
          (when command-line:*reset-timeline-pagination*
            (reset-timeline-pagination))
          (when command-line:*update-timeline*
            (ui:update-current-timeline))
          (when command-line:*check-follow-requests*
            (ui:start-follow-request-processing))))))

(defun run (draw-welcome-string)
  (windows:with-croatoan-window (croatoan-window specials:*main-window*)
    (setf (frame-rate croatoan-window) +fps+)
    (db-utils:with-ready-database (:connect nil)
      (unwind-protect
           (progn
             (when draw-welcome-string
               (ui:show-welcome-window))
             (hooks:run-hooks 'hooks:*before-main-loop*)
             (run-event-loop croatoan-window))
        (end-screen)))))

(defun load-script-file ()
  "Load (execute) a lisp file used in requests of a command line switch"
  (setf program-events:*process-events-immediately* t)
  (shared-init)
  (db-utils:with-ready-database (:connect nil)
    (client:init)
    (client:authorize)
    (load command-line:*script-file* :verbose nil :print nil)))

(defun main ()
  "The entry point function of the program"
  (let ((first-time-starting (not (db-utils:db-file-exists-p))))
    (init-i18n)
    (res:init)
    (command-line:manage-opts)
    (if command-line:*script-file*
        (load-script-file)
        (let ((croatoan::*debugger-hook* #'(lambda (c h)
                                             (declare (ignore h))
                                             (end-screen)
                                             (print c))))
          (init)
          (run first-time-starting)))))
