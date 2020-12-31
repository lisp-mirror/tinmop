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
the event  that is fired  wnen no input  from user (key  pressed mouse
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
  (db-utils:with-ready-database (:connect t)
    (db:purge-history)))

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

(defun load-gemini-url (url)
  (let* ((event (make-instance 'program-events:gemini-request-event
                                       :url url)))
    (program-events:push-event event)))

(defun load-configuration-files ()
  (swconf:load-config-file swconf:+shared-conf-filename+)
  (swconf:load-config-file swconf:+conf-filename+))

(defun init ()
  "Initialize the program"
  (res:init)
  (load-configuration-files)
  (init-db)
  (gemini-client:init-default-gemini-theme)
  (db-utils:with-ready-database (:connect nil)
    (complete:initialize-complete-username-cache)
    (modules:load-module +starting-init-file+)
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
    (conversations-window:init)
    (setup-bindings)
    ;; ... and init-keyboard-mapping-for last
    (keybindings:init-keyboard-mapping)
    (ui:focus-to-thread-window)
    ;; now init the client
    (client:init)
    (client:authorize)
    (if command-line:*gemini-url*
        (load-gemini-url command-line:*gemini-url*)
        (progn
          (when command-line:*module-file*
            (modules:load-module command-line:*module-file*))
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

(defun run ()
  (windows:with-croatoan-window (croatoan-window specials:*main-window*)
    (setf (frame-rate croatoan-window) +fps+)
    (db-utils:with-ready-database (:connect nil)
      (unwind-protect
           (progn
             (hooks:run-hooks 'hooks:*before-main-loop*)
             (run-event-loop croatoan-window))
        (end-screen)))))

(defun load-script-file ()
  "Load (execute) a lisp file used in requests of a command line switch"
  (setf program-events:*process-events-immediately* t)
  (load-configuration-files)
  (init-db)
  (db-utils:with-ready-database (:connect nil)
    (client:init)
    (client:authorize)
    (load command-line:*script-file* :verbose nil :print nil)))

(defun main ()
  "The entry point function of the program"
  (init-i18n)
  (command-line:manage-opts)
  (if command-line:*script-file*
      (load-script-file)
      (let ((croatoan::*debugger-hook* #'(lambda (c h)
                                           (declare (ignore h))
                                           (end-screen)
                                           (print c))))
        (init)
        (run))))
