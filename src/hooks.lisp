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

(in-package :hooks)

(defvar *hook* nil
  "The hook currently being run.")

(defgeneric add-hook (hook fn &key append)
  (:documentation "Add FN to the value of HOOK.")
  (:method ((hook symbol) fn &key append)
    (declare (type (or function symbol) fn))
    (if (not append)
        (pushnew fn (symbol-value hook))
        (unless (member fn (symbol-value hook))
          (appendf (symbol-value hook) (list fn))))))

(defgeneric remove-hook (hook fn)
  (:documentation "Remove FN from the symbol value of HOOK.")
  (:method ((hook symbol) fn)
    (removef (symbol-value hook) fn)))

(defmacro with-hook-restart (&body body)
  `(with-simple-restart (continue "Call next function in hook ~s" *hook*)
     ,@body))

(defun run-hooks (&rest hooks)
  "Run all the hooks in HOOKS, without arguments.
The variable `*hook*' is bound to the name of each hook as it is being
run."
  (dolist (*hook* hooks)
    (run-hook *hook*)))

(defgeneric run-hook (hook &rest args)
  (:documentation "Apply each function in HOOK to ARGS.")
  (:method ((*hook* symbol) &rest args)
    (dolist (fn (symbol-value *hook*))
      (with-hook-restart
        (apply fn args)))))

(defgeneric run-hook-compose (hook args)
  (:documentation "Apply first function in HOOK to ARGS, second hook
to the results of first function applied and so on,
returns the results of af the last hook.")
  (:method ((*hook* symbol) args)
    (let ((results args))
      (dolist (fn (symbol-value *hook*))
        (with-hook-restart
          (setf results (funcall fn results))))
      results)))

(defgeneric run-hook-until-failure (hook &rest args)
  (:documentation "Like `run-hook-with-args', but quit once a function returns nil.")
  (:method ((*hook* symbol) &rest args)
    (loop
       for fn in (symbol-value *hook*)
       always (apply fn args))))

(defgeneric run-hook-until-success (hook &rest args)
  (:documentation "Like `run-hook-with-args', but quit once a function returns
non-nil.")
  (:method ((*hook* symbol) &rest args)
    (loop
       for fn in (symbol-value *hook*)
       thereis (apply fn args))))

(defparameter *before-main-loop* ()
  "run this hooks before UI main loop starts")

(defparameter *before-quit* ()
  "Run this hooks just before  closing the database connection and the
  program")

(defparameter *before-rendering-message-text* '()
  "Run   this    hooks   before    rendering   the   message    on   a
  message-window (the message window is passed as parameter")

(defparameter *before-sending-message* '()
  "Run this  hooks before sending  the message, note that  the message
  could  be encrypted  after this  hooks  runs, the  function takes  a
  message-window as parameter")

(defparameter *skip-message-hook* '()
  "Run this  hooks to check if a message must be skipped,
all hooks must returns nil for this message to be not skipped

Each function takes 4 parameters: status, timeline, folder, kind (:home :public)
localp")

(defparameter *after-saving-message* '()
  "Run this  hooks to a message just saved in the database.

Each function takes 1 parameter: the database row for the saved status.")

(defparameter *before-displaying-links-hook* '()
  "Run this hooks before sending the list of URLs to the window that allow the user to
open the links")

(defparameter *after-char-to-command-window* '()
  "Run this hooks after a character has been typed by the user on the command window.")

(defparameter *before-fire-string-event-command-window* '()
  "Run this hooks before sending user input to the program (command window only).")

(defparameter *after-delete-char-from-command-window* '()
  "Run this hooks after deleting a character from the input of the command-window.")
