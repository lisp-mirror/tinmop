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

(in-package :specials)

(defparameter *main-window*                     nil
  "The main window of the program. AKA the screen")

(defparameter *keybindings-suggestions-window*  nil
  "The window to show suggestions for keybindings.")

(defparameter *strings-suggestions-window*      nil
  "The window to show suggestions for keybindings.")

(defparameter *command-window*                  nil
  "The window to deal with user key input.")

(defparameter *thread-window*                   nil
  "The threaded messages window.")

(defparameter *message-window*                  nil
  "The window where a single message is rendered.")

(defparameter *send-message-window*             nil
  "The window shown to confirm sending a new message.")

(defparameter *follow-requests-window*          nil
  "The window shown to accept follow requests.")

(defparameter *tags-window*                     nil
  "The window shown to manage tags subscriptions.")

(defparameter *conversations-window*            nil
  "The window that shows conversations.")

(defparameter *open-attach-window*              nil
  "The window that shows attachments for a message.")

(defparameter *open-message-link-window*        nil
  "The window that shows links in a message.")

(defparameter *gemini-streams-window*           nil
  "The window that shows all gemini-streams.")

(defparameter *gemini-certificates-window*      nil
  "The window that shows all gemini client certificates.")

(defparameter *gemini-subscription-window*      nil
  "The window that shows all the subscribed gemlogs.")

(defparameter *gemini-toc-window*      nil
  "The window that shows the table of contents of a gemini page.")

(defparameter *chats-list-window*               nil
  "The window that shows all the chats.")
