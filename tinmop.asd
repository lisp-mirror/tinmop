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

(defsystem :tinmop
  :author      "cage"
  :license     "GPLv3"
  :version     "0.1.5.1"
  :pathname    "src"
  :serial      t
  :depends-on (:alexandria
               :cl-ppcre
               :tooter
               :croatoan
               :osicat
               :cl-spark
               :access
               :sqlite
               :sxql
               :sxql-composer
               :marshal
               :bordeaux-threads
               :cl-base64
               :cl+ssl
               :log4cl
               :local-time
               :cl-colors2
               :cl-i18n
               :clunit2
               :esrap
               :ieee-floats
               :parse-number
               :cl-html5-parser
               :unix-opts
               :crypto-shortcuts
               :drakma
               :usocket
               :babel
               :quri
               :percent-encoding
               :uiop)
  :components ((:file "package")
               (:file "config")
               (:file "constants")
               (:file "conditions")
               (:file "num-utils")
               (:file "misc-utils")
               (:file "box")
               (:file "filesystem-utils")
               (:file "os-utils")
               (:file "text-utils")
               (:file "html-utils")
               (:file "crypto-utils")
               (:file "resources-utils")
               (:file "interfaces")
               (:file "mtree-utils")
               (:file "bs-tree")
               (:file "rb-tree")
               (:file "priority-queue")
               (:file "queue")
               (:file "stack")
               (:file "x509-ffi")
               (:file "x509")
               (:file "db-utils")
               (:file "db")
               (:file "date-formatter")
               (:file "emoji-shortcodes")
               (:file "software-configuration")
               (:file "tui-utils")
               (:module gemini
                        :components ((:file "package")
                                     (:file "gemini-constants")
                                     (:file "gemini-parser")
                                     (:file "client")))
               (:file "command-line")
               (:file "specials")
               (:file "keybindings")
               (:file "complete")
               (:file "program-events")
               (:file "api-pleroma")
               (:file "api-client")
               (:file "hooks")
               (:file "windows")
               (:file "notify-window")
               (:file "suggestions-window")
               (:file "complete-window")
               (:file "keybindings-window")
               (:file "point-tracker")
               (:file "modeline-window")
               (:file "line-oriented-window")
               (:file "message-rendering-utils")
               (:file "thread-window")
               (:file "message-window")
               (:file "open-attach-window")
               (:file "open-message-link-window")
               (:file "command-window")
               (:file "sending-message")
               (:file "follow-requests")
               (:file "tags-window")
               (:file "conversations-window")
               (:file "gemini-viewer")
               (:file "main-window")
               (:file "ui-goodies")
               (:file "modules")
               (:file "main")
               (:module tests
                        :components ((:file "package")
                                     (:file "all-tests")
                                     (:file "misc-tests")
                                     (:file "box-tests")
                                     (:file "numeric-tests")
                                     (:file "text-utils-tests")
                                     (:file "mtree-tests")
                                     (:file "thread-window-tests")
                                     (:file "gemini-parser-tests")
                                     (:file "program-events-tests")))))

;; (push :debug-mode *features*)
