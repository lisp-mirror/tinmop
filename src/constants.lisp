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

(in-package :constants)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun actual-program-name ()
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (if (string= +program-name+ "tinmop")
        "tinmop"
        (format nil "~a (original name: \"tinmop\")" +program-name+))))

(define-constant +help-about-message+
    (format nil
            "~a
Copyright (C) 2019  cage

This program is free software:  you can redistribute it and/or modify
it under the terms of the  GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is  distributed in the hope that it  will be useful, but
WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.

For bug report please point your browser to:

~a"
            (actual-program-name)
            +issue-tracker+)
  :test #'string=)

(define-constant +welcome-message+
  (format nil
 "
                        _______
                       /_  __(_)___  ____ ___  ____  ____
                        / / / / __ \\/ __ `__ \\/ __ \\/ __ \\
                       / / / / / / / / / / / / /_/ / /_/ /
                      /_/ /_/_/ /_/_/ /_/ /_/\\____/ .___/
                                                 /_/


Welcome to ~a! A gemini, pleroma and kami client.

- for available keychords (sequence of keys to fire a command) help type '?';

- for searching in the help type: 'C-h a' (control and 'h' keys toghether, then 'a');
  [if this keychords  does not works, ask to your system administrator]

- documentation is available in man format. Type 'man tinmop' at shell's prompt
  or 'C-h m';

- for general information about this program (including license) type 'C-a';

- if you need more help or for bug report: ~a
  (collaborations are welcome too!).

Some useful keybinding to start (manpage contains the full list):

· gemini
  - '>' open URL (also kami is supported)
  - 'l' open link window
  - 'b' go back in history of visited URLs
  - '!' search geminispace (contacts gemini://geminispace.info/search)
· pleroma
  - 'C-t u' update current timeline
  - 'C-t c' change timeline

· kamid
  - 'newline' open node
  - 'e'       edit node
  - 'd'       download file
  - 'u'       upload file

Enjoy!

This program is  distributed in the hope that it  will be useful, but
WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
General Public License for more details."
          (actual-program-name)
          +issue-tracker+)

  :test #'string=)

(define-constant +http-code-ok+               200             :test #'=)

(define-constant +mime-type-jpg+                "image/jpeg"  :test #'string=)

(define-constant +mime-type-png+                "image/png"   :test #'string=)

(define-constant +mime-type-html+               "text/html"   :test #'string=)

(define-constant +db-file+                      "db.sqlite3"  :test #'string=
                 :documentation "the filename of the database")

(define-constant +json-true+                    "true"        :test #'string=)

(define-constant +json-false+                   "false"       :test #'string=)

(define-constant +fps+                         60             :test #'=
                 :documentation "The redraw frequency in frame per second")

(define-constant +command-window-height+        1             :test #'=)

(define-constant +starting-init-file+           "init.lisp"   :test #'string=)

(define-constant +box-height-diff+              3             :test #'=
                 :documentation "When fitting columns of text in a box
                 remove this rows from total height")

(define-constant +default-command-prompt+       "> "          :test #'string=)

(define-constant +menu-button-ok+               "OK"          :test #'string=)

(define-constant +status-public-visibility+     "public"      :test #'string=)

(define-constant +status-direct-visibility+     "direct"      :test #'string=)

(define-constant +folder-tag-prefix+            "#"           :test #'string=
                 :documentation "The prefix for messages hashtag")

(define-constant +folder-direct-message-prefix+ "@"           :test #'string=
                 :documentation "The prefix for direct message folder, unused")

(define-constant +mention-prefix+               "@"           :test #'string=
                 :documentation "The prefix for a mention in a message")

(define-constant +cache-tls-certificate-type+   "certificate" :test #'string=
                 :documentation "The cache type for TLS certificate")

(define-constant +standard-editor+              "ed"          :test #'string=
                 :documentation "Ed is the standard editor!")
