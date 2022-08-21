;; tinmop: an humble gemini kami and pleroma client
;; Copyright © 2022  cage

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

(defpackage gopher-parser
  (:use
   :cl
   :cl-ppcre
   :esrap
   :config
   :constants
   :text-utils
   :misc)
  (:local-nicknames (:a :alexandria))
  (:export
   :+line-type-file+
   :+line-type-dir+
   :+line-type-cso+
   :+line-type-error+
   :+line-type-mac-hex-file+
   :+line-type-dos-archive-file+
   :+line-type-uuencoded-file+
   :+line-type-index-search+
   :+line-type-telnet-session+
   :+line-type-binary-file+
   :+gopher-scheme+
   :line-file
   :line-dir
   :line-cso
   :line-error
   :line-mac-hex-file
   :line-dos-archive-file
   :line-dos-uuencoded-file
   :line-index-search
   :line-telnet-session
   :line-binary-file
   :line-redundant-server
   :line-tn3270-session
   :line-gif-file
   :line-image-file
   :line-file-p
   :line-dir-p
   :line-cso-p
   :line-error-p
   :line-mac-hex-file-p
   :line-dos-archive-file-p
   :line-uuencoded-file-p
   :line-index-search-p
   :line-telnet-session-p
   :line-binary-file-p
   :line-redundant-server-p
   :line-tn3270-session-p
   :line-gif-file-p
   :line-image-file-p
   :parse-menu))

(defpackage gopher-client
  (:use
   :cl
   :cl-ppcre
   :esrap
   :config
   :constants
   :text-utils
   :misc
   :gemini-constants)
   (:local-nicknames (:a :alexandria))
   (:export
    :+gopher-scheme+))
