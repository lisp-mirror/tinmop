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
   :+gopher-scheme+
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
   :+line-type-gif-image-file+
   :+line-type-image-file+
   :+line-type-info+
   :+line-type-uri+
   :line-type-id
   :selector
   :username
   :port
   :host
   :line-file
   :line-dir
   :line-cso
   :line-error
   :line-mac-hex-file
   :line-dos-archive-file
   :line-uuencoded-file
   :line-index-search
   :line-telnet-session
   :line-binary-file
   :line-redundant-server
   :line-tn3270-session
   :line-gif-file
   :line-image-file
   :line-info
   :line-uri
   :line-unknown
   :line-type-file-p
   :line-type-info-p
   :line-type-dir-p
   :line-type-cso-p
   :line-type-error-p
   :line-type-mac-hex-file-p
   :line-type-dos-archive-file-p
   :line-type-uuencoded-file-p
   :line-type-index-search-p
   :line-type-telnet-session-p
   :line-type-binary-file-p
   :line-type-redundant-server-p
   :line-type-tn3270-session-p
   :line-type-gif-file-p
   :line-type-image-file-p
   :line-type-image-uri-p
   :line-unknown-p
   :parse-menu
   :parse-text-file
   :parse-iri))

(defpackage gopher-client
  (:use
   :cl
   :cl-ppcre
   :config
   :constants
   :text-utils
   :misc
   :gopher-parser)
  (:local-nicknames (:a :alexandria)
                    (:parser :gopher-parser))
  (:export
   :make-collect-fn
   :request
   :request-from-iri))
