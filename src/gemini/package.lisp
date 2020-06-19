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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :gemini-parser
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :esrap
   :config
   :constants
   :text-utils
   :misc
   :alexandria)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :gemini-link
   :target
   :name
   :gemini-response
   :status-code
   :meta
   :parse-gemini-file
   :sexp->links
   :sexp->text
   :parse-gemini-response-header))

(defpackage :gemini-client
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :esrap
   :config
   :constants
   :text-utils
   :misc
   :alexandria
   :gemini-parser)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :gemini-protocol-error
   :error-code
   :error-description
   :request))
