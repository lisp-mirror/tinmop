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

(defpackage :gemini-constants
  (:use
   :cl
   :alexandria)
  (:export
   :+gemini-scheme+
   :+gemini-default-port+))

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
   :alexandria
   :gemini-constants)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :+gemini-scheme+
   :gemini-link
   :target
   :name
   :gemini-response
   :status-code
   :meta
   :parse-gemini-file
   :absolutize-link
   :make-gemini-uri
   :sexp->links
   :gemini-page-theme
   :link-prefix-gemini
   :link-prefix-other
   :h1-prefix
   :h2-prefix
   :h3-prefix
   :quote-prefix
   :bullet-prefix
   :sexp->text
   :parse-gemini-response-header
   :gemini-uri-p))

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
   :gemini-constants
   :gemini-parser)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :+gemini-default-port+
   :response-status-code
   :code
   :description
   :find-code-description
   :find-code-class
   :mime-gemini-p
   :gemini-protocol-error
   :error-code
   :error-description
   :gemini-tofu-error
   :make-gemini-file-response
   :host
   :response-certificate-requested-p
   :response-input-p
   :response-sensitive-input-p
   :response-redirect-p
   :absolute-url-p
   :init-default-gemini-theme
   :gemini-file-response
   :status-code
   :status-code-message
   :meta
   :parsed-file
   :url-header
   :source-url
   :source
   :links
   :text-rendering-theme
   :gemini-file-response-p
   :close-ssl-socket
   :make-client-certificate
   :request))
