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
   :iri-parser
   :misc
   :alexandria
   :gemini-constants)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :+gemini-scheme+
   :+preformatted-prefix+
   :+max-header-level+
   :*pre-group-id*
   :*pre-alt-text*
   :*header-group-id*
   :*source-line-id*
   :geminize-h1
   :geminize-h2
   :geminize-h3
   :geminize-list
   :geminize-quote
   :geminize-link
   :geminize-preformatted
   :render-gemini-link
   :gemini-link
   :target
   :name
   :make-gemini-link
   :path-last-dir
   :gemini-response
   :status-code
   :meta
   :parse-gemini-file
   :absolutize-link
   :make-gemini-iri
   :sexp->links
   :gemini-page-theme
   :link-prefix-gemini
   :link-prefix-other
   :link-prefix-http
   :h1-prefix
   :h2-prefix
   :h3-prefix
   :quote-prefix
   :bullet-prefix
   :source-line
   :source-line-id
   :artificialp
   :artificial
   :with-lines
   :with-pre-group-id
   :pre-group-id
   :pre-start
   :value
   :pre-line
   :group-id
   :lines
   :alt-text
   :pre-end
   :quoted-lines
   :lines
   :vertical-space
   :header-line
   :level
   :unordered-list-line
   :link-line
   :link-text
   :simple-line
   :text-line
   :sexp->text-rows
   :sexp->text
   :parse-gemini-response-header
   :gemini-iri-p
   :gemini-first-h1
   :with-initialized-parser))

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
   :mime-text-stream
   :gemini-protocol-error
   :error-code
   :error-description
   :gemini-tofu-error
   :*gemini-page-theme*
   :make-gemini-file-response
   :host
   :response-certificate-requested-p
   :response-input-p
   :response-sensitive-input-p
   :response-redirect-p
   :response-success-p
   :absolute-gemini-url-p
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
   :displace-iri
   :close-ssl-socket
   :make-client-certificate
   :debug-gemini
   :open-tls-socket
   :request
   :gemini-file-stream-p
   :text-file-stream-p
   :request-dispatch
   :with-request-dispatch-table
   :fetch-cached-certificate
   :build-redirect-iri
   :slurp-gemini-url))

(defpackage :gemini-subscription
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :config
   :constants
   :text-utils
   :misc
   :alexandria
   :gemini-constants
   :gemini-parser
   :gemini-client)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :subscribe
   :refresh))
