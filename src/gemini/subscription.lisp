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

(in-package :gemini-subscription)

(define-constant +maximum-redirections+ 5 :test `=)

(defun slurp-gemini-url (url &optional (redirect-count 0))
  "Read 'full'  data from gemini  address `url'; note that  specs says
that gemini flow  is streamed by default so this  function has limited
use as there is a chance that  it would not returns. Anyway for gemlog
subscription (for example) could be used.

TODO: Add client certificate."
  (let ((iri         (iri:iri-parse url)))
    (multiple-value-bind (status description meta response socket)
        (gemini-client:request (uri:host iri)
                               (uri:path iri)
                               :query    (uri:query    iri)
                               :port     (uri:port     iri)
                               :fragment (uri:fragment iri))
      (declare (ignore description))
      (cond
        ((response-success-p status)
         (let ((data (misc:make-fresh-array 0 0 '(unsigned-byte 8) nil)))
           (loop for new-byte = (read-byte response nil nil)
                 while new-byte do
                   (vector-push-extend new-byte data))
           (gemini-client:close-ssl-socket socket)
           data))
        ((and (response-redirect-p status)
              (< redirect-count +maximum-redirections+))
         (slurp-gemini-url (gemini-client:build-redirect-iri meta iri) (1+ redirect-count)))))))

(defun link-post-timestamp (link-text)
  "Returns a local-time object parsing a gemlog entry's link text

A link text entry is like 'aaaa-mm-dd post title'

This function parses the 'aaaa-mm-dd' part.
"
  (when (>= (length link-text) 10)
    (local-time:parse-timestring link-text :start 0 :end 10 :fail-on-error nil)))

(defun link-post-title (link-text)
  "Returns the title of gemlog entry from link text

A link text entry is like 'aaaa-mm-dd post title'

This function return the 'post-title' substring."
  (misc:safe-subseq link-text 10))

(defun link-post-timestamp-p (link-text)
  "Is this a valid gemlog link text?"
  (link-post-timestamp link-text))

(defun subscribe (url)
  "Subscribe to a gemlog that can be found at 'url'"
  (labels ((subtitle-p (nodes h2-pos)
             (when h2-pos
               (let ((res t))
                 (loop for i from h2-pos downto 0 do
                   (let ((node (elt nodes i)))
                     (when (and node
                                (not (html-utils:tag= :h1
                                                      (html-utils:children node))))
                       (return-from subtitle-p nil))))
                 res))))
    (when-let* ((data   (slurp-gemini-url url))
                (page   (babel:octets-to-string data))
                (parsed (parse-gemini-file page))
                (iri    (iri:iri-parse url))
                (title  (first (html-utils:children (html-utils:find-tag :h1
                                                                         parsed)))))
      (let* ((maybe-subtitle-pos (html-utils:position-tag :h2 parsed))
             (subtitle           (when (subtitle-p parsed maybe-subtitle-pos)
                                   (first (html-utils:children (elt parsed
                                                                    maybe-subtitle-pos))))))
        (when (not (db:gemini-find-subscription url))
          (db:gemini-subscribe-url url title subtitle))
        t))))

(defun refresh (url)
  "Refresh gemlog entries that can be  found at 'url'. The gemlog must
be subscribed before (see: 'gemini-subscription:subcribe'"
  (when-let* ((data       (slurp-gemini-url url))
              (page       (babel:octets-to-string data))
              (parsed     (parse-gemini-file page))
              (gemlog-iri (iri:iri-parse url)))
    (let ((links (remove-if-not (lambda (a) (link-post-timestamp-p (name a)))
                                (sexp->links parsed
                                             (uri:host gemlog-iri)
                                             (uri:port gemlog-iri)
                                             (uri:path gemlog-iri)))))
      (loop for link in links do
        (when (not (db:find-gemlog-entry (to-s (target link))))
          (let ((date (link-post-timestamp (name link))))
            (db:add-gemlog-entries (to-s gemlog-iri)
                                   (target link)
                                   (link-post-title (name link))
                                   date
                                   nil)))))))
