;; tinmop: an humble gemini and pleroma client
;; Copyright (C) 2021  cage

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

(in-package :gempub)

(defrule title         "title"         (:constant :title))
(defrule gpubVersion   "gpubVersion"   (:constant :gpubVersion))
(defrule index         "index"         (:constant :index))
(defrule author        "author"        (:constant :author))
(defrule language      "language"      (:constant :language))
(defrule charset       "charset"       (:constant :charset))
(defrule description   "description"   (:constant :description))
(defrule published     "published"     (:constant :published))
(defrule publishDate   "publishDate"   (:constant :publishDate))
(defrule revisionDate  "revisionDate"  (:constant :revisionDate))
(defrule copyright     "copyright"     (:constant :copyright))
(defrule license       "license"       (:constant :license))
(defrule version       "version"       (:constant :version))
(defrule cover         "cover"         (:constant :cover))

(defrule blank (or #\space #\Newline #\Tab)
  (:constant nil))

(defrule blanks (* blank)
  (:constant nil))

(defrule key-value-separator #\:)

(defrule value (+ (not #\Newline))
  (:text t))

(defrule key (or title
                 gpubVersion
                 index
                 author
                 language
                 charset
                 description
                 published
                 publishDate
                 revisionDate
                 copyright
                 license
                 version
                 cover))

(defrule entry (and key (? blanks) key-value-separator (? blanks) value blanks)
  (:function (lambda (a)  (list (first a) (fifth a)))))

(defrule metadata (* entry)
  (:function flatten))

(defgeneric parse-metadata (object))

(defmethod parse-metadata ((object string))
  (parse 'metadata object))

(define-constant +metadata-entry-name "metadata.txt" :test #'string=)

(defun extract-metadata (zip-file)
  (when (zip-info:zip-file-p zip-file)
    (let ((entries (zip-info:list-entries zip-file)))
      (when (find +metadata-entry-name entries :test #'String=)
        (when-let ((metadata-raw (os-utils:unzip-single-file zip-file
                                                             +metadata-entry-name)))
          (parse 'metadata metadata-raw))))))

(defun save-metadata (zip-file)
  (when-let ((metadata (extract-metadata zip-file)))
    (db:gempub-metadata-add zip-file
                            nil
                            (getf metadata :title)
                            (getf metadata :gpubVersion)
                            (getf metadata :index)
                            (getf metadata :author)
                            (getf metadata :language)
                            (getf metadata :charset)
                            (getf metadata :description)
                            (getf metadata :published)
                            (getf metadata :publishDate)
                            (getf metadata :revisionDate)
                            (getf metadata :copyright)
                            (getf metadata :license)
                            (getf metadata :version)
                            (getf metadata :cover))))

(defun sync-library (&key (notify nil))
  (let ((all-known        (db:all-gempub-metadata))
        (all-gempub-files (remove-if-not (lambda (a) (ignore-errors (zip-info:zip-file-p a)))
                                         (fs:collect-files/dirs (swconf:gempub-library-directory))))
        (removed-known    '())
        (added-file       '()))
    (loop for known in all-known do
      (let ((local-uri (db:row-local-uri known)))
        (when (not (and (fs:file-exists-p    local-uri)
                        (zip-info:zip-file-p local-uri)))
          (push local-uri removed-known)
          (db:gempub-metadata-delete local-uri))))
    (loop for gempub-file in all-gempub-files do
      (when (not (db:gempub-metadata-find gempub-file))
        (push gempub-file added-file)
        (save-metadata gempub-file)))
    (when notify
      (loop for removed in removed-known do
        (ui:notify (format nil (_ "Removed gempub ~s from library, missing file") removed)))
      (loop for added in added-file do
        (ui:notify (format nil (_ "Added gempub ~s into the library") added))))))
