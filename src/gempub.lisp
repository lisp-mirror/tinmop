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

(defrule spaces (+ blank)
  (:constant nil))

(defrule column (or "title"
                    "author"
                    "language"
                    "description"
                    "publish-date"
                    "revision-date"
                    "copyright")
  (:text t))

(defrule column-value (and #\" (+ (not #\")) #\")
  (:text t))

(defrule term (or and-where
                  or-where
                  like)
  (:function (lambda (a) (join-with-strings a " "))))

(defrule like (and column spaces "like" spaces column-value)
  (:function (lambda (a) (format nil
                                 "~a like \"%~a%\""
                                 (first a)
                                 (string-trim '(#\") (fifth a))))))

(defrule and-where (and term spaces "and" spaces term))

(defrule or-where (and term spaces "or" spaces term))

(defrule where-clause (and "where" spaces (+ term))
  (:function (lambda (a) (strcat "where " (join-with-strings (third a) " ")))))

(defun parse-search-gempub (query)
  (let* ((where-clause (when (string-not-empty-p query)
                         (parse 'where-clause query)))
         (sql-query    (if where-clause
                           (strcat (format nil
                                           "select * from \"~a\" ~a"
                                           db::+table-gempub-metadata+
                                           where-clause))
                           (format nil "select * from \"~a\"" db::+table-gempub-metadata+))))
    (db-utils:query-low-level sql-query)))

(defclass gempub-library-window (focus-marked-window
                                 simple-line-navigation-window
                                 title-window
                                 border-window)
  ((query-rows
    :initform '()
    :initarg  :query-rows
    :accessor query-rows)))

(defmethod refresh-config :after ((object gempub-library-window))
  (open-attach-window:refresh-view-links-window-config object
                                                       swconf:+key-gempub-library-window+)
  (refresh-config-sizes object swconf:+key-thread-window+)
  (win-move object
            (- (win-width *main-window*)
               (win-width object))
            0)
  (win-move object
            (- (win-width *main-window*)
               (win-width object))
            0)
  object)

(defun row->list-item (row)
  (join-with-strings* " "
                      (db:row-title     row)
                      (db:row-author    row)
                      (db:row-published row)))

(defun row->unselected-list-item (row)
  (row->list-item row))

(defun row->selected-list-item (row)
  (row->list-item row))

(defmethod resync-rows-db ((object gempub-library-window)
                           &key
                             (redraw t)
                             (suggested-message-index 0))
  (with-accessors ((rows             rows)
                   (selected-line-bg selected-line-bg)
                   (selected-line-fg selected-line-fg)
                   (query-rows       query-rows)) object
    (flet ((make-rows (rows bg fg)
             (mapcar (lambda (row)
                       (make-instance 'line
                                      :normal-text   (row->unselected-list-item row)
                                      :selected-text (row->selected-list-item   row)
                                      :fields        row
                                      :normal-bg     fg
                                      :normal-fg     bg
                                      :selected-bg   bg
                                      :selected-fg   fg))
                     rows)))
      (with-croatoan-window (croatoan-window object)
        (line-oriented-window:update-all-rows object
                                              (make-rows query-rows
                                                         selected-line-bg
                                                         selected-line-fg))
        (when suggested-message-index
          (handler-bind ((conditions:out-of-bounds
                          (lambda (e)
                            (invoke-restart 'ignore-selecting-action e))))
            (select-row object suggested-message-index)))
        (when redraw
          (win-clear object)
          (draw object))))))

(defun open-gempub-library-window (query)
  (let* ((low-level-window (tui:make-croatoan-window :enable-function-keys t)))
    (setf *gempub-library-window*
          (make-instance 'gempub-library-window
                         :query-rows        (parse-search-gempub query)
                         :top-row-padding   0
                         :title             (_ "Gempub library")
                         :single-row-height 1
                         :uses-border-p     t
                         :keybindings       keybindings:*gempub-library-keymap*
                         :croatoan-window   low-level-window))
    (refresh-config  *gempub-library-window*)
    (resync-rows-db  *gempub-library-window* :redraw nil)
    (when (not (line-oriented-window:rows-empty-p *gempub-library-window*))
      (line-oriented-window:select-row  *gempub-library-window* 0))
    (draw  *gempub-library-window*)
    *gempub-library-window*))
