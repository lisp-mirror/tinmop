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

(in-package :thread-window)

(defclass thread-window (modeline-window
                         tree-holder
                         row-oriented-widget
                         focus-marked-window
                         title-window)
  ((favourite-text
    :initform "+1"
    :initarg  :favourite-text
    :accessor favourite-text)
   (boosted-text
    :initform "^"
    :initarg  :boosted-text
    :accessor boosted-text)
   (sensitive-text
    :initform "!"
    :initarg  :sensitive-text
    :accessor sensitive-text)
   (root-text
    :initform "*"
    :initarg  :root-text
    :accessor root-text)
   (favourite-text-off
    :initform "~+1"
    :initarg  :favourite-text-off
    :accessor favourite-text-off)
   (boosted-text-off
    :initform "~^"
    :initarg  :boosted-text-off
    :accessor boosted-text-off)
   (sensitive-text-off
    :initform "!"
    :initarg  :sensitive-text-off
    :accessor sensitive-text-off)
   (root-text-off
    :initform "*"
    :initarg  :root-text-off
    :accessor root-text-off)
   (read-fg
    :initform :black
    :initarg  :read-fg
    :accessor read-fg)
   (read-bg
    :initform :cyan
    :initarg  :read-bg
    :accessor read-bg)
   (read-attribute
    :initform :nil
    :initarg  :read-attribute
    :accessor read-attribute)
   (unread-fg
    :initform :black
    :initarg  :unread-fg
    :accessor unread-fg)
   (unread-bg
    :initform :cyan
    :initarg  :unread-bg
    :accessor unread-bg)
   (unread-attribute
    :initform :nil
    :initarg  :unread-attribute
    :accessor unread-attribute)
   (selected-fg
    :initform :black
    :initarg  :selected-fg
    :accessor selected-fg)
   (selected-bg
    :initform :cyan
    :initarg  :selected-bg
    :accessor selected-bg)
   (selected-attribute
    :initform :nil
    :initarg  :selected-attribute
    :accessor selected-attribute)
   (deleted-fg
    :initform :black
    :initarg  :deleted-fg
    :accessor deleted-fg)
   (deleted-bg
    :initform :cyan
    :initarg  :deleted-bg
    :accessor deleted-bg)
   (deleted-attribute
    :initform :nil
    :initarg  :deleted-attribute
    :accessor deleted-attribute)
   (date-format
    :initform '(:year " " :month " " :day " " :hour ":" :minute)
    :initarg  :date-format
    :accessor date-format)
   (timeline-type
    :initform db:+local-timeline+
    :initarg  :timeline-type
    :accessor timeline-type)
   (timeline-folder
    :initform db:+default-status-folder+
    :initarg  :timeline-folder
    :accessor timeline-folder)))

(defmacro lambda-ignore-args (args &body body)
  `(lambda (,@args)
     (declare (ignore ,@args))
     ,@body))

(defun make-tui-modeline-text (window text)
  (with-accessors ((modeline-fg modeline-fg)
                   (modeline-bg modeline-bg)) window
    (make-tui-string text :fgcolor modeline-fg :bgcolor modeline-bg)))

(defmacro with-tuify-results ((window) form)
  `(make-tui-modeline-text ,window ,form))

(defun modeline-bold-expand (window text)
  (with-accessors ((modeline-fg modeline-fg)
                   (modeline-bg modeline-bg)) window
    (make-tui-string text
                     :fgcolor    modeline-fg
                     :bgcolor    modeline-bg
                     :attributes (attribute-bold))))

(defun expand-timeline-type (window)
  (with-accessors ((timeline-type timeline-type)) window
    (let* ((actual-timeline     (if (string-not-empty-p timeline-type)
                                    timeline-type
                                    (_ "no timeline selected")))
          (timeline-description (db:timeline-type->description actual-timeline)))
      (modeline-bold-expand window timeline-description))))

(defun expand-folder-name (window)
  (with-accessors ((timeline-folder timeline-folder)) window
    (let ((folder (if (string-not-empty-p timeline-folder)
                      timeline-folder
                      (_ "no folder selected"))))
      (modeline-bold-expand window folder))))

(defun expand-message-hashtags (window)
  (when-let ((selected-row (selected-row window)))
    (with-tuify-results (window)
      (db-utils:db-getf (fields selected-row) :tags))))

(defun expand-total-messages (window)
  (with-accessors ((timeline-folder timeline-folder)
                   (timeline-type   timeline-type)) window
    (if (and timeline-folder
             timeline-type)
        (with-tuify-results (window)
          (to-s (db:count-status timeline-type
                                 timeline-folder
                                 :account-id nil)))
        "")))

(defun expand-redp-messages (window)
  (with-accessors ((timeline-folder timeline-folder)
                   (timeline-type   timeline-type)) window
    (if (and timeline-folder
             timeline-type)
        (with-tuify-results (window)
          (to-s (db:count-status-redp timeline-type
                                      timeline-folder
                                      :account-id nil)))
        "")))

(defun default-expander ()
  (list (cons "%" (lambda (w) (with-tuify-results (w) "%")))
        (cons "s" (lambda (w) (with-tuify-results (w) (swconf:config-server-name))))
        (cons "u" (lambda (w) (with-tuify-results (w) (swconf:config-username))))
        (cons "k" #'expand-timeline-type)
        (cons "f" #'expand-folder-name)
        (cons "h" #'expand-message-hashtags)
        (cons "t" #'expand-total-messages)
        (cons "r" #'expand-redp-messages)))

(defmethod initialize-instance :after ((object thread-window) &key &allow-other-keys)
  (with-accessors ((mapping-code->fn mapping-code->fn)) object
    (setf mapping-code->fn
          (default-expander))))

(defmethod refresh-config :after ((object thread-window))
  (refresh-config-colors    object swconf:+key-thread-window+)
  (refresh-config-sizes     object swconf:+key-thread-window+)
  (refresh-modeline-config  object swconf:+key-thread-window+)
  (with-accessors ((read-fg            read-fg)
                   (read-bg            read-bg)
                   (read-attribute     read-attribute)
                   (unread-fg          unread-fg)
                   (unread-bg          unread-bg)
                   (unread-attribute   unread-attribute)
                   (selected-fg        selected-fg)
                   (selected-bg        selected-bg)
                   (selected-attribute selected-attribute)
                   (deleted-fg         deleted-fg)
                   (deleted-bg         deleted-bg)
                   (deleted-attribute  deleted-attribute)
                   (favourite-text-off favourite-text-off)
                   (boosted-text-off   boosted-text-off)
                   (sensitive-text-off sensitive-text-off)
                   (date-format        date-format)) object
    (setf date-format (swconf:date-fmt swconf:+key-thread-window+))
    (multiple-value-bind (cfg-read-bg cfg-read-fg cfg-read-attribute)
        (swconf:thread-message-read-colors)
      (multiple-value-bind (cfg-unread-bg cfg-unread-fg cfg-unread-attribute)
          (swconf:thread-message-unread-colors)
        (multiple-value-bind (cfg-selected-bg cfg-selected-fg cfg-selected-attribute)
            (swconf:thread-message-selected-colors)
          (multiple-value-bind (cfg-deleted-bg cfg-deleted-fg cfg-deleted-attribute)
              (swconf:thread-message-deleted-colors)
            (setf read-fg             cfg-read-fg)
            (setf read-bg             cfg-read-bg)
            (setf read-attribute      cfg-read-attribute)
            (setf unread-fg           cfg-unread-fg)
            (setf unread-bg           cfg-unread-bg)
            (setf unread-attribute    cfg-unread-attribute)
            (setf selected-fg         cfg-selected-fg)
            (setf selected-bg         cfg-selected-bg)
            (setf selected-attribute  cfg-selected-attribute)
            (setf deleted-fg          cfg-deleted-fg)
            (setf deleted-bg          cfg-deleted-bg)
            (setf deleted-attribute   cfg-deleted-attribute)))))
    (flet ((set-symbol (slot-on slot-off key)
             (multiple-value-bind (value fg)
                 (swconf:thread-message-symbol key)
               (setf (slot-value object slot-on)
                     (make-tui-string value :fgcolor fg))
               (setf (slot-value object slot-off)
                     (make-tui-string value
                                      :fgcolor    fg
                                      :attributes (attribute-invisible))))))
      (set-symbol 'favourite-text 'favourite-text-off swconf:+key-favourite+)
      (set-symbol 'sensitive-text 'sensitive-text-off swconf:+key-sensitive+)
      (set-symbol 'boosted-text   'boosted-text-off   swconf:+key-boosted+)
      (set-symbol 'root-text      'root-text-off      swconf:+key-root+))
    (win-move object
              (- (win-width *main-window*)
                 (win-width object))
              0)))

(defmethod calculate ((object thread-window) dt))

(defun render-messages (window)
  (loop
     for message in (rows window)
     for y from 1                do
       (cond
         ((selectedp message)
          (print-text window (selected-text message) 1 y))
         ((marked-to-delete-p message)
          (print-text window (deleted-text message) 1 y))
         (t
          (print-text window (normal-text message) 1 y))))
  (expand-modeline-spec window)
  (win-refresh window))

(defmethod draw ((object thread-window))
  (when-window-shown (object)
    (win-clear object :redraw nil)
    (win-box   object)
    (render-messages object)
    (call-next-method)))

(defgeneric build-lines (object annotated-tree selected-pos))

(defgeneric go-message-down (object))

(defgeneric go-message-up (object))

(defgeneric goto-message (object message-index))

(defgeneric goto-first-message (object))

(defgeneric goto-last-message (object))

(defgeneric open-message (object))

(defgeneric mark-selected-message-to-delete (object &key move-down-selected-message))

(defgeneric mark-selected-message-prevent-delete (object &key move-down-selected-message))

(defgeneric search-next-message-body (object text-looking-for))

(defgeneric search-previous-message-body (object text-looking-for))

(defgeneric search-next-message-meta (object text-looking-for))

(defgeneric search-next-unread (object))

(defgeneric search-previous-message-meta (object text-looking-for))

(defun message-root (tree)
  (mtree:root-node tree))

(defun tree-lines (tree)
  (mtree:count-nodes tree))

(defun grow-tree-to-fit-window (timeline-type
                                folder
                                message-index
                                desired-window-position
                                window-height
                                &key
                                  (arrow-char      ">")
                                  (spacer-child    "-")
                                  (child-char      "+")
                                  (line-char       "|")
                                  (last-child-char "."))
  "Note: assumes that the message are  numbered in a BFS fashion, also
no gaps in numbering are allowed. (see mtree:tree->annotated-lines and
db:renumber-timeline-message-index."
  (labels ((tree-line->data-plist (line)
             (db:annotated-tree-line->data-plist line))
           (plist-message-index (data)
             (db-utils:db-getf data :message-index))
           (tree= (a b)
             (db:message-tree-root-equal a b))
           (tree->annotated-tree (tree)
             (tree->annotated-lines tree
                                    :arrow-char      arrow-char
                                    :spacer-child    spacer-child
                                    :child-char      child-char
                                    :line-char       line-char
                                    :last-child-char last-child-char
                                    :print-data      t
                                    :print-data-fn   #'identity))
           (slice-annotated-tree-lines (annotated-tree-lines
                                        starting-message-index
                                        ending-message-index)
             (flet ((line-pos (lines message-index)
                      (position-if (lambda (a)
                                     (let ((row (tree-line->data-plist a)))
                                       (= message-index
                                          (plist-message-index row))))
                                   lines)))
               (let* ((slice-start  (line-pos annotated-tree-lines starting-message-index))
                      (slice-end    (line-pos annotated-tree-lines ending-message-index))
                      (slice        (subseq annotated-tree-lines
                                            slice-start
                                            slice-end))
                      (selected-pos (line-pos slice message-index)))
                 (values slice selected-pos)))))
    (let ((starting-message-index (- message-index desired-window-position))
          (end-message-index      (+ message-index (- window-height
                                                      desired-window-position)))
          (trees                  ())
          (tree-lines             ()))
      (loop
         for scan-index from starting-message-index to end-message-index
         when (db:message-from-timeline-folder-message-index timeline-type
                                                             folder
                                                             scan-index)
         do
           (let ((tree (db:message-index->tree timeline-type folder scan-index)))
             (pushnew tree trees :test #'tree=)))
      (if (null trees)
          (values nil nil)
          (progn
            (reversef trees)
            (loop for tree in trees do
                 (setf tree-lines (lcat tree-lines (tree->annotated-tree tree))))
            (multiple-value-bind (fitted-lines selected-pos)
                (slice-annotated-tree-lines tree-lines
                                            starting-message-index
                                            end-message-index)
              (values fitted-lines selected-pos)))))))

(defun fit-timeline-to-window (window message-index)
   (with-accessors ((render-arrow-value         render-arrow-value)
                    (render-leaf-value          render-leaf-value)
                    (render-branch-value        render-branch-value)
                    (render-spacer-value        render-spacer-value)
                    (render-vertical-line-value render-vertical-line-value)
                    (timeline-folder            timeline-folder)
                    (timeline-type              timeline-type)) window
     (let* ((window-height                (win-height-no-border window))
            (message-sequence-index       (db:message-index->sequence-index message-index))
            (window-selected-row-position (rem message-sequence-index window-height)))
       (multiple-value-bind (annotated-tree selected-pos)
           (grow-tree-to-fit-window timeline-type
                                    timeline-folder
                                    message-index
                                    window-selected-row-position
                                    window-height
                                    :arrow-char      render-arrow-value
                                    :spacer-child    render-spacer-value
                                    :child-char      render-branch-value
                                    :line-char       render-vertical-line-value
                                    :last-child-char render-leaf-value)
         (values annotated-tree selected-pos)))))

(defun annotated-line->message-subject (line)
  (let* ((subject-placeholder (_ "Missing subject"))
         (data-element        (db:annotated-tree-line->data-plist line))
         (subject             (or (db:row-message-subject data-element)
                                  subject-placeholder)))
    #+debug-mode (join-with-strings* " " subject (db:row-message-status-id data-element))
    #-debug-mode subject))

(defmethod build-lines ((object list) annotated-tree selected-pos)
  (let* ((renderizable-tree (mapcar (lambda (line)
                                      (let* ((annotation   (annotated-text-symbol (last-elt line)))
                                             (new-line     (copy-list line))
                                             (subject      (annotated-line->message-subject line)))
                                        (setf (last-elt new-line)
                                              (cons annotation subject))
                                        new-line))
                                    annotated-tree))
         (rendered-tree-lines (mapcar (lambda (line)
                                        (reduce #'cat-tui-string
                                                (colorize-tree-line line object)
                                                :initial-value (make-tui-string "")))
                                      renderizable-tree))
         (fields              (mapcar (lambda (line)
                                        (rest  (last-elt line)))
                                      annotated-tree)))
    (values rendered-tree-lines fields)))

(defclass row-prefix ()
  ((author
    :initform nil
    :initarg  :author
    :accessor author)
   (creation-date
    :initform nil
    :initarg  :creation-date
    :accessor creation-date)
   (index
    :initform -1
    :initarg  :index
    :accessor index)
   (redp
    :initform nil
    :initarg  :redp
    :accessor redp)
   (deletedp
    :initform nil
    :initarg  :deletedp
    :accessor deletedp)
   (root-message-p
    :initform nil
    :initarg  :root-message-p
    :accessor root-message-p)))

(defun make-message-row-prefix (window fields index max-index max-author-length)
  (with-accessors ((favourite-text      favourite-text)
                   (favourite-text-off  favourite-text-off)
                   (boosted-text        boosted-text)
                   (boosted-text-off    boosted-text-off)
                   (sensitive-text      sensitive-text)
                   (sensitive-text-off  sensitive-text-off)
                   (date-format         date-format)) window
    (flet ((append-space (a)
             (strcat a " ")))
      (let* ((author              (db-utils:db-getf  fields :username))
             (created-at          (db-utils:db-getf  fields :created-at))
             (redp                (db-utils:db-getf  fields :redp))
             (deletedp            (db-utils:db-getf  fields :deletedp))
             (rootp               (not (db-utils:db-getf  fields :in-reply-to-id)))
             (encoded-date        (db-utils:encode-datetime-string created-at))
             (formatted-date      (append-space (format-time encoded-date date-format)))
             (padding-index-count (num:count-digit max-index))
             (padded-index        (append-space (text-utils:left-padding (to-s index)
                                                                         padding-index-count)))
             (padded-author       (append-space (text-utils:right-padding author
                                                                          max-author-length))))
        (make-instance 'row-prefix
                       :index          padded-index
                       :creation-date  formatted-date
                       :author         padded-author
                       :redp           redp
                       :deletedp       deletedp
                       :root-message-p rootp)))))

(defun pad-row-prefix (prefixes)
  (flet ((find-max (slot)
           (reduce (lambda (a b) (max a (length (slot-value b slot))))
                   prefixes
                   :initial-value -1))
         (pad (instance slot total-size)
           (setf (slot-value instance slot)
                 (right-padding (slot-value instance slot)
                                total-size))))
    (let ((max-index         (find-max 'index))
          (max-creation-date (find-max 'creation-date)))
      (loop for prefix in prefixes do
           (pad prefix 'index         max-index)
           (pad prefix 'creation-date max-creation-date))
      prefixes)))

(defun make-message-row (window rendered-tree-line row-message message-prefix-info)
  (with-accessors ((favourite-text     favourite-text)
                   (favourite-text-off favourite-text-off)
                   (boosted-text       boosted-text)
                   (boosted-text-off   boosted-text-off)
                   (sensitive-text     sensitive-text)
                   (sensitive-text-off sensitive-text-off)
                   (root-text          root-text)
                   (root-text-off      root-text-off)
                   (read-fg            read-fg)
                   (read-bg            read-bg)
                   (read-attribute     read-attribute)
                   (unread-fg          unread-fg)
                   (unread-bg          unread-bg)
                   (unread-attribute   unread-attribute)
                   (selected-fg        selected-fg)
                   (selected-bg        selected-bg)
                   (selected-attribute selected-attribute)
                   (deleted-fg         deleted-fg)
                   (deleted-bg         deleted-bg)
                   (deleted-attribute  deleted-attribute)
                   (date-format        date-format)) window
    (let* ((max-width            (win-width-no-border window))
           (fields               (fields row-message))
           (line-total-width     (win-width-no-border window))
           (favouritedp          (db-utils:db-getf  fields :favourited))
           (boostedp             (db-utils:db-getf  fields :reblogged))
           (sensitivep           (db-utils:db-getf  fields :sensitive))
           (message              (make-tui-string ""))
           (raw-selected-message "")
           (raw-deleted-message  "")
           (redp                 (redp           message-prefix-info))
           (rootp                (root-message-p message-prefix-info))
           (fg                   (if redp
                                    read-fg
                                    unread-fg))
           (bg                   (if redp
                                     read-bg
                                     unread-bg))
           (attribute            (if redp
                                     read-attribute
                                     unread-attribute)))
      (labels ((make-colored-string (a)
                 (make-tui-string a
                                  :attributes attribute
                                  :fgcolor    fg
                                  :bgcolor    bg))
               (message-cat (a)
                 (setf message (cat-tui-string message a)))
               (selected-message-cat (a)
                 (if (typep a 'string)
                     (setf raw-selected-message (strcat raw-selected-message a))
                     (setf raw-selected-message (strcat raw-selected-message
                                                        (tui-string->chars-string a)))))
               (deleted-message-cat (a)
                 (if (typep a 'string)
                     (setf raw-deleted-message (strcat raw-deleted-message a))
                     (setf raw-deleted-message (strcat raw-deleted-message
                                                       (tui-string->chars-string a)))))
               (selected-message-invisible-text (text)
                 (selected-message-cat (build-string (text-length text))))
               (deleted-message-invisible-text (text)
                 (deleted-message-cat (build-string (text-length text))))
               (ellipsize (text)
                 (text-ellipsis text max-width)))
        (message-cat (make-colored-string (index         message-prefix-info)))
        (message-cat (make-colored-string (creation-date message-prefix-info)))
        (message-cat (make-colored-string (author        message-prefix-info)))
        (selected-message-cat (index         message-prefix-info))
        (selected-message-cat (creation-date message-prefix-info))
        (selected-message-cat (author        message-prefix-info))
        (deleted-message-cat (index         message-prefix-info))
        (deleted-message-cat (creation-date message-prefix-info))
        (deleted-message-cat (author        message-prefix-info))
        (if favouritedp
            (progn
              (deleted-message-cat favourite-text)
              (selected-message-cat favourite-text)
              (message-cat favourite-text))
            (progn
              (deleted-message-invisible-text favourite-text-off)
              (selected-message-invisible-text favourite-text-off)
              (message-cat favourite-text-off)))
        (if boostedp
            (progn
              (deleted-message-cat boosted-text)
              (selected-message-cat boosted-text)
              (message-cat boosted-text))
            (progn
              (deleted-message-invisible-text boosted-text-off)
              (selected-message-invisible-text boosted-text-off)
              (message-cat boosted-text-off)))
        (if sensitivep
            (progn
              (deleted-message-cat sensitive-text)
              (selected-message-cat sensitive-text)
              (message-cat sensitive-text))
            (progn
              (deleted-message-invisible-text sensitive-text-off)
              (selected-message-invisible-text sensitive-text-off)
              (message-cat sensitive-text-off)))
        (if rootp
            (progn
              (deleted-message-cat root-text)
              (selected-message-cat root-text)
              (message-cat root-text))
            (progn
              (deleted-message-invisible-text root-text-off)
              (selected-message-invisible-text root-text-off)
              (message-cat root-text-off)))
        (message-cat rendered-tree-line)
        (selected-message-cat rendered-tree-line)
        (deleted-message-cat rendered-tree-line)
        (let* ((right-padding-string  (right-padding-suffix raw-selected-message
                                                               line-total-width))
               (right-padding-special (make-tui-string right-padding-string
                                                       :fgcolor fg
                                                       :bgcolor bg)))
          (message-cat          right-padding-special)
          (selected-message-cat right-padding-string)
          (deleted-message-cat right-padding-string)
          (let ((selected-message (make-tui-string raw-selected-message
                                                   :attributes selected-attribute
                                                   :fgcolor selected-fg
                                                   :bgcolor selected-bg))
                (deleted-message  (make-tui-string raw-deleted-message
                                                   :attributes deleted-attribute
                                                   :fgcolor deleted-fg
                                                   :bgcolor deleted-bg)))
            (values (ellipsize message)
                    (ellipsize selected-message)
                    (ellipsize deleted-message))))))))

(defmethod build-lines ((object thread-window) annotated-tree selected-pos)
  (with-accessors ((tree-color-map     tree-color-map)
                   (selected-bg        selected-bg)
                   (selected-fg        selected-fg)
                   (timeline-type      timeline-type)
                   (timeline-folder    timeline-folder)
                   (row-selected-index row-selected-index)
                   (rows               rows)) object
    (if (null annotated-tree)
        (setf rows nil)
        (progn
          (setf row-selected-index selected-pos)
          (multiple-value-bind (tree-lines all-fields)
              (build-lines tree-color-map annotated-tree selected-pos)
            (let* ((prefixes          ())
                   (new-rows          ())
                   (message-indices   (mapcar (lambda (row)
                                                (db-utils:db-getf row :message-index))
                                              all-fields))
                   (max-message-index (last-elt message-indices))
                   (max-author-length (db:max-username-length timeline-type timeline-folder)))
              (loop
                 for index from 0
                 for message-index in message-indices
                 for fields in all-fields do
                   (push (make-message-row-prefix object
                                                  fields
                                                  message-index
                                                  max-message-index
                                                  max-author-length)
                         prefixes)
                   (push (make-instance 'line
                                        :fields      fields
                                        :selected-bg selected-bg
                                        :selected-fg selected-fg
                                        :index       index
                                        :selected    (= index selected-pos))
                         new-rows))
              (nreversef prefixes)
              (nreversef new-rows)
              (pad-row-prefix prefixes)
              (loop
                 for row in new-rows
                 for message-prefix     in prefixes
                 for rendered-tree-line in tree-lines
                 do
                   (multiple-value-bind (message selected-message deleted-message)
                       (make-message-row object rendered-tree-line row message-prefix)
                     (with-accessors ((normal-text   normal-text)
                                      (selected-text selected-text)
                                      (deleted-text  deleted-text)) row
                       (setf normal-text   message)
                       (setf selected-text selected-message)
                       (setf deleted-text  deleted-message))))
              (setf rows new-rows))))))
    object)

(defmethod go-message-down ((object thread-window))
  (with-accessors ((selected-bg        selected-bg)
                   (selected-fg        selected-fg)
                   (row-selected-index row-selected-index)
                   (timeline-type      timeline-type)
                   (timeline-folder    timeline-folder)
                   (rows               rows)) object
    (when rows
      (let ((new-index (1+ row-selected-index)))
        (if (>= new-index
                (length rows))
            (let* ((last-message-index (db:row-message-index (fields (last-elt rows))))
                   (next-message-index (1+ last-message-index)))
              (when (db:message-from-timeline-folder-message-index timeline-type
                                                                   timeline-folder
                                                                   next-message-index)
                (multiple-value-bind (tree pos)
                    (fit-timeline-to-window object next-message-index)
                  (build-lines object tree pos))))
            (progn
              (unselect-all object)
              (select-row object new-index)))
        (draw object)))))

(defmethod go-message-up ((object thread-window))
  (with-accessors ((selected-bg        selected-bg)
                   (selected-fg        selected-fg)
                   (row-selected-index row-selected-index)
                   (timeline-type      timeline-type)
                   (timeline-folder    timeline-folder)
                   (rows               rows)) object
    (when rows
      (let ((new-index (1- row-selected-index)))
        (if (< new-index 0)
            (let* ((first-message-index    (db:row-message-index (fields (first-elt rows))))
                   (previous-message-index (1- first-message-index)))
              (when (db:message-from-timeline-folder-message-index timeline-type
                                                                   timeline-folder
                                                                   previous-message-index)
                (multiple-value-bind (tree pos)
                    (fit-timeline-to-window object previous-message-index)
                  (build-lines object tree pos))))
            (progn
              (unselect-all object)
              (select-row object new-index)))
        (draw object)))))

(defmethod goto-message ((object thread-window) message-index)
  (assert (numberp message-index))
  (with-accessors ((timeline-folder timeline-folder)
                   (timeline-type   timeline-type)) object
    (let ((message (db:message-from-timeline-folder-message-index timeline-type
                                                                  timeline-folder
                                                                  message-index)))
      (if message
          (multiple-value-bind (tree pos)
              (fit-timeline-to-window object message-index)
            (build-lines object tree pos)
            (draw object))
          (ui:info-message (format nil
                                    (_ "No message with index ~a exists.")
                                    message-index))))))

(defmethod goto-first-message ((object thread-window))
    (goto-message object db:+message-index-start+))

(defmethod goto-last-message ((object thread-window))
  (with-accessors ((timeline-folder timeline-folder)
                   (timeline-type   timeline-type)) object
    (when-let ((last-message-index (db:last-message-index-status timeline-type timeline-folder)))
      (goto-message object last-message-index))))

(defmethod resync-rows-db ((object thread-window) &key (redraw t) (suggested-message-index nil))
  (with-accessors ((row-selected-index row-selected-index)
                   (rows               rows)) object
    (let ((saved-row-selected-index (if suggested-message-index
                                        (db:message-index->sequence-index suggested-message-index)
                                        row-selected-index))
          (first-message-index      (or suggested-message-index
                                        (db:row-message-index (fields (first-elt rows))))))
      (handler-bind ((conditions:out-of-bounds
                      (lambda (e)
                        (invoke-restart 'ignore-selecting-action e))))
        (multiple-value-bind (tree pos)
            (fit-timeline-to-window object first-message-index)
          (build-lines object tree pos)
          (unselect-all object)
          (select-row object saved-row-selected-index)
          (when redraw
            (draw object))))))
  object)

(defun reblogged-data (reblogger-status)
  (when-let* ((reblogged-id     (db:row-message-reblog-id reblogger-status))
              (reblogged-status (db:find-status-id reblogged-id)))
    (let ((body            (db:row-message-rendered-text reblogged-status))
          (attachments     (status-attachments->text reblogged-id)))
      (values body attachments))))

(defmethod open-message ((object thread-window))
  (with-accessors ((row-selected-index row-selected-index)
                   (rows               rows)
                   (timeline-type      timeline-type)
                   (timeline-folder    timeline-folder)) object
    (when-let* ((selected-row (selected-row object))
                (fields       (fields selected-row))
                (original     (db-utils:db-getf fields :content ""))
                (status-id    (db:row-message-status-id fields))
                (header       (message-original->text-header fields)))
      (let* ((body          (db:row-message-rendered-text fields))
             (attachments   (status-attachments->text status-id))
             (refresh-event (make-instance 'program-events:refresh-conversations-window-event))
             (poll          (db:find-poll-bound-to-status status-id))
             (poll-text     (poll->text (db:row-id poll)
                                        (truncate (/ (win-width-no-border object)
                                                     2)))))
        (multiple-value-bind (reblogged-status-body reblogged-status-attachments)
            (reblogged-data fields)
          (let ((actual-body        (if (string= body reblogged-status-body)
                                        body
                                        (strcat body reblogged-status-body)))
                (actual-attachments (if (string= attachments reblogged-status-attachments)
                                        attachments
                                        (strcat reblogged-status-attachments attachments))))
            (setf (message-window:source-text *message-window*)
                  (strcat header
                          actual-body
                          poll-text
                          actual-attachments))
            (db:mark-status-red-p timeline-type timeline-folder status-id)
            (resync-rows-db object :redraw t)
            (program-events:push-event refresh-event)
            (draw *message-window*)))))))

(defun mark-selected-status-boolean-value (window function)
  (with-accessors ((row-selected-index row-selected-index)
                   (rows               rows)
                   (timeline-type      timeline-type)
                   (timeline-folder    timeline-folder)) window
    (assert (selected-row window))
    (let* ((selected-row (selected-row window))
           (fields       (fields selected-row))
           (status-id    (db:row-message-status-id fields)))
      (funcall function timeline-type timeline-folder status-id))))

(defmethod mark-selected-message-to-delete ((object thread-window)
                                            &key (move-down-selected-message nil))
  (mark-selected-status-boolean-value object #'db:mark-status-deleted-p)
  (resync-rows-db object :redraw t)
  (when move-down-selected-message
    (go-message-down object)))

(defmethod mark-selected-message-prevent-delete ((object thread-window)
                                                 &key (move-down-selected-message nil))
  (mark-selected-status-boolean-value object #'db:mark-status-prevent-deletion)
  (resync-rows-db object :redraw t)
  (when move-down-selected-message
    (go-message-down object)))

(defun rebuild-lines (window message-id)
  (multiple-value-bind (tree pos)
      (fit-timeline-to-window window message-id)
    (build-lines window tree pos)))

(defun search-messages-body (window text-looking-for direction error-message)
  (with-accessors ((row-selected-index row-selected-index)
                   (rows               rows)
                   (timeline-folder    timeline-folder)
                   (timeline-type      timeline-type)) window
    (when-let* ((selected-fields (selected-row-fields window))
                (starting-index  (db-utils:db-getf selected-fields :message-index)))
      (let ((matching-status (if (eq direction :next)
                                 (db:search-next-message-body     timeline-type
                                                                  timeline-folder
                                                                  text-looking-for
                                                                  starting-index)
                                 (db:search-previous-message-body timeline-type
                                                                  timeline-folder
                                                                  text-looking-for
                                                                  starting-index))))
        (if matching-status
            (let ((new-message-index (db:row-message-index matching-status)))
              (rebuild-lines window new-message-index)
              (open-message window))
            (ui:info-message (format nil error-message text-looking-for)))))))

(defmethod search-next-message-body ((object thread-window) text-looking-for)
  (search-messages-body object
                   text-looking-for
                   :next
                   (_ "No next message that contains ~s exists.")))

(defmethod search-previous-message-body ((object thread-window) text-looking-for)
  (search-messages-body object
                   text-looking-for
                   :previous
                   (_ "No previous message that contains ~s exists.")))

(defun search-messages-meta (window text-looking-for direction error-message)
  (with-accessors ((row-selected-index row-selected-index)
                   (rows               rows)
                   (timeline-folder    timeline-folder)
                   (timeline-type      timeline-type)) window
    (when-let* ((selected-fields (selected-row-fields window))
                (starting-index  (db-utils:db-getf selected-fields :message-index)))
      (let ((matching-status (if (eq direction :next)
                                 (db:search-next-message-meta     timeline-type
                                                                  timeline-folder
                                                                  text-looking-for
                                                                  starting-index)
                                 (db:search-previous-message-meta timeline-type
                                                                  timeline-folder
                                                                  text-looking-for
                                                                  starting-index))))
        (if matching-status
            (let ((new-message-index (db:row-message-index matching-status)))
              (rebuild-lines window new-message-index)
              (open-message window))
            (ui:info-message (format nil error-message text-looking-for)))))))

(defmethod search-next-message-meta ((object thread-window) text-looking-for)
  (search-messages-meta object
                        text-looking-for
                        :next
                        (_ "No next message that contains ~s exists.")))

(defmethod search-previous-message-meta ((object thread-window) text-looking-for)
  (search-messages-meta object
                        text-looking-for
                        :previous
                        (_ "No previous message that contains ~s exists.")))

(defmethod search-next-unread ((object thread-window))
  (with-accessors ((row-selected-index row-selected-index)
                   (rows               rows)
                   (timeline-folder    timeline-folder)
                   (timeline-type      timeline-type)) object
    (when-let* ((selected-fields (selected-row-fields object))
                (starting-index  (db-utils:db-getf selected-fields :message-index)))
      (let ((matching-status (db:search-next-unread-message timeline-type
                                                            timeline-folder
                                                            starting-index)))
        (if matching-status
            (let ((new-message-index (db:row-message-index matching-status)))
              (rebuild-lines object new-message-index)
              (open-message object))
            (ui:info-message (_ "No others unread messages exist.")))))))

(defgeneric marked-to-delete-p (object))

(defmethod marked-to-delete-p ((object line))
  (db-utils:db-getf (fields object) :deletedp))

(defun init ()
  (let* ((low-level-window (make-croatoan-window :enable-function-keys t)))
    (setf *thread-window*
          (make-instance 'thread-window
                         :title           (_ "Threads")
                         :keybindings     keybindings:*thread-keymap*
                         :key-config      swconf:+key-thread-window+
                         :croatoan-window low-level-window))
    (refresh-config *thread-window*)
    (setf (keybindings *thread-window*) keybindings:*thread-keymap*)
    (resync-rows-db *thread-window* :suggested-message-index db:+message-index-start+)
    *thread-window*))
