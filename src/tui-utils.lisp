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

(in-package :tui-utils)

(defun make-background (color-bg &key (color-fg nil) (char #\Space))
  (make-instance 'complex-char
                 :simple-char char
                 :color-pair (if color-fg
                                 (list color-fg color-bg)
                                 (list color-bg color-bg))))

(defun make-croatoan-window (&rest keys)
  (apply #'make-instance
         'window
         (append (list :stacked               nil
                       :input-blocking        nil)
                 keys)))

(defun make-blocking-croatoan-window (&rest keys)
  (apply #'make-instance
         'window
         (append (list :stacked               nil
                       :input-blocking          t)
                 keys)))

(defun make-screen ()
  (make-instance 'screen
                 :input-buffering            nil
                 :process-control-chars      nil
                 :enable-newline-translation   t
                 :input-blocking             nil
                 :input-echoing              nil
                 :enable-function-keys         t
                 :enable-scrolling           nil
                 :insert-mode                nil
                 :enable-colors                t
                 :use-terminal-colors        nil
                 :cursor-visible             nil
                 :stacked                    nil))

(defun make-tui-char (char
                      &key
                        (attributes nil)
                        (fgcolor    nil)
                        (bgcolor    nil))
  (make-instance 'complex-char
                 :simple-char char
                 :attributes  attributes
                 :fgcolor     fgcolor
                 :bgcolor     bgcolor))

(defun make-tui-string (string
                        &key
                          (attributes nil)
                          (fgcolor    nil)
                          (bgcolor    nil))
  (make-instance 'complex-string
                 :string      string
                 :attributes  attributes
                 :fgcolor     fgcolor
                 :bgcolor     bgcolor))

(defmethod string-empty-p ((s complex-char))
  (null (simple-char s)))

(defmethod string-empty-p ((s complex-string))
  (or (misc:vector-empty-p  (complex-char-array s))
      (every #'string-empty-p (complex-char-array s))))

(defmacro tui-format ((control-string &rest args)
                      &key
                        (attributes nil)
                        (fgcolor    nil)
                        (bgcolor    nil))
  `(make-tui-string (apply #'format nil ,control-string ,@args)
                    :attributes  ,attributes
                    :fgcolor     ,fgcolor
                    :bgcolor     ,bgcolor))

(defun complex-string-length (complex-string)
  (length (complex-char-array complex-string)))

(defun decode-key-event (event)
  (cond
    ((characterp event)
     (key-to-string event))
    ((symbolp event)
     (symbol-name event))
    (t
     (error (_ "Unknown event ~a") event))))

(defun colorize-tree-element (color-map annotated-element)
  "Colormap is an alist like:
        (:branch    . branch-color)
        (:arrow     . arrow-color)
        (:data      . data-color)
        (:data-leaf . leaf-color)
        (:data-root . root-color)"
  (let ((semantic-value (annotated-text-symbol annotated-element))
        (value          (annotated-text-value  annotated-element)))
    (make-tui-string value :fgcolor (cdr (assoc semantic-value color-map)))))

(defun colorize-tree-line (annotated-line color-map)
  (let ((res-line ()))
    (loop for block in annotated-line do
         (push (colorize-tree-element color-map block)
               res-line))
    (setf res-line (reverse res-line))
    res-line))

(defgeneric text-width (object))

(defgeneric text-slice (object start &optional end))

(defmethod text-width ((object string))
  (length object))

(defmethod text-width ((object complex-string))
  (complex-string-length object))

(defmethod text-slice ((object string) start &optional (end nil))
  (subseq object start end))

(defmethod text-slice ((object complex-string) start &optional (end nil))
  (let ((res (clone object)))
    (setf (complex-char-array res)
          (misc:array-slice (complex-char-array object) start end))
    res))

(defun find-max-line-width (lines)
  (reduce #'max (mapcar #'text-width lines)))

(defmethod (setf bgcolor) ((object complex-string) new-bg)
  (loop for xchar across (complex-char-array object) do
       (setf (bgcolor xchar) new-bg)))

(defmethod (setf fgcolor) ((object complex-string) new-fg)
  (loop for xchar across (complex-char-array object) do
      (setf (fgcolor xchar) new-fg)))

(defmethod clone-into ((from complex-char) (to complex-char))
  (flet ((%copy (thing)
           (if (listp thing)
               (copy-list thing)
               thing)))
    (setf (simple-char to) (simple-char from)
          (attributes  to) (copy-list (attributes from))
          (fgcolor     to) (%copy (fgcolor from))
          (bgcolor     to) (%copy (bgcolor from)))
    to))

(defmethod clone ((object complex-char))
  (with-simple-clone (object 'complex-char)))

(defmethod clone-into ((from complex-string) (to complex-string))
  (with-accessors ((char-array-to complex-char-array)) to
    (setf char-array-to
          (make-array (length (complex-char-array from))
                      :initial-element (make-instance 'complex-char)
                      :element-type 'complex-char
                      :fill-pointer (length (complex-char-array from))
                      :adjustable   t))
    (loop
       for xch across (complex-char-array from)
       for i from 0
       do
         (setf (elt char-array-to i)
               (clone xch)))
    to))

(defmethod clone ((object complex-string))
  (with-simple-clone (object 'complex-string)))

(defun ncat-complex-string (a b)
  (with-accessors ((inner-array-a complex-char-array)) a
    (with-accessors ((inner-array-b complex-char-array)) b
      (setf inner-array-a
            (concatenate 'vector inner-array-a inner-array-b)))))

(defgeneric cat-complex-string (a b &key color-attributes-contagion))

(defun cat-complex-string-no-contagion (a b)
  (with-accessors ((inner-array-a complex-char-array)) a
    (let* ((res (make-instance 'complex-string
                               :complex-char-array (copy-array inner-array-a))))
      (with-accessors ((inner-array-res complex-char-array)) res
        (map nil
             (lambda (a)
               (vector-push-extend (make-instance 'complex-char
                                                  :simple-char a)
                                   inner-array-res))
             b))
      res)))

(defmethod cat-complex-string ((a complex-string) (b sequence)
                               &key (color-attributes-contagion t))
  (if (not color-attributes-contagion)
      (cat-complex-string-no-contagion a b)
      (with-accessors ((inner-array-a complex-char-array)) a
        (let* ((res                  (make-instance 'complex-string
                                                    :complex-char-array (copy-array inner-array-a)))
               (last-complex-char    (and (not (misc:vector-empty-p inner-array-a))
                                          (last-elt inner-array-a)))
               (last-char-attributes (and last-complex-char
                                          (attributes last-complex-char)))
               (last-char-fg         (and last-complex-char
                                          (fgcolor last-complex-char)))
               (last-char-bg         (and last-complex-char
                                          (bgcolor last-complex-char))))
          (with-accessors ((inner-array-res complex-char-array)) res
            (map nil
                 (lambda (a)
                   (vector-push-extend (make-instance 'complex-char
                                                      :bgcolor     last-char-bg
                                                      :fgcolor     last-char-fg
                                                      :attributes  last-char-attributes
                                                      :simple-char a)
                                       inner-array-res))
                 b))
          res))))

(defmethod cat-complex-string ((a complex-string) (b complex-string)
                               &key (color-attributes-contagion t))
  (declare (ignore color-attributes-contagion))
  (with-accessors ((inner-array-a complex-char-array)) a
    (with-accessors ((inner-array-b complex-char-array)) b
      (let ((res (make-instance 'complex-string
                                :complex-char-array (copy-array inner-array-a))))
        (with-accessors ((inner-array-res complex-char-array)) res
          (loop for i across  inner-array-b do
               (vector-push-extend i inner-array-res))
          res)))))

(defalias cat-tui-string #'cat-complex-string)

(defun complex-char->char (complex-char)
  (simple-char complex-char))

(defun tui-string->chars-string (tui-string)
  (with-accessors ((complex-char-array complex-char-array)) tui-string
    (let ((res (misc:make-fresh-array 0 #\a 'character nil)))
      (with-output-to-string (stream res)
        (loop for i across complex-char-array do
             (format stream "~a" (complex-char->char i)))
        res))))

(defgeneric text-ellipsize (object len &key truncate-string))

(defmethod text-ellipsize ((object string) len &key (truncate-string "..."))
  (ellipsize object len :truncate-string truncate-string))

(defmethod text-ellipsize ((object complex-string) len &key (truncate-string "..."))
  "If 'string''s length is bigger  than 'len', cut the last characters
  out.  Also replaces the last n  characters (where n is the length of
  'truncate-string')     of     the      shortened     string     with
  'truncate-string'. It  defaults to  \"...\", but can  be nil  or the
  empty string."
  (let ((string-len (text-width object)))
    (cond
      ((<= string-len len)
       object)
      ((< len
          (text-width truncate-string))
       (text-slice object 0 len))
      (t
       (cat-complex-string (text-slice object 0 (- len (text-width truncate-string)))
                           truncate-string)))))

(defgeneric right-pad-text (object total-size &key padding-char))

(defmethod right-pad-text ((object string) (total-size number) &key (padding-char #\Space))
  (assert (> total-size 0))
  (right-padding object total-size :padding-char padding-char))

(defmethod right-pad-text ((object complex-string) (total-size number)
                               &key (padding-char #\Space))
  (assert (> total-size 0))
  (let ((suffix (make-string (max 0 (- total-size (text-width object)))
                             :initial-element padding-char)))
    (cat-tui-string object suffix)))

(defun text->tui-attribute (text)
  (if (null text)
      text
      (progn
        (assert (member text '("reverse" "bold" "underline" "italic" "blink" "dim" "invis")
                        :test #'string-equal))
        (list (make-keyword (string-upcase text))))))

(defun assemble-attributes (&optional
                              (reverse   nil)
                              (bold      nil)
                              (underline nil)
                              (italic    nil)
                              (blink     nil)
                              (dim       nil)
                              (invis     nil))
  (if (every #'null
             (list reverse
                   bold
                   underline
                   italic
                   blink
                   dim
                   invis))
      nil
      (macrolet ((gen-push (&rest vars)
                   `(progn
                      ,@(loop for var in vars collect
                             `(when ,var
                                (push ,(make-keyword var) attributes-list))))))

        (let ((attributes-list ()))
          (gen-push reverse
                    bold
                    underline
                    italic
                    blink
                    dim
                    invis)
          attributes-list))))

(defmacro gen-single-attributes-functions (&rest names)
  (let ((template-attrs (misc:make-fresh-list (length names) nil)))
    `(progn
       ,@(loop
            for name in names
            for set-on from 0 collect
              `(defun ,(misc:format-fn-symbol t "attribute-~a" name) ()
                 ,(let ((attrs (copy-list template-attrs)))
                    (setf (elt attrs set-on) t)
                    `(assemble-attributes ,@attrs)))))))

(gen-single-attributes-functions reverse
                                 bold
                                 underline
                                 italic
                                 blink
                                 dim
                                 invisible)

(defun combine-attributes (&rest attributes)
  (reduce #'misc:lcat attributes))

(defgeneric colorize-line (line regexp &key &allow-other-keys))

(defmethod  colorize-line ((line string) (regexp swconf:color-re-assign) &key &allow-other-keys)
  (colorize-line line
                 (swconf:re regexp)
                 :fgcolor    (or (swconf:color-name  regexp)
                                 (swconf:color-value regexp))
                 :attributes (swconf:attributes regexp)))

(defmethod  colorize-line ((line list) (regexp swconf:color-re-assign) &key &allow-other-keys)
  (colorize-line line
                 (swconf:re regexp)
                 :fgcolor    (or (swconf:color-name  regexp)
                                 (swconf:color-value regexp))
                 :attributes (swconf:attributes regexp)))

(defmethod colorize-line ((line string) regexp
                          &key
                            (fgcolor          nil)
                            (bgcolor          nil)
                            (attributes       nil)
                            (return-as-list-p t))
  (let ((res     ())
        (scanner (create-scanner regexp)))
    (labels ((append-to-res (data)
               (setf res (append res (list data))))
             (re-split (data)
               (when (string-not-empty-p data)
                 (multiple-value-bind (start-re end-re)
                     (scan scanner data)
                   (if (null start-re)
                       (append-to-res data)
                       (let* ((pre             (subseq data 0 start-re))
                              (datum           (subseq data start-re end-re))
                              (datum-colorized (make-tui-string datum
                                                                :attributes attributes
                                                                :bgcolor    bgcolor
                                                                :fgcolor    fgcolor))
                              (post            (subseq data end-re)))
                         (when (string-not-empty-p pre)
                           (append-to-res pre))
                         (append-to-res datum-colorized)
                         (re-split post)))))))
      (re-split line)
      (if return-as-list-p
          res
          (colorized-line->tui-string res)))))

(defmethod colorize-line ((line complex-string) regexp &key &allow-other-keys)
  (declare (ignore regexp))
  line)

(defmethod colorize-line ((line list) regexp
                          &key
                            (fgcolor nil) (bgcolor nil) (attributes nil)
                            (return-as-list-p t))
  (let ((res (flatten (loop for i in line collect
                           (colorize-line i
                                          regexp
                                          :fgcolor    fgcolor
                                          :bgcolor    bgcolor
                                          :attributes attributes)))))
    (if return-as-list-p
        res
        (colorized-line->tui-string res))))

(defgeneric colorized-line->tui-string (line &key &allow-other-keys))

(defmethod  colorized-line->tui-string ((line string)
                                        &key
                                          (attributes nil)
                                          (fgcolor    nil)
                                          (bgcolor    nil))
  (make-tui-string line
                   :attributes  attributes
                   :fgcolor     fgcolor
                   :bgcolor     bgcolor))


(defmethod colorized-line->tui-string ((line complex-string) &key &allow-other-keys)
  line)

(defmethod colorized-line->tui-string ((line list) &key &allow-other-keys)
  "Line is a list of simple or complex strings"
  (reduce (lambda (a b)
            (cat-complex-string a b :color-attributes-contagion nil))
          line
          :initial-value (make-tui-string "")))

(defgeneric print-debug (object &optional stream))

(defmethod print-debug ((object complex-char) &optional (stream *standard-output*))
  (print-unreadable-object (object stream :type t :identity nil)
    (with-accessors ((simple-char simple-char)
                     (attributes  attributes)
                     (fgcolor     fgcolor)
                     (bgcolor     bgcolor)) object
      (if (not (or attributes
                   fgcolor
                   bgcolor))
          (format stream "~s" simple-char)
          (format stream
                  "~s fg: ~s bg: ~s attr: ~s"
                  simple-char
                  fgcolor
                  bgcolor
                  attributes)))))

(defmethod print-debug ((object complex-string) &optional (stream *standard-output*))
  (print-unreadable-object (object stream :type t :identity nil)
    (loop for i across (complex-char-array object) do
         (print-debug i))))

(defmacro with-notify-errors (&body body)
  #+debug-mode `(progn ,@body)
  #-debug-mode `(handler-case
                    (progn
                      ,@body)
                  (error (e)
                    (ui:notify (format nil (_ "Error: ~a") e)
                               :life     (* (swconf:config-notification-life) 5)
                               :as-error t))))
