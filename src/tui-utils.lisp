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

(in-package :tui-utils)

(defun make-win-background (color-bg &key (color-fg nil) (char #\Space))
  "Makes an object suitable as background for a window using `color-bg' as background color,
`color-fg' as  foreground color (default to  `color-bg') and character
`char'."
  (croatoan:make-background color-bg :color-fg color-fg :char char))

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
  "Returns the length (in characters  units) of a complex string passed
as argument `complex-string'."
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

(defun text-length (text)
  (text-width text))

(defun find-max-line-width (lines)
  (assert lines)
  (reduce #'max (mapcar #'text-length lines)))

(defmethod (setf bgcolor) ((object complex-string) new-bg)
  (loop for xchar across (complex-char-array object) do
       (setf (bgcolor xchar) new-bg)))

(defmethod (setf fgcolor) ((object complex-string) new-fg)
  (loop for xchar across (complex-char-array object) do
      (setf (fgcolor xchar) new-fg)))

(defun ncat-complex-string (a b)
  "Destructively concatenate the `complex-string' `a' and `b'"
  (croatoan:nconcat-complex-string a b))

(defgeneric to-tui-string (object &key &allow-other-keys))

(defmethod to-tui-string ((object string) &key &allow-other-keys)
  (make-tui-string object))

(defgeneric cat-complex-string (a b &key color-attributes-contagion)
  (:documentation "Return  a new `complex-string' that  is the results
  of concatenating `a' and 'b'. If `color-attributes-contagion' is non
  nil `b' will inherit all the attributes and color of a."))

(defmethod cat-complex-string ((a complex-string) (b sequence)
                                  &key (color-attributes-contagion t))
  "Return a  complex string  that is the  results of  concatenating of
  `a'    (a    `complex-string')    and     `b'    (a    string)    If
  `color-attributes-contagion'  is non  nil `b'  will inherit  all the
  attributes and color of a."
  (croatoan:concat-complex-string a b :color-attributes-contagion color-attributes-contagion))

(defmethod cat-complex-string ((a sequence) (b complex-string)
                                  &key (color-attributes-contagion t))
  "Return a  complex string  that is the  results of  concatenating of
  `a'    (a    string)    and     `b'    (a    `complex-string')    If
  `color-attributes-contagion'  is non  nil `b'  will inherit  all the
  attributes and color of a."
  (croatoan:concat-complex-string a b :color-attributes-contagion color-attributes-contagion))

(defmethod cat-complex-string ((a complex-string) (b complex-string)
                                  &key (color-attributes-contagion nil))
  "Return a complex string that is the results of concatenating of `a'
  and  `b': two  `complex-string'. If  `color-attributes-contagion' is
  non nil `b' will inherit all the attributes and color of a."
  (croatoan:concat-complex-string a b :color-attributes-contagion color-attributes-contagion))

(defalias cat-tui-string #'cat-complex-string)

(defun tui-char->char (complex-char)
  (simple-char complex-char))

(defun tui-string->chars-string (tui-string)
  "Convert a `tui-string' to a `string'."
  (croatoan:complex-string->chars-string tui-string))

(defun tui-string-subseq (string start end)
  (croatoan:text-slice string start end))

(defgeneric text-ellipsis (object len &key truncate-string)
  (:documentation "If `object''s length is bigger  than `len', cut the last characters
  out.  Also replaces the last n  characters (where n is the length of
  `truncate-string')     of     the      shortened     string     with
  `truncate-string'. It  defaults to  \"…\", but can  be nil  or the
  empty string."))

(defmethod text-ellipsis ((object string) len &key (truncate-string "…"))
  (ellipsize object len :truncate-string truncate-string))

(defmethod text-ellipsis ((object complex-string) len &key (truncate-string "…"))
  (croatoan:text-ellipsize object len :truncate-string truncate-string))

(defgeneric right-pad-text (object total-size &key padding-char)
  (:documentation "Prepend a number of copies of `padding-char' to `object' so that the
latter has a length equals to `total-size'"))

(defmethod right-pad-text ((object string) (total-size number) &key (padding-char #\Space))
  (assert (> total-size 0))
  (croatoan:text-right-pad object total-size :padding-char padding-char))

(defmethod right-pad-text ((object complex-string) (total-size number)
                               &key (padding-char #\Space))
  (assert (> total-size 0))
  (croatoan:text-right-pad object total-size :padding-char padding-char))

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
    (colorize-line line
                   (create-scanner regexp)
                   :fgcolor    fgcolor
                   :bgcolor    bgcolor
                   :attributes attributes
                   :return-as-list-p return-as-list-p))

(defmethod colorize-line ((line string) (regexp function)
                          &key
                            (fgcolor          nil)
                            (bgcolor          nil)
                            (attributes       nil)
                            (return-as-list-p t))
  (let ((res     ())
        (scanner regexp))
    (labels ((append-to-res (data)
               (reversef res)
               (push data res)
               (setf res (reverse res)))
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

(defgeneric apply-coloring (from to))

(defmethod apply-coloring ((from complex-string) (to string))
  (with-accessors ((complex-char-array-from complex-char-array)) from
    (let* ((res            (make-tui-string to))
           (length-diff    (- (length to)
                              (text-length from)))
           (last-char-from (last-elt complex-char-array-from))
           (last-char-fg   (fgcolor last-char-from))
           (last-char-bg   (bgcolor last-char-from))
           (last-char-attr (attributes last-char-from)))
      (with-accessors ((complex-char-array-to complex-char-array)) res
      (loop
        for from-char across complex-char-array-from
        for to-char   across complex-char-array-to
        do
           (setf (attributes to-char)
                 (attributes from-char))
           (setf (fgcolor to-char)
                 (fgcolor from-char))
           (setf (bgcolor to-char)
                 (bgcolor from-char)))
      (when (> length-diff 0)
        (loop for i from length-diff below (length to) do
          (let ((char (elt complex-char-array-to i)))
            (setf (attributes char)
                  last-char-attr)
            (setf (fgcolor char)
                  last-char-fg)
            (setf (bgcolor char)
                  last-char-bg))))
        res))))

(defmethod remove-corrupting-utf8-chars ((object complex-string))
  (setf (complex-char-array object)
        (remove-if (lambda (a) (display-corrupting-utf8-p (simple-char a)))
                   (complex-char-array object)))
  object)

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

(defun standard-error-notify-life ()
  (* (swconf:config-notification-life) 5))

(defmacro with-notify-errors (&body body)
  #+debug-mode `(progn ,@body)
  #-debug-mode `(handler-case
                    (progn
                      ,@body)
                  (error (e)
                    (ui:notify (format nil (_ "Error: ~a") e)
                               :life     (* (swconf:config-notification-life) 5)
                               :as-error t))))

(defmacro with-print-error-message (&body body)
  #+debug-mode `(progn ,@body)
  #-debug-mode `(handler-case
                    (progn
                      ,@body)
                  (error (e)
                    (ui:error-message (format nil (_ "Error: ~a") e)))))
