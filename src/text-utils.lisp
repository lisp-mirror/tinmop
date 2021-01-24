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

;; derived from

;; niccolo': a chemicals inventory
;; Copyright (C) 2016  Universita' degli Studi di Palermo

;; This  program is  free  software: you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published  by  the  Free  Software Foundation,  version  3  of  the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :text-utils)

(alexandria:define-constant +float-regexp+   "-?[0-9]+(\\.[0-9]+([eE]-?[0-9]+)?)?" :test 'string=)

(alexandria:define-constant +integer-regexp+ "0|[1-9][0-9]+|[1-9]"                 :test 'string=)

(defun uchar-length (leading-byte)
  (let ((ones (do* ((ct 7 (1- ct))
                    (bit (ldb (byte 1 ct) leading-byte)
                         (ldb (byte 1 ct) leading-byte))
                    (ones-ct 0))
                   ((= bit 0) ones-ct)
                (incf ones-ct))))
    (cond
      ((= ones 0)
       1)
      ((= ones 1)
       0)
      (t
       ones))))

(defun utf8-encoded-p (file)
  (with-open-file (stream file :direction :input
                          :if-does-not-exist :error
                          ::element-type '(unsigned-byte 8))
    (let* ((leading-byte (read-byte stream))
           (leading-byte-length (uchar-length leading-byte)))
      (cond
        ((= leading-byte-length 0)
         nil)
        ((> leading-byte-length 6)
         nil)
        (t
         (loop for i from 0 below (1- leading-byte-length) do
              (let* ((ch (read-byte stream))
                     (ll (uchar-length ch)))
                (when (> ll 0)
                  (return-from utf8-encoded-p nil))))
         t)))))

(defgeneric to-s (object))

(defmethod to-s ((object string))
  object)

(defmethod to-s ((object vector))
  (coerce object 'string))

(defmethod to-s ((object character))
  (string object))

(defmethod to-s (object)
  (format nil "~a" object))

(defun clean-unprintable-chars (string)
  (cl-ppcre:scan-to-strings "[\\p{Letter}\\p{Number}\\p{Punctuation}]+" string))

(defun strcat (&rest chunks)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (strcat* chunks))

(defun strcat* (chunks)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (reduce (lambda (a b) (concatenate 'string a b)) chunks))

(defun strip-prefix (string prefix)
  (let ((re (strcat "^" prefix)))
    (cl-ppcre:regex-replace re string "")))

(defun strip-withespaces (string)
  (let ((re "\\s"))
    (cl-ppcre:regex-replace re string "")))

(defun common-prefix (&rest strings)
  (when strings
    (let* ((prefix-count    0)
           (sorted-strings (num:shellsort strings #'(lambda (a b) (> (length a)
                                                                     (length b)))))
           (pivot-string   (alexandria:first-elt sorted-strings))
           (actual-strings (rest sorted-strings))
           (res            (string (alexandria:first-elt pivot-string))))
      (labels ((advance-res ()
                 (incf prefix-count)
                 (setf res (strcat res (string (elt pivot-string prefix-count)))))
               (%advance ()
                 (loop for i in actual-strings do
                      (when (not (cl-ppcre:scan (strcat "^" res) i))
                        (setf res (subseq res 0 (1- (length res))))
                        (return-from %advance nil)))
                 (when (< (1+ prefix-count)
                          (length pivot-string))
                   (advance-res)
                   (%advance))))
        (%advance)
        res))))

(defgeneric join-with-strings (object junction))

(defmethod join-with-strings ((object sequence) (junction string))
  (reduce #'(lambda (a b) (text-utils:strcat a junction b)) object))

(defmethod join-with-strings ((object sequence) (junction character))
  (join-with-strings object (string junction)))

(defmethod join-with-strings ((object string) junction)
  (declare (ignore junction))
  object)

(defun join-with-strings* (junction &rest strings)
  (apply #'join-with-strings strings (list junction)))

(defun split-words (text)
  (cl-ppcre:split "\\p{White_Space}" text))

(defun split-lines (text)
  (cl-ppcre:split "[\\n\\r]" text))

(defun min-length-word (text)
     (loop for i in (split-words text)
        minimizing (length i) into min
        finally (return min)))

(defun max-length-word (text)
  (loop for i in (split-words text)
     maximizing (length i) into max
     finally (return max)))

(defun basename (file)
  (let ((pos (cl-ppcre:scan "\\." file)))
    (if pos
        (subseq file 0 pos)
        file)))

(defun wrap-with (s wrapper)
  (strcat wrapper s wrapper))

(defun build-string (length &optional (initial-element #\Space))
  (make-string length :initial-element initial-element))

(defun right-padding (str total-size &key (padding-char #\Space))
  (strcat str
          (make-string (max 0 (- total-size (length str)))
                       :initial-element padding-char)))

(defun right-padding-suffix (str total-size &key (padding-char #\Space))
  (make-string (max 0 (- total-size (length str)))
               :initial-element padding-char))

(defun left-padding (str total-size &key (padding-char #\Space))
  (strcat (make-string (max 0 (- total-size (length str)))
                       :initial-element padding-char)
          str))

(defun left-padding-prefix (str total-size &key (padding-char #\Space))
  (make-string (max 0 (- total-size (length str)))
               :initial-element padding-char))

(defun ellipsize (string len &key (truncate-string "..."))
  "If 'string''s length is bigger  than 'len', cut the last characters
  out.  Also replaces the last n  characters (where n is the length of
  'truncate-string')     of     the      shortened     string     with
  'truncate-string'. It  defaults to  \"...\", but can  be nil  or the
  empty string."
  (let ((string-len (length string)))
    (cond
      ((<= string-len len)
       string)
      ((< len
          (length truncate-string))
       (subseq string 0 len))
      (t
       (strcat (subseq string 0 (- len (length truncate-string)))
               truncate-string)))))

(defgeneric string-empty-p (s))

(defmethod string-empty-p (s)
  (error 'type-error
         :datum         s
         :expected-type 'string))

(defmethod string-empty-p ((s null))
  (declare (ignore s))
  t)

(defmethod string-empty-p ((s string))
  (string= s ""))

(defun string-not-empty-p (s)
  (not (string-empty-p s)))

(defun string-starts-with-p (start s &key (test #'string=))
  "Returns non nil if `s' starts with the substring `start'.
Uses `test' to match strings (default #'string=)"
  (when (>= (length s)
            (length start))
    (funcall test s start :start1 0 :end1 (length start))))

(defun string-ends-with-p (end s &key (test #'string=))
  "Returns t if s ends with the substring 'end', nil otherwise.
Uses `test' to match strings (default #'string=)"
  (when (>= (length s)
            (length end))
    (funcall test s end :start1 (- (length s) (length end)))))

(defvar *blanks* '(#\Space #\Newline #\Backspace #\Tab
                   #\Linefeed #\Page #\Return #\Rubout))

(defgeneric trim-blanks (s))

(defmethod trim-blanks ((s string))
  (string-trim *blanks* s))

(defmethod trim-blanks ((s null))
  s)

(defun justify-monospaced-text (text &optional (chars-per-line 30))
  (if (null (split-words text))
      (list " ")
      (let  ((text  (split-words text))
             (chars-per-line (round chars-per-line)))

        (labels ((spaces-pos-per-line (line) (floor (/ (length line) 2)))
                 (wline<= (l) (<= l  chars-per-line))
                 (line-length (line)
                   (reduce #'+ (mapcar #'length line) :initial-value 0))
                 (line-fit-p (line word)
                   (wline<= (+ (line-length line) (length word))))
                 (add-until-fit (text &optional (res '()))
                   (if (not (line-fit-p res (first text)))
                       (subseq res 0 (1- (length res)))
                       (add-until-fit (rest text) (append res (list (first text) " ")))))
                 (get-spacepos (line how-much)
                   (do ((pos '()))
                       ((>= (length pos) how-much) pos)
                     (let ((ranpos (random (length line))))
                       (when (and (oddp ranpos)
                                  (not (find ranpos pos :test #'=)))
                         (push ranpos pos)))))
                 (increment-each-space (line)
                   (loop for i in line collect (if (cl-ppcre:scan "\\p{White_Space}+" i)
                                                   (concatenate 'string i (string " "))
                                                   i)))
                 (justify-line (line &optional
                                     (spaces-left (- chars-per-line (line-length line))))
                   (cond
                     ((= (spaces-pos-per-line line) 0)
                      (copy-list line))
                     ((= spaces-left 0)
                      (copy-list line))
                     ((= spaces-left (spaces-pos-per-line line))
                      (increment-each-space line))
                     ((< spaces-left (spaces-pos-per-line line))
                      (loop for i in (get-spacepos line spaces-left) do
                           (setf (nth i line) (concatenate 'string (nth i line) (string " "))))
                      (copy-list line))
                     ((> spaces-left (spaces-pos-per-line line))
                      (justify-line
                       (increment-each-space line)
                       (- spaces-left (spaces-pos-per-line line)))))))
          (mapcar #'(lambda (l) (reduce #'(lambda (a b) (concatenate 'string a b)) l))
                  (do ((results '()))
                      ((null text) (reverse results))
                    (progn
                      (let* ((line (add-until-fit text))
                             (rest-text (if (> (1+ (floor (/ (length line) 2)))
                                               (length text))
                                            nil
                                            (subseq text (1+ (floor (/ (length line) 2)))))))
                        (setf text rest-text)
                        (push (justify-line line) results)))))))))

(defun flush-left-mono-text (text-words box-width &optional (lines '()))
  "Given a  list of  words (see:  split-words) return  a list  of text
lines that fits in 'box-width'"
  (flet ((join (words)
           (if words
               (join-with-strings words " ")
               "")))
    (if (null text-words)
        (reverse lines)
        (multiple-value-bind (line rest-of-words)
            (do ((words  text-words (rest words))
                 (line   '()        (misc:lcat line (list (first words))))
                 (line+1 '()        (if (> (length words) 1)
                                        (misc:lcat line (subseq words 0 2))
                                        line)))
                ((or (null words)
                     (> (length (join line+1)) box-width))
                 (values (join line) words)))
          (flush-left-mono-text rest-of-words box-width (misc:lcat (list line) lines))))))

(defun box-fit-as-much-lines (lines box-height)
  "Fit as much as possible lines in box.

   Example:

   Input: '(line1 line2 line3 line4 line5 line6 line7 line8 line9)

   +---------------+  -
   |line1          |  |
   |line2          |  | box-height
   |line3          |  |
   |line4          |  |
   |line5          |  |
   +---------------+  -

If there are more  lines than the number that can  be fitted the other
lines are discarded.

Return  two values:  a  column
 '(line1 line2  line3 line4 line5)

and the index of the first line was  not possible to fit or nil if the
lines fitted in the box (6 in this case).

"
  (let ((split-at (min (length lines) box-height)))
    (values (subseq lines 0 split-at)
            split-at)))

(defun find-max-line-length (lines)
  (reduce #'max (mapcar #'length lines)))

(defun box-fit-single-column (lines box-height box-width)
  "Fit as lines in box.

   Example:

   Input: '(line1 line2 line3 line4 line5 line6 line7 line8 line9)

   +---------------+  -                +---------------+  -
   |line1          |  |                |line6          |  |
   |line2          |  | box-height     |line7          |  | box-height
   |line3          |  |                |line8          |  |
   |line4          |  |                |line9          |  |
   |line5          |  |                |               |  |
   +---------------+  -                +---------------+  -

         A                                   B

Return the columns; each of them can  be fitted in the box (see figure
A and B).

Each line is padded with spaces to reach longest string in lines.

If  the padded  lines will  not  fit in  the  box width  they will  be
truncated.

"
  (labels ((fit (lines box-height)
             (multiple-value-bind (column rest-index)
                 (box-fit-as-much-lines lines box-height)
               (if (< rest-index (length lines))
                   (append (list column)
                           (fit (subseq lines rest-index)
                                box-height))
                   (list column)))))
    (let* ((max-width (find-max-line-length lines))
           (columns   (fit lines  box-height))
           (fitted    (loop for  lines in columns collect
                        (let ((padded (mapcar (lambda (a) (right-padding a max-width))
                                              lines)))
                          (if (> (length (first padded))
                                 box-width)
                              (loop for  line in lines collect
                                (subseq line 0 box-width))
                              padded)))))
      fitted)))

(defun box-fit-as-much-lines-columns (lines box-width box-height
                                      &key
                                        (spaces-between 1)
                                        (pad-right-fn (lambda (a max-width)
                                                        (right-padding a max-width)))
                                        (pad-left-fn
                                         (lambda (a) (strcat (build-string spaces-between)
                                                             a)))
                                        (column-width-fn (lambda (column)
                                                           (length (first column))))
                                        (build-pad-line-fn (lambda (column-width)
                                                             (build-string column-width)))
                                        (truncate-restart-fn
                                         (lambda (batch)
                                           (mapcar (lambda (a)
                                                     (subseq a
                                                             0
                                                             (- box-width spaces-between)))
                                                   batch)))
                                        (find-max-line-length-fn #'find-max-line-length))
  "Fit as much as possible lines in box using, if necessary multiple columns.

   Example:

   Input: '(line1 line2 line3 line4 line5 line6 line7 line8 line9)

     spaces-between
         |---|
   +---------------+  -
   |line1     line6|  |
   |line2     line7|  | box-height
   |line3     line8|  |
   |line4     line9|  |
   |line5          |  |
   +---------------+  -

   |-----------|
     box-width

If there  are more  lines than the  number that can  be fitted  the other
lines are discarded.

Return  two values:  a  batch  of columns
 (list  '(line1 line2 line3 line4 line5)
        '(line6 line7 line8 line9 '    '))

and the index of the first line was  not possible to fit or nil if the
lines fitted in the box (nil in this case).

"
  (let ((columns          ())
        (rest-lines-index 0)
        (lines-length     (length lines)))
    (labels ((pad-height (column)
               (let ((column-width (funcall column-width-fn column)))
                 (if (< (length column)
                        box-height)
                     (let ((pad-line (funcall build-pad-line-fn column-width)))
                       (append column
                               (make-list (- box-height
                                             (length column))
                                          :initial-element pad-line)))
                     column)))
             (pad (batch add-space-left)
               (let* ((max-width (funcall find-max-line-length-fn batch))
                      (padded    (mapcar (lambda (a) (funcall pad-right-fn a max-width))
                                         batch))
                      (column    (if add-space-left
                                     (mapcar pad-left-fn padded)
                                     padded)))
                 (if (> (+ max-width spaces-between)
                        box-width)
                     (restart-case
                         (error 'conditions:out-of-bounds
                                :text
                                (format nil
                                        (_ "Can not fit column of width of ~a in a box of width ~a")
                                        (+ max-width spaces-between)
                                        box-width))
                       (use-value (value)
                         (pad value add-space-left))
                       (truncate  ()
                         (pad (funcall truncate-restart-fn batch)
                              add-space-left)))
                     (let ((column-width (funcall column-width-fn column)))
                       (values (pad-height column) column-width)))))
             (fit (line-index-from line-index-to &optional (width-so-far 0))
               (if (>= line-index-from lines-length) ; perfectly fitted
                   (setf rest-lines-index nil)
                   (let* ((column-height      (min line-index-to lines-length))
                          (batch              (subseq lines
                                                      line-index-from
                                                      column-height)))
                     (multiple-value-bind (column column-width)
                         (pad batch (/= line-index-from 0))
                       (when (<= (+ width-so-far column-width)
                                 box-width)
                         (incf rest-lines-index (length column))
                         (push column columns)
                         (fit column-height
                              (+ column-height
                                 box-height)
                              (+ width-so-far
                                 column-width))))))))
      (fit 0 box-height)
      (values (reverse columns)
              (and rest-lines-index
                   (<= rest-lines-index
                       lines-length)
                   rest-lines-index)))))

(defun box-fit-multiple-column (lines box-width box-height &key (spaces-between 1))
  "Given 'lines' as list of strings  this procedure will fits them in a
box of width and height passed as parameters ('box-width' and 'box-height').

   Example:

   Input: '(line1 line2 line3 line4 line5 line6 line7 line8 line9 line10 line11 line12)

     spaces-between                      spaces-between
         |---|                               |---|
   +----------------+  -               +----------------+  -
   |line1     line6 |  |               |line11          |  |
   |line2     line7 |  | box-height    |line12          |  | box-height
   |line3     line8 |  |               |                |  |
   |line4     line9 |  |               |                |  |
   |line5     line10|  |               |                |  |
   +----------------+  -               +----------------+  -

   |-----------|                       |-----------|
     box-width                           box-width

Returns a  list of  fitted columns  each element of  this list  can be
printed in the box column by column; in the example above the results are:

(((\"line1\" \"line2\" \"line3\" \"line4\" \"line5\")
  (\" line6 \" \" line7 \" \" line8 \" \" line9 \" \" line10\"))
 ((\"line11\" \"line12\" \"      \" \"      \" \"      \")))

"
  (labels ((fit ()
             (multiple-value-bind (columns rest-index)
                 (box-fit-as-much-lines-columns lines box-width
                                                box-height
                                                :spaces-between spaces-between)
               (if rest-index
                   (append (list columns)
                           (box-fit-multiple-column (subseq lines rest-index)
                                                    box-width
                                                    box-height))
                   (list columns)))))
    (fit)))

(defun annotated-text-symbol (a)
  (car a))

(defun annotated-text-value (a)
  (cdr a))

(defun annotated-value-max-line-length (a)
  (find-max-line-length (mapcar #'annotated-text-value a)))

(defun cat-annotated-values (line)
  (strcat* (mapcar #'annotated-text-value line)))

(defun pad-annotated-batch-clsr (box-width spaces-between)
  "return (lambda (batch) ...)
  where batch is:
  '(((:a . string) (:a . string)) ; line1
    ((:a . string) (:a . string)  ; line2
    ...
 Note: this function needs to be improved.
"
  (let ((width (- box-width spaces-between)))
    (lambda (batch)
      (labels ((length-fitted (line)
                 (length (cat-annotated-values line)))
               (cut-last (line)
                 (let ((shrinked (coerce (misc:safe-all-but-last-elt (cdr (alexandria:last-elt line)))
                                         'string)))
                   (setf (cdr (alexandria:last-elt line))
                         shrinked)
                   (remove-if (lambda (a)
                                (let((string (cdr a)))
                                  (string-empty-p string)))
                              line)))
               (fit-line (line)
                 (cond
                   ((null line)
                    (restart-case
                        (error (_ "Unrecoverable error: ~a can not be fitted in a box of width ~a")
                               batch width)
                      (use-value (value) value)))
                   ((<= (length-fitted line)
                        width)
                    line)
                   (t
                    (fit-line (cut-last line))))))
        (loop for line in batch collect
             (fit-line line))))))

(defun box-fit-multiple-column-annotated (lines box-width box-height &key (spaces-between 1))
  "Same as  box-fit-multiple-column but each  element of 'lines'  is a
  list of cons cell:

     '(((:a . string) (:a . string)) ; line1
       ((:a . string) (:a . string)) ; line2
        ...
"
  (labels ((pad-right  (line max-width)
             (let* ((raw-string  (cat-annotated-values line))
                    (diff        (- max-width
                                    (length raw-string)))
                    (last-string (cdr (alexandria:last-elt line))))
               (when (> diff 0)
                 (setf (cdr (alexandria:last-elt line))
                       (strcat last-string
                               (build-string diff))))
               line))
           (pad-left (line)
             (let ((first-string (cdr (first line))))
               (setf (cdr (first line))
                     (strcat (build-string spaces-between)
                             first-string))
               line))
           (find-max-line-length (lines)
             (let ((all-strings (mapcar #'cat-annotated-values
                                        lines)))
               (reduce #'max
                       (mapcar #'length all-strings))))
           (build-pad-line (column-width)
             (list (cons :padding
                         (build-string column-width))))
           (column-width (column)
             (length (cat-annotated-values (first column))))
           (fit ()
             (multiple-value-bind (columns rest-index)
                 (box-fit-as-much-lines-columns lines box-width
                                                box-height
                                                :pad-right-fn            #'pad-right
                                                :pad-left-fn             #'pad-left
                                                :find-max-line-length-fn #'find-max-line-length
                                                :build-pad-line-fn       #'build-pad-line
                                                :column-width-fn         #'column-width
                                                :truncate-restart-fn
                                                (pad-annotated-batch-clsr box-width spaces-between)
                                                :spaces-between spaces-between)
               (if rest-index
                   (append (list columns)
                           (box-fit-multiple-column-annotated (subseq lines rest-index)
                                                              box-width
                                                              box-height))
                   (list columns)))))
    (fit)))

(defun collect-links (text &optional (schemes '("http" "https" "ftp" "gemini")))
  "Collect all hyperlinks in a text marked from a list of valid `schemes'"
  (flet ((build-re-scheme ()
           (let ((res ""))
             (loop for (scheme . rest) on schemes do
                  (if rest
                      (setf res (strcat res "(" scheme ")|"))
                      (setf res (strcat res "(" scheme ")://"))))
             (strcat "(" res ")"))))
    (let* ((results ())
           (re      (strcat (build-re-scheme) "\\P{White_Space}+"))
           (words   (split-words text))
           (scanner (cl-ppcre:create-scanner re)))
      (loop for word in words when (cl-ppcre:scan scanner word) do
           (pushnew (cl-ppcre:scan-to-strings scanner word)
                    results
                    :test #'string=))
      results)))

(defun percent-encode (string)
  (percent-encoding:encode string :encoding :utf-8))

(defun percent-decode (string)
  (percent-encoding:decode string :encoding :utf-8))

(defun percent-decode-allow-null (data)
  (when data
    (percent-decode data)))

(defun percent-encoded-p (string)
  (loop for i in (coerce string 'list)
        for ct from 0 do
          (cond
            ((char= i #\%)
             (when (not (cl-ppcre:scan "(?i)^%[0123456789abcdef]{2}" string :start ct))
               (return-from percent-encoded-p nil)))
            ((or (percent:reservedp i)
                 (char= i #\Space)
                 (not (or (percent:alphap (char-code i))
                          (percent:digitp (char-code i)))))
             (return-from percent-encoded-p nil))))
  t)

(defun percent-encode-allow-null (data)
  (when data
    (percent-encode data)))

(defun maybe-percent-encode (data)
  "Note that when data is null this function returns nil"
  (if (percent-encoded-p data)
      data
      (percent-encode-allow-null data)))
