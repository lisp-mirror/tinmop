;; tinmop: an humble gemini and pleroma client
;; Copyright (C) 2018  cage

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

(in-package :modeline-window)

(defclass modeline-window (wrapper-window)
  ((mapping-code->fn
    :initform ()
    :initarg :mapping-code->fn
    :accessor mapping-code->fn)
   (modeline-src
    :initform ""
    :initarg  :modeline-text-src
    :accessor modeline-src)
   (modeline-text
    :initform (_ "modeline")
    :initarg  :modeline-text
    :accessor modeline-text)
   (modeline-fg
    :initform nil
    :initarg  :modeline-fg
    :accessor modeline-fg)
   (modeline-bg
    :initform nil
    :initarg  :modeline-bg
    :accessor modeline-bg)
   (parsed-modeline
    :initform ()
    :initarg  :parsed-modeline
    :accessor parsed-modeline)))

(defgeneric expand-modeline-spec (object))

;; MODELINE := (FIELD | TEXT)*
;; FIELD    := PERCENT KEY
;; KEY      := [%abcdefghilmnopqrstuvzABCDEFGHILMNOPQRSTUVZ']
;; PERCENT  := '%'
;; TEXT     := (not percent)+

(defrule key
    (or #\%
        #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
        #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
        #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
        #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)
  (:text t))

(defrule percent #\%
  (:constant :key))

(defrule field (and percent key))

(defrule text (+ (not percent))
  (:text t))

(defrule modeline (* (or field text)))

(defun call-function-mapped (win key mapping)
  (let ((fn (cdr (assoc key mapping :test #'string=))))
    (if fn
        (funcall fn win)
        (format nil "~~~a" key))))

(defmethod expand-modeline-spec ((object modeline-window))
  (with-accessors ((modeline-src     modeline-src)
                   (modeline-text    modeline-text)
                   (modeline-bg      modeline-bg)
                   (modeline-fg      modeline-fg)
                   (mapping-code->fn mapping-code->fn)
                   (parsed-modeline  parsed-modeline)) object
    ;; parsed is like '("foo" (:key "a") "bar" ...)
    (let ((res    (make-tui-string "")))
      (loop for i in parsed-modeline do
           (let ((executed (cond
                             ((listp i)
                              (call-function-mapped object (lastcar i) mapping-code->fn))
                             (t
                              (make-tui-string i
                                               :fgcolor modeline-fg
                                               :bgcolor modeline-bg)))))
             (setf res (cat-tui-string res executed))))
      (setf res (text-ellipsis res (win-width-no-border object)))
      (setf modeline-text res))))

(defun refresh-modeline-config (win key)
  (with-accessors ((modeline-src     modeline-src)
                   (modeline-text    modeline-text)
                   (modeline-bg      modeline-bg)
                   (modeline-fg      modeline-fg)
                   (mapping-code->fn mapping-code->fn)
                   (parsed-modeline  parsed-modeline)) win
    (multiple-value-bind (bg fg)
        (swconf:modeline-colors key)
      (setf modeline-fg     fg)
      (setf modeline-bg     bg)
      (setf modeline-src    (swconf:modeline-fmt key))
      (setf parsed-modeline (parse 'modeline modeline-src))
      (expand-modeline-spec win))
    win))

(defmethod calculate ((object modeline-window) dt))

(defmethod draw :after ((object modeline-window))
  (with-accessors ((modeline-bg   modeline-bg)
                   (modeline-fg   modeline-fg)
                   (modeline-text modeline-text)) object
    (when-window-shown (object)
      (let* ((line        modeline-text)
             (line-length (text-length line))
             (max-width   (win-width-no-border object)))
        (when (< line-length max-width)
          (setf line
                (cat-tui-string line (build-string (- max-width line-length)))))
        (print-text object line 1 (1- (win-height object)))))))

(defun add-modeline-char-expander (win code fn)
  (assert (stringp   code))
  (assert (functionp fn))
  (with-accessors ((mapping-code->fn mapping-code->fn)) win
    (setf mapping-code->fn
          (acons code fn mapping-code->fn))))
