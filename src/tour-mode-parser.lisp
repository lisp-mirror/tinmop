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
;; along with this program.
;; If not, see [[http://www.gnu.org/licenses/][http://www.gnu.org/licenses/]].

(in-package :tour-mode-parser)

(define-constant +-separator+ "/" :test #'string=)

(defrule digit (character-ranges (#\0 #\9))
  (:text t))

(defrule range-delimter #\-)

(defrule list-delimiter (+ (or #\Space #\,)))

(defrule number (and digit (* digit))
  (:text t)
  (:function parse-integer))

(defstruct range from to)

(defrule range (and number range-delimter number)
  (:function (lambda (a) (make-range :from (first a) :to (third a)))))

(defrule tour-tail (? (and list-delimiter tour))
  (:function rest))

(defrule tour (and (or range number) tour-tail)
  (:function flatten))

(defun parse-tour-mode (data)
  (parse 'tour data))
