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
;; along with this program.
;; If not, see <http://www.gnu.org/licenses/>.

(in-package :text-utils-tests)

(defsuite text-utils-suite (all-suite))

(defun column-fit-annotated ()
  (let* ((lines (list (list (cons :a "1") (cons :b "12") (cons :c "1"))
                      (list (cons :a "2") (cons :b "3") (cons :c "4"))
                      (list (cons :a "5") (cons :b "6") (cons :c "7"))))
         (batches     (box-fit-multiple-column-annotated lines  8  2)))
    batches))

(defun column-fit ()
  (let* ((lines   (list "1121"
                        "234"
                        "567"))
         (batches (box-fit-multiple-column lines 9 2)))
    batches))

(deftest column-fit (text-utils-suite)
  (assert-true
      (tree-equal (column-fit)
                  '((("1121 " "234  ") ("567 " "    ")))
                  :test #'string=)))

(deftest column-fit-annotated (text-utils-suite)
  (assert-true
      (tree-equal (column-fit-annotated)
                  '(((((:a . "1") (:b . "12") (:c . "1")) ((:a . "2") (:b . "3") (:c . "4 ")))
                     (((:a . " 5") (:b . "6") (:c . "7")) ((:padding . "    ")))))
                  :test #'string=)))

(deftest match-words (text-utils-suite)
  (assert-true  (match-words '("a" "b" "c") '("a" "b" "c")))
  (assert-true  (match-words '("a" "b" "c" "d") '("a" "b" "c")))
  (assert-true  (match-words '("a" "foo" "bar" "d") '("foo" "bar")))
  (assert-true  (match-words '("a" "b" "c" "d") '("c" "d")))
  (assert-false (match-words '("a" "b" "c" "d") '("b" "a")))
  (assert-false (match-words '("a" "b" "c" "d") '("a" "b" "x")))
  (assert-false (match-words '("a" "b" "c" "d") '("a" "b" "c" "d" "e"))))
