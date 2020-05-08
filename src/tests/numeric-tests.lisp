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
;; along with this program.
;; If not, see [[http://www.gnu.org/licenses/][http://www.gnu.org/licenses/]].

(in-package :numeric-tests)

(defsuite numeric-suite (all-suite))

(deftest test-shellsort (numeric-suite)
  (assert-true
      (let* ((bag    (loop repeat 10000 collect (lcg-next-upto 10000)))
             (sorted (shellsort bag #'<)))
        (and (null (set-difference bag sorted :test #'=))
             (not (find nil (mapcar #'< sorted (rest sorted))))))))

(deftest test-multisort (numeric-suite)
  (let* ((bag     '(("b" . 10) ("b" . 1) ("z" . 5) ("a" . 10) ("b" . 7) ("a" . 100)))
         (sorted (multisort bag (list (gen-multisort-test string< string> car)
                                      (gen-multisort-test <       >       cdr)))))
    (assert-equalp
        '(("a" . 10) ("a" . 100) ("b" . 1)  ("b" . 7) ("b" . 10) ("z" . 5))
        sorted)))

(deftest test-binary-search (numeric-suite)
  (let* ((max-value  1000)
         (all-values (loop for i from 0 below (num:lcg-next-upto max-value) collect i))
         (results    ()))
    (loop repeat 10 do
         (let* ((needle (misc:random-elt all-values))
                (result (binary-search all-values needle)))
           (push result results)))
    (assert-true (notany #'null results))
    (assert-true (binary-search '(1 2) 2))
    (assert-true (binary-search '(1) 1))
    (assert-false (binary-search '(1) 2))
    (assert-false (binary-search '(1 2) 3))
    (assert-false (binary-search '(2 3) 1))
    (assert-false (binary-search '(2 4 5) 3))
    (assert-false (binary-search '(2 4 5) 1))
    (assert-false (binary-search all-values (1+ max-value)))))
