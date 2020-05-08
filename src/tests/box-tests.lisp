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

(in-package :box-tests)

(defsuite box-suite (all-suite))

(defun shared-boxes ()
  (let ((contents (vector nil)))
    (values (box contents)
            (box contents))))

(deftest test-unbox (box-suite)
  (assert-true (numberp (unbox 1)))
  (assert-true (= (unbox 1) (unbox 1))))

(deftest test-shared (box-suite)
  (multiple-value-bind (box-1 box-2)
      (shared-boxes)
    (assert-false (eq box-1 box-2))
    (assert-true  (eq (unbox box-1) (unbox box-2)))
    (assert-true  (eq (unbox 1) (unbox 1)))))

(defun shared-dboxes ()
  (let ((contents (vector 'a)))
    (values (dbox contents)
            (dbox contents))))

(deftest test-dbox-shared (box-suite)
  (multiple-value-bind (dbox-1 dbox-2)
      (shared-dboxes)
    (assert-false (eq dbox-1 dbox-2))
    (assert-true  (eq (dunbox dbox-1) (dunbox dbox-2)))
    (assert-true
        (progn
          (setf (unbox dbox-1) (unbox dbox-2))
          (setf (dunbox dbox-1) "foo")
          (eq (dunbox dbox-1)
              (dunbox dbox-2)))
      (format nil "~a ~a" dbox-1 dbox-2))))
