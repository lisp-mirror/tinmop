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

(in-package :gemini-parser-tests)

(defsuite gemini-parser-suite (all-suite))

(defun normalize (path expected)
  (string= (gemini-parser::normalize-path path)
           expected))

(deftest test-normalize-path (gemini-parser-suite)
  (assert-true (normalize "/a/x" "/a/x"))
  (assert-true (normalize "/a/../b/x" "/b/x"))
  (assert-true (normalize "/a/../b/x/.." "/b/x/"))
  (assert-true (normalize "/a/../b/x/." "/b/x/")))
