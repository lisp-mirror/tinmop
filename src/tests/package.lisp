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

(defpackage :all-tests
  (:use :cl
        :clunit)
  (:export
   :all-suite
   :run-all-tests))

(defpackage :misc-tests
  (:use :cl
        :alexandria
        :clunit
        :misc
        :all-tests)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export))

(defpackage :box-tests
  (:use :cl
        :alexandria
        :clunit
        :box
        :all-tests)
  (:export))

(defpackage :numeric-tests
  (:use :cl
        :clunit
        :num-utils
        :all-tests)
  (:export))

(defpackage :mtree-tests
  (:use :cl
        :clunit
        :all-tests
        :mtree)
  (:export))

(defpackage :text-utils-tests
  (:use :cl
        :clunit
        :all-tests
        :text-utils)
  (:export))

(defpackage :program-events-tests
  (:use :cl
        :clunit
        :misc
        :box
        :all-tests
        :program-events)
  (:export))

(defpackage :thread-window-tests
  (:use :cl
        :clunit
        :misc
        :text-utils
        :all-tests
        :thread-window)
  (:export))

(defpackage :gemini-parser-tests
  (:use :cl
        :clunit
        :misc
        :text-utils
        :all-tests
        :gemini-parser)
  (:export))
