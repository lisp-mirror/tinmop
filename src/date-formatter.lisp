;; tinmop: an humble mastodon client
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

(in-package :date-formatter)

;; FORMAT   := (FIELD | TEXT)*
;; FIELD    := [%hour %minute %second %month %year %day %weekday %%]
;; TEXT     := (not percent)+

(defrule field
    (or "%hour"
        "%min"
        "%second"
        "%month"
        "%year"
        "%day"
        "%weekday"
        "%short-weekday"
        "%long-weekday"
        "%long-month"
        "%short-month"
        "%%")
  (:text t))

(defrule text (+ (not percent))
  (:text t))

(defrule format (* (or field text)))

(defun expand-date-formatter-spec (spec)
  "Expand a date spec like '%year %short-month %day %hour:%min'
   to  a list like '(:year :dhort-month :day :hour \":\" (:min 2))

note that:
- %% expands to \"%\"
- %min  expands to (:min 2)
- %hour expands to (:hour 2)
- %day   expands to (:day 2)

This list can be passed to misc:format-time to get a time string"
  (let ((parsed (parse 'format spec)))
    (loop for element in parsed collect
         (cond
           ((string= element "%%")
            "%")
           ((string= element "%min")
            '(:min 2))
           ((string= element "%hour")
            '(:hour 2))
            ((string= element "%day")
            '(:day 2))
           ((scan "^%+" element)
            (make-keyword (string-upcase (subseq element 1))))
           (t
            element)))))
