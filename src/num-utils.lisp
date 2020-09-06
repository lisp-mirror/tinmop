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
;; If not, see [[http://www.gnu.org/licenses/][http://www.gnu.org/licenses/]].

;; NOTE: any random function are not for crypto use!

(in-package :num-utils)

(defun safe-parse-number (maybe-number &key (fix-fn #'(lambda (e) (declare (ignore e)) nil)))
  (handler-bind ((error
                   (lambda(e)
                     (return-from safe-parse-number (funcall fix-fn e)))))
    (if (or (not (stringp maybe-number))
            (string= maybe-number "-"))
        (error "Paring a non string element")
        (parse-number:parse-number maybe-number))))

(defun parse-number-default (maybe-number default)
  (safe-parse-number maybe-number
                     :fix-fn (lambda (e) (declare (ignore e)) default)))

(defun find-min-max (function the-list)
  (restart-case
      (reduce #'(lambda (a b) (if (funcall function a b) a b)) the-list)
    (use-value (e) e)))

(defun find-min (the-list)
  (find-min-max #'< the-list))

(defun find-max (the-list)
  (find-min-max #'> the-list))

(defgeneric round-all (object &key rounding-function))

(defmethod round-all ((object list) &key (rounding-function #'round))
  (mapcar #'(lambda (n) (funcall rounding-function n)) object))

(defmethod round-all ((object number) &key (rounding-function #'round))
  (funcall rounding-function object))

(defmethod round-all ((object vector) &key (rounding-function #'round))
  (map (type-of object) #'(lambda (n) (funcall rounding-function n)) object))

(defun fract (n)
  (multiple-value-bind (int frac)
      (truncate n)
    (declare (ignore int))
    frac))

(defun sign (n)
  (if (< n 0)
      -1
      1))

(defun count-digit (number &optional (so-far 1))
  (let ((reduced (truncate (/ number 10))))
    (if (= reduced 0)
        so-far
        (count-digit reduced (1+ so-far)))))

(alexandria:define-constant +fnv-prime-32+ 16777619 :test #'=)

(alexandria:define-constant +fnv-offset-basis-32+ 2166136261 :test #'=)

(defun fnv-hash-32 (octects)
  (let ((hash +fnv-offset-basis-32+))
    (loop for i across octects do
         (setf hash (boole boole-xor hash i))
         (setf hash (ldb (byte 32 0) (* hash +fnv-prime-32+))))
    hash))

(defun string-fnv-hash-32 (s)
  (fnv-hash-32 (map 'vector #'char-code (coerce s 'list))))

(alexandria:define-constant +fnv-prime-256+
    374144419156711147060143317175368453031918731002211 :test #'=)

(alexandria:define-constant +fnv-offset-basis-256+
    100029257958052580907070968620625704837092796014241193945225284501741471925557
  :test #'=)

(defun fnv-hash-256 (octects)
  (let ((hash +fnv-offset-basis-256+))
    (loop for i across octects do
         (setf hash (boole boole-xor hash i))
         (setf hash (ldb (byte 256 0) (* hash +fnv-prime-256+))))
    hash))

(defun string-fnv-hash-256 (s)
  (fnv-hash-256 (map 'vector #'char-code (coerce s 'list))))

(alexandria:define-constant +lcg-modulo-pow+ 64 :test #'=)

(alexandria:define-constant +lcg-good-bit-starts+ 32 :test #'=)

(alexandria:define-constant +lcg-good-bit-size+ 32 :test #'=)

(alexandria:define-constant +lcg-modulo+ 18446744073709551616 :test #'=)

(alexandria:define-constant +lcg-max+ 4294967295 :test #'=)

(alexandria:define-constant +lcg-a+ 3935559000370003845 :test #'=)

(alexandria:define-constant +lcg-c+ 2691343689449507681 :test #'=)

(defparameter *lcg-seed* 0)

(defun lcg-set-seed (&optional (seed (get-universal-time)))
  (setf *lcg-seed* seed))

(defun lcg-next ()
  (setf *lcg-seed*
        (ldb (byte +lcg-modulo-pow+ 0)
             (+ (* +lcg-a+ *lcg-seed*) +lcg-c+)))
  (ldb (byte +lcg-good-bit-size+ +lcg-good-bit-starts+) *lcg-seed*))

(defun lcg-next01 ()
  (coerce (/ (lcg-next) +lcg-max+)
          'float))

(defgeneric lcg-next-upto (max))

(defmethod lcg-next-upto ((max float))
  (multiple-value-bind (integer-part remainder)
      (truncate max)
    (coerce (+ (* (lcg-next01) integer-part) (* (lcg-next01) remainder))
            'float)))

(defmethod lcg-next-upto ((max integer))
  (mod (lcg-next) max))

(defmethod lcg-next-upto ((max ratio))
  (lcg-next-upto (float max)))

(defun lcg-next-in-range (from to)
  (+ (lcg-next-upto (- to from)) from))

(defun lcg-next-in-range* (range)
  "range is a cons cell (from . to)"
  (lcg-next-in-range (car range) (cdr range)))

(defmacro with-lcg-seed ((&optional (seed `(get-universal-time))) &body body)
  `(let ((*lcg-seed* ,seed))
     ,@body))

(defun get-random-float-sign ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if (= (the integer (lcg-next-upto 2)) 0) 1.0 -1.0))

(defgeneric shellsort (sequence predicate &key key)
  (:documentation "Note: makes a new sequence"))

(defmethod shellsort ((sequence list) predicate &key (key #'identity))
  (call-next-method (copy-list sequence)
                    predicate
                    :key key))

(defmethod shellsort ((sequence vector) predicate &key (key #'identity))
  (call-next-method (alexandria:copy-array sequence)
                    predicate
                    :key key))

(defun tokuda-sequence (n)
  (do ((k 1 (1+ k))
       (h 1.0 (+ (* 2.25 h) 1.0))
       (res '()))
      ((not (< h n)) res)
    (push (ceiling h) res)))

(defmethod shellsort (sequence predicate &key (key #'identity))
  (loop for gap in (tokuda-sequence (length sequence)) do
       (loop for i from gap below (length sequence) by 1 do
            (let ((tmp (elt sequence i)))
              (do ((j   i (- j gap)))
                  ((not (and (>= j gap)
                              (not (funcall predicate
                                            (funcall key (elt sequence (- j gap)))
                                            (funcall key tmp)))))
                   (setf (elt sequence j) tmp))
                (let ((swp (elt sequence (- j gap))))
                  (setf (elt sequence j) swp))))))
  sequence)

(defun multisort (bag fns)
  (shellsort bag
             #'(lambda (a b)
                 (let ((partial (loop named outer for fn in fns do
                                     (cond
                                       ((< (funcall fn a b) 0)
                                        (return-from outer t))
                                       ((> (funcall fn a b) 0)
                                        (return-from outer nil))))))
                   partial))))

(defun multisort* (bag &rest fns)
  (multisort bag fns))

(defmacro gen-multisort-test (fn-< fn-> fn-access)
  (alexandria:with-gensyms (a b access-a access-b)
    `(lambda (,a ,b)
       (let ((,access-a (funcall (misc:fn-delay ,fn-access) ,a))
             (,access-b (funcall (misc:fn-delay ,fn-access) ,b)))
         (cond
           ((funcall (misc:fn-delay ,fn-<) ,access-a ,access-b)
            -1)
           ((funcall (misc:fn-delay ,fn->) ,access-a ,access-b)
            1)
           (t 0))))))

(defparameter *default-epsilon* 1e-7)

(defmacro with-epsilon ((epsilon) &body body)
  `(let ((*default-epsilon* ,epsilon))
     ,@body))

(defun add-epsilon-rel (v &optional (epsilon *default-epsilon*))
  (+ v (* epsilon v)))

(defun epsilon<= (a b &optional (epsilon *default-epsilon*))
  (or (<= a b)
      (epsilon= a b epsilon)))

(defun epsilon>= (a b &optional (epsilon *default-epsilon*))
  (or (>= a b)
      (epsilon= a b epsilon)))

(defun epsilon= (a b &optional (epsilon *default-epsilon*))
  (and (<= (- b epsilon) a (+ b epsilon))))

(defun binary-search (sequence value-looking-for
                      &key
                        (compare-fn  #'<)
                        (equal-fn    #'=)
                        (left-limit  0)
                        (right-limit (1- (length sequence))))
  "Perform    a   binary    search   on    `sequence'   looking    for
  `value-looking-for' using  `equal-fn' as equality test  function and
  `compare-fn' as comparing function.  Values position where the value
  has been found  in `sequence' or nil.  `sequence' must  be sorted in
  ascending order using the same predicate as `compare-fn'.  Recursive."
  (when (not (misc:sequence-empty-p sequence))
    (assert (<  right-limit (length sequence)))
    (assert (>= left-limit 0))
    (assert (<  left-limit (length sequence)))
    (flet ((equals (element)
             (funcall equal-fn element value-looking-for))
           (less-than (a b)
             (funcall compare-fn a b)))
      (cond
        ((>= left-limit right-limit)
         (if (equals (elt sequence left-limit))
             left-limit
             nil))
        (t
         (let* ((midpoint        (floor (+ left-limit
                                           (/ (- right-limit left-limit)
                                              2))))
                (mid-point-value (elt sequence midpoint)))
           (cond
             ((equals mid-point-value)
              midpoint)
             ((less-than value-looking-for mid-point-value)
              (binary-search sequence
                             value-looking-for
                             :compare-fn  compare-fn
                             :equal-fn   equal-fn
                             :left-limit  left-limit
                             :right-limit (1- midpoint)))
             (t
              (binary-search sequence
                             value-looking-for
                             :compare-fn  compare-fn
                             :equal-fn   equal-fn
                             :left-limit  (1+ midpoint)
                             :right-limit right-limit)))))))))
