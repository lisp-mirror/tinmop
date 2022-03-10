;; tinmop: an humble gemini and pleroma client
;; Copyright (C) 2022  cage

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

(in-package :2d-utils)

(defun iaabb2-min-x (aabb)
  (elt aabb 0))

(defun iaabb2-max-x (aabb)
  (elt aabb 2))

(defun iaabb2-min-y (aabb)
  (elt aabb 1))

(defun iaabb2-max-y (aabb)
  (elt aabb 3))

(defun make-iaabb2 (min-x min-y max-x max-y)
  (list min-x min-y max-x max-y))

(defun iaabb2~ (a b)
  (and
   (= (elt a 0) (elt b 0))
   (= (elt a 1) (elt b 1))
   (= (elt a 2) (elt b 2))
   (= (elt a 3) (elt b 3))))

(defun valid-iaabb2 (aabb)
  (and (>= (elt aabb 0) 0)
       (>= (elt aabb 1) 0)
       (>= (elt aabb 2) 0)
       (>= (elt aabb 3) 0)
       (> (elt aabb 2) (elt aabb 0))
       (> (elt aabb 3) (elt aabb 1))))

(defun expand-iaabb2 (aabb coord)
  (let ((cp (copy-list aabb)))
    (when (< (elt coord 0) (elt aabb 0))
      (setf (elt cp 0) (elt coord 0)))
    (when (> (elt coord 0) (elt aabb 2))
      (setf (elt cp 2) (elt coord 0)))
    (when (< (elt coord 1) (elt aabb 1))
      (setf (elt cp 1) (elt coord 1)))
    (when (> (elt coord 1) (elt aabb 3))
      (setf (elt cp 3) (elt coord 1)))
    cp))

(defun union-iaabb2 (aabb aabb2)
  (let ((cp (copy-list aabb)))
    (setf cp (expand-iaabb2 cp (subseq aabb2 0 2)))
    (setf cp (expand-iaabb2 cp (list (elt aabb2 2) (elt aabb2 1))))
    (setf cp (expand-iaabb2 cp (list (elt aabb2 2) (elt aabb2 3))))
    (setf cp (expand-iaabb2 cp (list (elt aabb2 0) (elt aabb2 3))))
    cp))

(defun iaabb2->irect2 (coords)
  "(upper-left-x upper-left-y bottom-right-x bottom-right-y) to
   (upper-left-x upper-left-y  w h)"
  (let ((x1 (elt coords 0))
        (y1 (elt coords 1))
        (x2 (elt coords 2))
        (y2 (elt coords 3)))
  (list x1 y1 (- x2 x1) (- y2 y1))))

(defun irect2->iaabb2 (coords)
  "(upper-left-x upper-left-y  w h) to
   (upper-left-x upper-left-y bottom-right-x bottom-right-y)"
  (let ((x1 (elt coords 0))
        (y1 (elt coords 1))
        (w (elt coords 2))
        (h (elt coords 3)))
  (list x1 y1 (+ x1 w) (+ y1 h))))

(defun irect2->iaabb2* (&rest coords)
  (irect2->iaabb2 coords))

(defun inside-iaabb2-p (aabb x y)
  "t if x y is inside this bounding box
   aabb is: (upper-left-x upper-left-y bottom-right-x bottom-right-y)"
  (and
   (>= x (elt aabb 0))
   (<= x (elt aabb 2))
   (>= y (elt aabb 1))
   (<= y (elt aabb 3))))

(defun iaabb2-intersect-p (aabb1 aabb2)
  (if (or (>= (iaabb2-min-x aabb1) (iaabb2-max-x aabb2))
          (<= (iaabb2-max-x aabb1) (iaabb2-min-x aabb2))
          (>= (iaabb2-min-y aabb1) (iaabb2-max-y aabb2))
          (<= (iaabb2-max-y aabb1) (iaabb2-min-y aabb2)))
      nil
      t))

(defun iaabb2-inglobe-p (host guest)
  (and (inside-iaabb2-p host (iaabb2-min-x guest) (iaabb2-min-x guest))
       (inside-iaabb2-p host (iaabb2-max-x guest) (iaabb2-max-x guest))))

(defun iaabb2-null-p (aabb)
  (let ((rect (iaabb2->irect2 aabb)))
    (and (= 0 (elt rect 2))
         (= 0 (elt rect 3)))))

(defun trasl-iaabb2 (aabb &optional (dx (- (elt aabb 0))) (dy (- (elt aabb 1))))
  (list (+ (elt aabb 0) dx)
         (+ (elt aabb 1) dy)
         (+ (elt aabb 2) dx)
         (+ (elt aabb 3) dy)))

(defun trasl-irect2 (rect &optional (dx (- (elt rect 0))) (dy (- (elt rect 1))))
  (list (+ (elt rect 0) dx)
         (+ (elt rect 1) dy)
         (elt rect 2)
         (elt rect 3)))

(defun center-iaabb2 (aabb)
  (let ((rect (iaabb2->irect2 aabb)))
    (list (+ (elt rect 0) (/ (elt rect 2) 2))
          (+ (elt rect 1) (/ (elt rect 3) 2)))))
