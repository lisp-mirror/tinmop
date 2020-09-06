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

(in-package :mtree-tests)

(defsuite mtree-suite (all-suite))

(defun tree-sample ()
  (let ((tree        (make-instance 'm-tree :data 2))
        (subtree     (make-instance 'm-tree :data 4))
        (sub-subtree (make-instance 'm-tree :data 3)))
    (add-child tree (make-instance 'm-tree :data 8))
    (add-child tree (make-instance 'm-tree :data 16))
    (add-child subtree sub-subtree)
    (add-child tree subtree)
    tree))

(deftest test-find-3 (mtree-suite)
  (assert-equality #'=
      3
      (data (find-child (tree-sample) 3 :compare #'=))))

(deftest test-find-if-odd (mtree-suite)
  (assert-equality #'=
      3
      (data (first (find-child-if (tree-sample)
                                  #'(lambda (a) (oddp (data a))))))))

(deftest test-sorted-instance (mtree-suite)
  (let ((sorted (make-instance 'sorted-m-tree
                               :data     -1
                               :children (vector (make-instance 'sorted-m-tree
                                                                :data 5)
                                                 (make-instance 'sorted-m-tree
                                                                :data 4)
                                                 (make-instance 'sorted-m-tree
                                                                :data 3))))
        (simple (make-instance 'sorted-m-tree
                               :data     -1
                               :children (vector (make-instance 'sorted-m-tree
                                                                :data 3)
                                                 (make-instance 'sorted-m-tree
                                                                :data 4)
                                                 (make-instance 'sorted-m-tree
                                                                :data 5)))))
    (assert-true (mtree-equal sorted simple))))

(deftest test-sorted-add-child (mtree-suite)
  (let ((sorted (make-instance 'sorted-m-tree
                               :data     -1))
        (simple (make-instance 'sorted-m-tree
                               :data     -1
                               :children (vector (make-instance 'sorted-m-tree
                                                                :data 3)
                                                 (make-instance 'sorted-m-tree
                                                                :data 4)
                                                 (make-instance 'sorted-m-tree
                                                                :data 5)
                                                 (make-instance 'sorted-m-tree
                                                                :data 6)))))
    (add-child sorted (make-instance 'sorted-m-tree :data 6))
    (add-child sorted (make-instance 'sorted-m-tree :data 5))
    (add-child sorted (make-instance 'sorted-m-tree :data 4))
    (add-child sorted (make-instance 'sorted-m-tree :data 3))
    (assert-true (mtree-equal sorted simple))))


(defun graft-tree-test ()
  (make-instance 'm-tree
                 :data 'a
                 :children (vector (make-instance 'm-tree
                                                  :data 'b)
                                   (make-instance 'm-tree
                                                  :data 'c
                                                  :children (vector (make-instance 'm-tree
                                                                                   :data 'd))))))

(defun graft-tree-branch (start &rest children)
  (if (null children)
      (root-node start)
      (let ((child (make-instance 'm-tree
                            :data (first children))))
        (add-child start child)
        (apply #' graft-tree-branch child (rest children)))))

;;   a             a             a
;;  / \       +    |    ->      / \
;; b   c           c           b   c
;;      \          |              / \
;;       d         e             e   d

(defun graft-1 ()
  (let ((rootstock (graft-tree-test))
        (scion     (graft-tree-branch (make-instance 'mtree:m-tree :data 'a) 'c 'e)))
    (graft-branch rootstock scion)))

(deftest test-graft-1 (mtree-suite)
  (assert-equality #'mtree-equal
      (graft-1)
      (make-instance 'm-tree
                     :data     'a
                     :children
                     (vector (make-instance 'm-tree
                                            :data 'b)
                             (make-instance 'm-tree
                                            :data 'c
                                            :children
                                            (vector (make-instance 'm-tree
                                                                   :data 'd)
                                                    (make-instance 'm-tree
                                                                   :data 'e)))))))


;;   a             a             a
;;  / \       +    |    ->      / \
;; b   c           c           b   c
;;                 |              /
;;                 e             e

(defun graft-2 ()
  (let ((rootstock (make-instance 'm-tree
                                  :data     'a
                                  :children (vector (make-instance 'm-tree
                                                                   :data 'b)
                                                    (make-instance 'm-tree
                                                                   :data 'c))))
        (scion     (graft-tree-branch (make-instance 'mtree:m-tree :data 'a) 'c 'e)))
    (graft-branch rootstock scion)))

(deftest test-graft-2 (mtree-suite)
  (assert-equality #'mtree-equal
      (graft-2)
      (make-instance 'm-tree
                     :data 'a
                     :children
                     (vector (make-instance 'm-tree
                                            :data 'b)
                             (make-instance 'm-tree
                                            :data 'c
                                            :children
                                            (vector (make-instance 'm-tree
                                                                   :data 'e)))))))

;;   a             a             a------+
;;  / \       +    |    ->      / \     |
;; b   c           d           b   c    d
;;      \          |                \   |
;;       d         e                 d  e

(defun graft-3 ()
  (let ((rootstock (graft-tree-test))
        (scion     (graft-tree-branch (make-instance 'mtree:m-tree :data 'a) 'd 'e)))
    (graft-branch rootstock scion)))

(deftest test-graft-3 (mtree-suite)
  (assert-equality #'mtree-equal
      (graft-3)
      (make-instance 'm-tree
                     :data 'a
                     :children
                     (vector (make-instance 'm-tree
                                            :data 'b)
                             (make-instance 'm-tree
                                            :data 'c
                                            :children
                                            (vector (make-instance 'm-tree
                                                                   :data 'd)))
                             (make-instance 'm-tree
                                            :data 'd
                                            :children (vector (make-instance 'm-tree
                                                                             :data 'e)))))))

;;   a             b             a
;;  / \       +    |    ->      / \
;; b   c           c           b   c
;;      \          |                \
;;       d         e                 d

(defun graft-4 ()
  (let ((rootstock (graft-tree-test))
        (scion     (graft-tree-branch (make-instance 'mtree:m-tree :data 'b) 'c 'e)))
    (graft-branch rootstock scion)))

(deftest test-graft-4 (mtree-suite)
  (assert-equality #'mtree-equal
      (graft-4)
      (graft-tree-test)))

;;   a             a             a
;;  / \       +    |    ->      / \
;; b   c           c           b   c
;;      \          |                \
;;       d         d                 d
;;        \        |                  \
;;         #'+     #'-                 #'-

(defun graft-5 ()
  (let ((rootstock
         (make-instance 'm-tree
                        :data 'a
                        :children
                        (vector (make-instance 'm-tree
                                               :data 'b)
                                (make-instance 'm-tree
                                               :data 'c
                                               :children
                                               (vector (make-instance 'm-tree
                                                                      :data 'd
                                                                      :children
                                                                      (vector
                                                                       (make-instance 'm-tree
                                                                                      :data #'+))))))))
        (scion     (graft-tree-branch (make-instance 'mtree:m-tree :data 'a) 'c 'd #'-)))
    (graft-branch rootstock scion
                  :test (lambda (a b)
                          (cond
                            ((functionp a)
                             t)
                            (t
                             (string= a b)))))))

(deftest test-graft-5 (mtree-suite)
  (assert-equality #'mtree-equal
      (graft-5)
      (make-instance 'm-tree
                     :data 'a
                     :children
                     (vector (make-instance 'm-tree
                                            :data 'b)
                             (make-instance 'm-tree
                                            :data 'c
                                            :children
                                            (vector (make-instance 'm-tree
                                                                   :data 'd
                                                                   :children
                                                                   (vector
                                                                    (make-instance 'm-tree
                                                                                   :data #'-)))))))))

;;   a             a             a
;;  / \       +    |    ->      / \
;; b   c           c           b   c
;;    /            |              / \
;;   b             d             b   d
;;                 |                  \
;;                 #'-                 #'-

(defun graft-6 ()
  (let ((rootstock (make-instance 'm-tree
                                  :data 'a
                                  :children
                                  (vector (make-instance 'm-tree
                                                         :data 'b)
                                          (make-instance 'm-tree
                                                         :data 'c
                                                         :children
                                                         (vector (make-instance 'm-tree
                                                                                :data 'b))))))
        (scion     (graft-tree-branch (make-instance 'mtree:m-tree :data 'a) 'c 'd #'-)))
    (graft-branch rootstock scion
                  :test (lambda (a b)
                          (cond
                            ((functionp a)
                             t)
                            (t
                             (string= a b)))))))

(deftest test-graft-6 (mtree-suite)
  (assert-equality #'mtree-equal
      (graft-6)
      (make-instance 'm-tree
                     :data 'a
                     :children
                     (vector (make-instance 'm-tree
                                            :data 'b)
                             (make-instance 'm-tree
                                            :data 'c
                                            :children
                                            (vector (make-instance 'm-tree
                                                                   :data 'b)
                                                    (make-instance 'm-tree
                                                                   :data 'd
                                                                   :children
                                                                   (vector (make-instance 'm-tree
                                                                                          :data #'-)))))))))

;;   a             a             a
;;  / \       +    |    ->      / \
;; b   c           c           b   c
;;      \          |                \
;;       d         #-                #-
;;        \                           \
;;         e                           e

(defun graft-7 ()
  (let ((rootstock (graft-tree-branch (make-instance 'mtree:m-tree :data 'a) 'c 'd 'e))
        (scion     (graft-tree-branch (make-instance 'mtree:m-tree :data 'a) 'c #'-)))
    (add-child rootstock (make-instance 'mtree:m-tree :data 'b))
    (graft-branch rootstock
                  scion
                  :test (lambda (a b)
                          (cond
                            ((or (functionp a)
                                 (functionp b))
                             t)
                            (t
                             (string= a b)))))))

(deftest test-graft-7 (mtree-suite)
  (let ((probe (graft-tree-branch (make-instance 'mtree:m-tree :data 'a) 'c #'- 'e)))
    (add-child probe (make-instance 'mtree:m-tree :data 'b))
    (assert-equality #'mtree-equal
        (graft-7)
        probe)))



;;   a             a             a
;;  / \       +    |    ->      / \
;; b   c           c           b   c
;;      \          |              / \
;;       d         x             x   d
;;        \                           \
;;         e                           e

(defun graft-8 ()
  (let ((rootstock (graft-tree-branch (make-instance 'mtree:m-tree :data 'a) 'c 'd 'e))
        (scion     (graft-tree-branch (make-instance 'mtree:m-tree :data 'a) 'c 'x)))
    (add-child rootstock (make-instance 'mtree:m-tree :data 'b))
    (graft-branch rootstock
                  scion
                  :test (lambda (a b)
                          (cond
                            ((or (functionp a)
                                 (functionp b))
                             t)
                            (t
                             (string= a b)))))))

(deftest test-graft-8 (mtree-suite)
  (let ((probe (graft-tree-branch (make-instance 'mtree:m-tree :data 'a) 'c 'd 'e)))
    (add-child probe (make-instance 'mtree:m-tree :data 'b))
    (add-child (find-child probe 'c)
               (make-instance 'mtree:m-tree :data 'x))
    (assert-equality #'mtree-equal
        (graft-8)
        probe)))
