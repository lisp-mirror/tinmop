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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package mtree-utils)

;; tree := nil | node
;; node := (list atom node*)
;; example: '(1 (2) (3 (4) (5)))

(defgeneric leafp (object))

(defmethod  leafp ((object cons))
  (null (cdr object)))

(defun random-choose-leaf (tree)
  (if (leafp tree)
      (car   tree)
      (let ((children (cdr tree)))
        (random-choose-leaf (misc:random-elt children)))))

(defun traverse-apply-tree (function tree &optional (args nil))
  (append
   (if (and (consp tree)
            (not (null tree)))
       (reverse
        (append
         (reverse (loop for i in (cdr tree) collect (traverse-apply-tree function i args)))
         (list (apply function (append (list (car tree)) args)))))
       nil)))

(defun traverse-napply-tree (function tree &optional (args nil))
  (when (and (consp tree)
             (not (null tree)))
    (loop for i in (cdr tree) collect (traverse-napply-tree function i args))
    (rplaca tree (apply function (append (list (car tree)) args)))))

(defun traverse-find-if-tree (tree item &key (test #'equal) (key #'identity))
  (progn
    (traverse-apply-tree #'(lambda (x) (if (funcall test item (funcall key x))
                                           (return-from traverse-find-if-tree x)
                                           nil))
                         tree)
    nil))

(defun traverse-find-all-if-tree (tree item &key (test #'equal) (key #'identity))
  (let ((res '()))
    (traverse-apply-tree #'(lambda (x) (if (funcall test item (funcall key x))
                                           (push x res)))
                         tree)
    res))

(defun traverse-apply-tree-cdr (function tree &optional (args nil))
  (append
   (if (and (consp tree)
            (not (null tree)))
       (append
        (list (apply function (append (list tree) args)))
        (loop for i in (cdr tree) by #'cdr collect (traverse-apply-tree-cdr function i args)))
       nil)))

(defun traverse-nadd-child (tree node child &key (test #'equal) (key #'identity))
  (traverse-apply-tree-cdr
   #'(lambda (x) (when (funcall test (funcall key (car x)) node)
                   (progn
                     (rplacd x (append (list (list child)) (cdr x)))
                     (rplaca x (car x)))))
   tree)
  tree)

(defun nappend-child (tree child)
  (rplacd tree (concatenate 'list (cdr tree) (list (list child)))))

(defun traverse-ndelete-child (tree node &key (test #'equal) (key #'identity))
  (traverse-apply-tree-cdr
   #'(lambda (x) (loop
                    for i in (cdr x)
                    for ct = 0 then (1+ ct)
                    do
                      (if (funcall test (funcall key (car i)) node)
                          (rplacd x (misc-utils:safe-delete@ (cdr x) ct)))))
   tree)
  tree)

(defmacro %navigate (tree path)
  (if path
      `(nth ,(first path) (%navigate ,tree ,(rest path)))
      tree))

(defmacro navigate (tree path)
  `(%navigate ,tree ,(reverse path)))

(defun init-children ()
  (misc:make-fresh-array 0 nil t t))

(defclass m-tree ()
  ((data
    :initform nil
    :initarg :data
    :accessor data)
   (parent
    :initform nil
    :initarg :parent
    :accessor parent)
   (children
    :initform (init-children)
    :initarg :children
    :accessor children)))

(defmethod marshal:class-persistant-slots ((object m-tree))
  '(data parent children))

(defgeneric pprint-tree (object stream &optional level parent-length other-data))

(defgeneric add-child (object child &optional child-pos))

(defgeneric child-data-pushnew (object child &key key test))

(defgeneric graft-branch (rootstock scion &key key test overwrite-rootstock-data-p))

(defgeneric add-children (object children))

(defgeneric add-children* (object &rest children))

(defgeneric find-child (object to-find &key compare))

(defgeneric find-child-if (object predicate))

(defgeneric rootp (object))

(defgeneric top-down-visit (object function &optional args))

(defgeneric bottom-up-visit (object function &optional args))

(defgeneric remove-all-children (object))

(defgeneric remove-child (object needle &key key test))

(defgeneric remove-child-if (object predicate))

(defgeneric count-leaves (object))

(defgeneric count-nodes (object))

(defgeneric collect-nodes-data (object))

(defgeneric mtree-equal (tree-1 tree-2 &key key-fn compare-fn))

(defgeneric root-node (object))

(defgeneric single-node-tree-p (object))

(defgeneric tree->text-lines (object &key
                                       last-child-char line-char child-char arrow-char
                                       print-data
                                       print-data-fn))

(defgeneric tree->annotated-lines (object &key
                                       last-child-char line-char child-char arrow-char
                                       print-data
                                       print-data-fn))

(defparameter *use-pprint-tree* nil)

(defmethod print-object ((object m-tree) stream)
  (if *use-pprint-tree*
      (pprint-tree object stream)
      (format stream "[data ~a children ~a]" (data object) (children object))))

(defmethod pprint-tree ((object m-tree) stream &optional (level 0) (parent-length 0)
                                                 (other-data nil))
  (declare (ignore other-data))
  (labels ((indent (level &optional (char " ")) (make-list level :initial-element char)))
    (with-accessors ((data data) (children children)) object
      (let ((data-length (+
                          (do ((parent (parent object) (parent parent))
                               (data-length 0))
                              ((not parent) data-length)
                            (incf data-length (length (format nil "~a" (data parent)))))

                          (length (format nil "~a" data)))))
        (format stream "~{~a~}~a" (indent (+ level parent-length))
                data)
        (if (leafp object)
            (format stream "~%")
            (progn
              (pprint-tree (elt children 0) stream 1)
              (map nil #'(lambda (c) (pprint-tree c stream (1+ level) data-length))
                   (subseq children 1))))))))

(defmethod clone ((object m-tree))
  (make-instance 'm-tree :data (data object) :parent (parent object)
                 :children (alexandria:copy-array (children object))))

(defmethod add-child ((object m-tree) (child m-tree)
                      &optional (child-pos (length (children object))))
  (with-accessors ((children children)) object
    (setf (parent child) object)
    (if (and child-pos
             (< child-pos (length children))
             (>= child-pos 0))
        (setf children
              (let ((res (misc:make-fresh-array (1+ (length children))
                                                nil (type-of child) t)))
                (loop for i from 0 below child-pos do
                     (setf (elt res i) (elt children i)))
                (setf (elt res child-pos) child)
                (loop for i from (1+ child-pos) below (length res) do
                     (setf (elt res i) (elt children (1- i))))
                res))
        (setf children
              (let ((res (misc:make-fresh-array (1+ (length children))
                                                nil (type-of child) t)))
                (loop for i from 0 below (1- (length res)) do
                     (setf (elt res i) (elt children i)))
                (setf (elt res (1- (length res))) child)
                res)))
    (values object child)))

(defmethod child-data-pushnew ((object m-tree) (child m-tree)
                               &key (key #'identity) (test #'eq))
  "Push  a child if there is no siblings with the same data under
`test' or `key' functions"
  (let ((old-data  (map 'list (lambda (a) (funcall key (data a)))
                        (children object)))
        (new-datum (funcall key (data child))))
    (when (not (find new-datum old-data :test test))
      (add-child object child))
    object))

(defmacro do-children ((child node) &body body)
  `(loop for ,child across (children ,node) do
        ,@body))

(defmacro do-children-from-end ((child node) &body body)
  `(loop for ,child across (reverse (children ,node)) do
        ,@body))

(defmethod graft-branch ((rootstock m-tree) (scion m-tree)
                         &key
                           (key #'identity) (test #'eq)
                           (overwrite-rootstock-data-p t))
  "Graft a tree with a single branch (scion) to a tree (rootstock).

  They have to share a common prefix of a list one node
  (i.e '(funcall test  (key (data rootstock)) (key  (data scion)))' is
  non-nil)

  If `overwrite-rootstock-data-p' is non-nil  any the node of the scion that is equals
  under `test' to the any of the rootstock overwrite it.

  Assume this function modify rootstock.

  Examples given:

    a             a             a
   / \       +    |    ->      / \
  b   c           c           b   c
       \          |              / \
        d         e             e   d

    a             a             a------+
   / \       +    |    ->      / \     |
  b   c           d           b   c    d
       \          |                \   |
        d         e                 d  e

    a             a             a
   / \       +    |    ->      / \
  b   c           c           b   c
                  |              /
                  e             e

    a             b             a
   / \       +    |    ->      / \
  b   c           c           b   c
       \          |                \
        d         e                 d

"
  (labels ((extract-data (a)
             (funcall key (data a)))
           (test-data (a b)
             (funcall test
                      (extract-data a)
                      (extract-data b)))
           (twins-sibling (children-rootstock children-scion)
             (loop for child-rootstock across children-rootstock do
                  (loop for child-scion across children-scion do
                       (when (test-data child-rootstock child-scion)
                         (return-from twins-sibling
                           (values child-rootstock child-scion)))))
             nil))
    (with-accessors ((parent-rootstock   parent)
                     (children-rootstock children)) rootstock
      (if (test-data rootstock scion)
          (progn
            (when overwrite-rootstock-data-p
              (setf (data rootstock)
                    (data scion)))
            (cond
              ((misc:vector-empty-p children-rootstock)
               (add-children rootstock (children scion)))
              (t
               (multiple-value-bind (twin-rootstock twin-scion)
                   (twins-sibling children-rootstock
                                  (children scion))
                 (if twin-rootstock
                     (graft-branch twin-rootstock twin-scion :test test :key key)
                     (add-children rootstock (children scion)))))))
          (when (not (rootp rootstock))
            (add-child parent-rootstock scion)))
      rootstock)))

(defmethod add-children ((object m-tree) (children list))
  (loop for i in children do
       (add-child object i))
  object)

(defmethod add-children ((object m-tree) (children vector))
  (loop for i across children do
       (add-child object i))
  object)

(defmethod add-children* ((object m-tree) &rest children)
  (add-children object children))

(defmethod find-child ((object m-tree) to-find &key (compare #'equalp))
  (with-accessors ((data data) (children children)) object
    (if (funcall compare data to-find)
        object
        (if (leafp object)
            nil
            (find-if-not #'null
                         (map 'vector #'(lambda (c)
                                          (find-child c to-find :compare compare))
                              children))))))

(defmethod find-child-if ((object m-tree) predicate)
  (let ((res '()))
    (labels ((%find-child-if (object predicate)
               (when (funcall predicate object)
                 (push object res))
               (do-children (child object)
                 (%find-child-if child predicate))))
      (%find-child-if object predicate)
      res)))

(defmethod leafp ((object m-tree))
  (= (length (children object)) 0))

(defmethod rootp ((object m-tree))
  (null (parent object)))

(defmethod top-down-visit ((node m-tree) function &optional (args nil))
  (apply function (concatenate 'list (list node) args))
  (loop for c across (children node) do
       (top-down-visit c function args)))

(defmethod bottom-up-visit ((node m-tree) function &optional (args nil))
  (loop for c across (children node) do
       (bottom-up-visit c function args))
  (apply function (concatenate 'list (list node) args)))

(defmethod remove-all-children ((object m-tree))
  (setf (children object) (init-children)))

(defmethod remove-child ((object m-tree) (needle m-tree) &key
                                                           (key #'identity)
                                                           (test #'eq))
  (with-accessors ((children children)) object
    (if (leafp object)
        nil
        (loop for i fixnum from 0 below (length children) do
             (if (funcall test (funcall key needle) (funcall key (elt children i)))
                 (progn
                   (setf children
                         (concatenate `(vector ,(array-element-type children)
                                               ,(1- (length children)))
                                      (subseq children 0 i)
                                      (subseq children (1+ i))))
                   (return-from remove-child t))
                 (remove-child (elt children i) needle :key key :test test))))))

(defmethod remove-child ((object m-tree) needle &key
                                                  (key #'identity)
                                                  (test #'eq))
  (with-accessors ((children children)) object
    (if (leafp object)
        nil
        (loop for i fixnum from 0 below (length children) do
             (if (funcall test (funcall key needle) (funcall key (elt children i)))
                 (progn
                   (setf children
                         (concatenate `(vector ,(array-element-type children)
                                               ,(1- (length children)))
                                      (subseq children 0 i)
                                      (subseq children (1+ i))))
                   (return-from remove-child t))
                 (remove-child (elt children i) needle :key key :test test))))))

(defmethod remove-child-if ((object m-tree) predicate)
  (top-down-visit object
                  #'(lambda (n)
                      (with-accessors ((children children)) n
                        (setf children (delete-if predicate children))))))

(defmethod count-leaves ((object m-tree))
  (let ((results 0))
    (top-down-visit object #'(lambda (n)
                               (when (leafp n)
                                 (incf results))))
    results))

(defmethod count-nodes ((object m-tree))
  (let ((results 0))
    (top-down-visit object #'(lambda (n)
                               (declare (ignore n))
                               (incf results)))
    results))

(defmethod collect-nodes-data ((object m-tree))
  (let ((results ()))
    (top-down-visit object #'(lambda (n)
                               (push (data n) results)))
    results))

(defmethod mtree-equal ((tree-1 m-tree) (tree-2 m-tree)
                        &key
                          (key-fn      #'identity)
                          (compare-fn  #'eq))
  (labels ((%mtree-equal (tree-a tree-b)
             (with-accessors ((children-a children)) tree-a
               (with-accessors ((children-b children)) tree-b
                 (let ((value-a (funcall key-fn (data tree-a)))
                       (value-b (funcall key-fn (data tree-b))))
                   (if (funcall compare-fn value-a value-b)
                       (if (= (length children-a)
                              (length children-b))
                           (progn
                             (loop
                                for child-a across children-a
                                for child-b across children-b do
                                  (%mtree-equal child-a
                                                child-b))
                             t)
                           (return-from mtree-equal nil))
                       (return-from mtree-equal nil)))))))
    (%mtree-equal tree-1 tree-2)))

(defmethod root-node ((object m-tree))
  (if (rootp object)
      object
      (root-node (parent object))))

(defmethod single-node-tree-p ((object m-tree))
  (and (rootp object)
       (leafp object)))

(defun make-node (data &optional (parent nil))
  (make-instance 'm-tree :data data :parent parent))

(defclass sorted-m-tree (m-tree)
  ((compare-fn
    :initform #'<
    :initarg  :compare-fn
    :accessor compare-fn
    :documentation "The predicate for children comparison. Default #'<")
   (key-fn
    :initform #'identity
    :initarg  :key-fn
    :accessor key-fn
    :documentation  "The  function to  extract  the  values from  slot
    `data' from each children. Default #'identity"))
  (:documentation "A tree that keep its children sorted"))

(misc:definline sort-children (tree)
  (with-accessors ((children   children)
                   (compare-fn compare-fn)
                   (key-fn     key-fn)) tree
    (setf children (num:shellsort children compare-fn
                                  :key (lambda (a) (funcall key-fn (data a)))))))

(defmethod initialize-instance :after ((object sorted-m-tree) &key &allow-other-keys)
  (sort-children object)
  object)

(defmethod add-child :after ((object sorted-m-tree) (child m-tree)
                             &optional (child-pos (length (children object))))
  (declare (ignore child child-pos))
  (sort-children object))

(alexandria:define-constant +tree-arrow-char+
    #+sbcl #\BLACK_RIGHT-POINTING_ISOSCELES_RIGHT_TRIANGLE
    #+ecl  (code-char 128898)
  :test #'char=)

(defmethod tree->text-lines ((object m-tree)
                             &key
                               (last-child-char (string #\╰))
                               (line-char       (string #\│))
                               (child-char      (string #\├))
                               (spacer-child    (string #\─))
                               (arrow-char      (format nil "~c " +tree-arrow-char+))

                               (print-data      nil)
                               (print-data-fn   #'to-s))
  (let ((res         ())
        (indent-step 1))
    (labels ((last-child-p (tree pos)
               (if (rootp tree)
                   t
                   (>= pos (1- (length (children (parent tree)))))))
             (%print (node indent-level child-pos empty-levels)
               (let ((line "")
                     (data (if print-data
                               (funcall print-data-fn (data node))
                               "")))
                 (flet ((cat-line (&rest chunks)
                          (setf line
                                (reduce #'strcat chunks :initial-value line))))
                   (loop for i from 1 below indent-level do
                        (if (find i empty-levels :test #'=)
                            (cat-line " ")
                            (cat-line line-char))
                        (loop repeat indent-step do
                             (cat-line " ")))
                   (cond
                     ((rootp node)
                      (cat-line data))
                     ((last-child-p node child-pos)
                      (push indent-level empty-levels)
                      (cat-line last-child-char spacer-child arrow-char data))
                     (t
                      (cat-line child-char spacer-child arrow-char data))))
                 (values line empty-levels)))
             (visit (tree indent-level child-pos empty-levels)
               (multiple-value-bind (line new-empty-levels)
                   (%print tree indent-level child-pos empty-levels)
                 (push line res)
                 (loop
                    for node across (children tree)
                    for ct-pos from 0
                    do
                      (visit node (1+ indent-level) ct-pos new-empty-levels)))))
      (visit object 0 0 ())
      (reverse res))))

(defmethod tree->annotated-lines ((object m-tree)
                                  &key
                                    (last-child-char
                                     (string #\BOX_DRAWINGS_LIGHT_ARC_UP_AND_RIGHT))
                                    (line-char
                                     (string #\BOX_DRAWINGS_LIGHT_VERTICAL))
                                    (child-char
                                     (string #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT))
                                    (spacer-child  (string #\BOX_DRAWINGS_LIGHT_HORIZONTAL))
                                    (arrow-char    "> ")
                                    (print-data    nil)
                                    (print-data-fn #'to-s))
  (let ((res         ())
        (indent-step 1))
    (labels ((last-child-p (tree pos)
               (if (rootp tree)
                   t
                   (>= pos (1- (length (children (parent tree)))))))
             (%print (node indent-level child-pos empty-levels)
               (let ((line ())
                     (data (if print-data
                               (funcall print-data-fn (data node))
                               "")))
                 (labels ((append-build-element (&rest chunks)
                            (setf line
                                  (reduce #'append
                                          (mapcar (lambda (a) (list a)) chunks)
                                          :initial-value line)))
                          (cat-line (&rest chunks)
                            (if line
                                (let* ((last-element (alexandria:last-elt line))
                                       (new-element  (list (annotated-text-value last-element)))
                                       (to-concat    (strcat* (append new-element
                                                                      chunks))))
                                  (setf (alexandria:last-elt line) (cons :branch to-concat)))
                                (setf line (list (cons :a (strcat* chunks))))))
                          (build-element (trunk-char data node)
                            (append-build-element (cons :branch
                                                        (strcat trunk-char
                                                                spacer-child))
                                                  (cons :arrow arrow-char)
                                                  (if (leafp node)
                                                      (cons :data-leaf data)
                                                      (cons :data data)))))
                   (loop for i from 1 below indent-level do
                        (if (find i empty-levels :test #'=)
                            (cat-line " ")
                            (append-build-element (cons :d line-char)))
                        (loop repeat indent-step do
                             (cat-line "  ")))
                   (cond
                     ((rootp node)
                      (append-build-element (cons :data-root data)))
                     ((last-child-p node child-pos)
                      (push indent-level empty-levels)
                      (build-element last-child-char data node))
                     (t
                      (build-element child-char data node))))
                 (values line empty-levels)))
             (visit (tree indent-level child-pos empty-levels)
               (multiple-value-bind (line new-empty-levels)
                   (%print tree indent-level child-pos empty-levels)
                 (push line res)
                 (loop
                    for node across (children tree)
                    for ct-pos from 0
                    do
                      (visit node (1+ indent-level) ct-pos new-empty-levels)))))
      (visit object 0 0 ())
      (reverse res))))
