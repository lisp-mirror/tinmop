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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :bs-tree)

(defclass node ()
  ((parent
    :initarg :parent
    :initform nil
    :accessor parent)
   (data
    :initarg :data
    :initform nil
    :accessor data)
   (left
    :initarg :left
    :initform nil
    :accessor left)
   (right
    :initarg :right
    :initform nil
    :accessor right)))

(defgeneric node->string (object))

(defgeneric search (object datum &key key key-datum compare equal))

(defgeneric search-opt (object datum &key key key-datum compare equal candidate))

(defgeneric insert (object datum &key key key-datum compare equal &allow-other-keys))

(defgeneric leafp (object))

(defgeneric all-children-leaf-p (object))

(defgeneric map (object function))

(defgeneric map-node (object function))

(defgeneric %walk (object function args))

(defgeneric walk (object function &rest args))

(defgeneric bstp (object &key comp-fn key))

(defgeneric node->dot (object))

(defgeneric reconstruct-parent (object &optional parent))

(defgeneric find-max-node (object))

(defmethod data ((object (eql nil)))
  nil)

(defmethod parent ((object (eql nil)))
  nil)

(defmethod print-object ((object node) stream)
  (format stream "~a" (node->string object)))

(defmethod node->string ((object (eql nil)))
  "nil")

(defmethod node->string ((object node))
  (if (null (data object))
      ""
      (format nil "~a ~% [~a] [~a]"
              (data object)
              (node->string (left object))
              (node->string (right object)))))

(defmethod leafp ((object node))
  (null (data object)))

(defmethod all-children-leaf-p ((object  node))
  (and (leafp (left object))
       (leafp (right object))))

(defmethod search ((object node) datum  &key (key #'identity)
                     (key-datum #'identity) (compare #'<) (equal #'=))
  (if (leafp object)
      nil
      (cond
        ((funcall equal (%key key (data object)) (%key key-datum datum))
         object)
        ((funcall compare (%key key-datum datum) (%key key (data object)))
         (search (left object) datum :key key :key-datum key-datum
                 :compare compare :equal equal))
        (t
         (search (right object) datum :key key :key-datum key-datum
                   :compare compare :equal equal)))))

(defmethod search-opt ((object node) datum  &key (key #'identity)
                     (key-datum #'identity) (compare #'<) (equal #'=)
                   (candidate nil))
  (if (leafp object)
      (if (and candidate (funcall equal (%key key (data candidate)) (%key key-datum datum)))
          candidate
          nil)
      (cond
        ((funcall compare (%key key (data object)) (%key key-datum datum))
          (search-opt (left object) datum :key key :key-datum key-datum
                    :compare compare :equal equal :candidate candidate))
        (t
         (search-opt (right object) datum :key key :key-datum key-datum
                   :compare compare :equal equal :candidate object)))))

(defun make-node (data left right parent)
  (make-instance 'node :left left :right right :data data :parent parent))

(defun make-leaf (parent)
  (make-instance 'node :parent parent :left nil :right nil))

(defun make-root-node (datum)
  (let* ((tree (make-node datum nil nil nil))
         (l-leaf (make-leaf tree))
         (r-leaf (make-leaf tree)))
    (setf (left tree) l-leaf
          (right tree) r-leaf)
    tree))

(defun %key (key-fn a)
  (funcall key-fn a))

(alexandria:define-constant +data+ :data :test #'eq)

(alexandria:define-constant +left+ :left :test #'eq)

(alexandria:define-constant +right+ :right :test #'eq)

(alexandria:define-constant +parent+ :parent :test #'eq)

(defmethod to-sexp ((object node))
  (let ((*print-circle* t))
    (list +data+ (to-sexp (data object))
          +left+ (to-sexp (left object))
          +right+ (to-sexp (right object))
          +parent+ (to-sexp (data (parent object))))))

(defmethod from-sexp ((object node) sexp)
  (declare (ignorable object))
  (labels ((%from-sexp (sexp)
             (if (null sexp)
                 (make-leaf nil)
                 (make-node (getf sexp +data+)
                            (from-sexp object (getf sexp +left+))
                            (from-sexp object (getf sexp +right+)) nil))))
    (let ((new-tree (%from-sexp sexp)))
      (reconstruct-parent new-tree))))

(defmacro %make-new-node (make-node-fn node data left right parent args)
  `(,make-node-fn ,data ,left ,right ,parent
                  ,@(loop for i in args collect
                         `(,i ,node))))

(defmacro with-insert-local-function ((make-left-node-fn
                                       make-right-node-fn
                                       make-leaf-node-fn
                                       make-leaf-fn
                                       left-descend-fn
                                       right-descend-fn)
                                      &body body)
  (let ((insert-fn (alexandria:format-symbol t "%INSERT")))
  `(labels ((,insert-fn (node datum key key-datum compare equal)
              (if (leafp node)
                  (let* ((new-node (,make-leaf-node-fn
                                     datum nil nil (parent node)))
                         (l-leaf (,make-leaf-fn new-node))
                         (r-leaf (,make-leaf-fn new-node)))
                    (setf (data new-node) datum
                          (left new-node) l-leaf
                          (right new-node) r-leaf)
                    new-node)
                  (cond
                    ((funcall equal (%key key (data node)) (%key key-datum datum))
                     node)
                    ((funcall compare (%key key-datum datum) (%key key (data node)))
                     ,(let ((a `(let ((new-node (,make-left-node-fn
                                                      (data node)
                                                      (,insert-fn (left node) datum key
                                                               key-datum compare equal)
                                                      (right node)
                                                      (parent node))))
                                      (setf (parent (right new-node)) new-node
                                            (parent (left new-node)) new-node)
                                      new-node)))
                       (if left-descend-fn
                           `(,left-descend-fn ,a)
                           a)))
                    (t
                     ,(let ((a `(let ((new-node (,make-right-node-fn
                                                    (data node)
                                                    (left node)
                                                    (,insert-fn (right node) datum key
                                                             key-datum compare equal)
                                                    (parent node))))
                                  (setf (parent (right new-node)) new-node
                                        (parent (left new-node)) new-node)
                                  new-node)))
                           (if right-descend-fn
                           `(,right-descend-fn ,a)
                           a)))))))
     ,@body)))

(defmethod insert ((object node) datum &key (key #'identity) (key-datum #'identity)
                   (compare #'<) (equal #'=))
  (with-insert-local-function (make-node make-node make-node make-leaf nil nil)
    (%insert object datum key key-datum compare equal)))

(defmethod map ((object node) function)
  (with-accessors ((data data) (left left) (right right)) object
    (if (leafp object)
        (make-leaf nil)
        (make-node (funcall function data) (map left function)
                   (map right function) nil))))

(defmethod map-node ((object node) function)
  (with-accessors ((color color) (data data) (left left) (right right)) object
    (if (leafp object)
        (funcall function object (make-leaf object))
        (funcall function object (make-node data
                                            (map-node left function)
                                            (map-node right function) nil)))))

(defmethod %walk ((object node) function args)
  (with-accessors ((color color) (data data) (left left) (right right)) object
    (when (not (leafp object))
        (apply function object args)
        (%walk left function args)
        (%walk right function args))))

(defmethod %walk ((object node) function (args (eql nil)))
  (with-accessors ((color color) (data data) (left left) (right right)) object
    (when (not (leafp object))
        (funcall function object)
        (%walk left function args)
        (%walk right function args))))

(defmethod %walk ((object (eql nil)) function args)
  (apply function object args))

(defmethod walk ((object node) function &rest args)
  (with-accessors ((color color) (data data) (left left) (right right)) object
    (when (not (leafp object))
        (apply function object args)
        (%walk left function args)
        (%walk right function args))))

(defmethod walk ((object (eql nil)) function &rest args)
  (apply function object args))

(defun gather-all (node &key (add-root t))
  (let ((res nil))
    (when (not (leafp node))
      (walk (left node) #'(lambda (n) (push (data n) res)))
      (walk (right node) #'(lambda (n) (push (data n) res)))
      (and add-root (push (data node) res)))
    res))

(defmethod bstp ((object node) &key (comp-fn #'<) (key #'identity))
  (labels ((balanced (node)
             (with-accessors ((left left) (right right)) node
               (if (not (leafp node))
                   (let ((left-children  (and left (gather-all left)))
                         (right-children (and right (gather-all right)))
                         (pivot (funcall key (data node))))
                     (cond
                       ((and (null left-children) (null right-children))
                        t);; leaf node, always balanced
                       ((and left-children right-children)
                        (and
                         (every #'(lambda (a) (funcall comp-fn (funcall key a) pivot))
                                left-children)
                         (every #'(lambda (a) (funcall comp-fn pivot (funcall key a)))
                                     right-children)
                         'a))
                       ((null left-children)
                        (and (every #'(lambda (a) (funcall comp-fn pivot (funcall key a)))
                               right-children))
                        'l)
                       ((null right-children)
                        (every #'(lambda (a) (funcall comp-fn (funcall key a) pivot))
                               left-children)
                        'r)))
                   (misc:dbg "leaf root ~a" (data node))))))
    (let ((res nil))
      (walk object #'(lambda (n) (push
                                  (cons (data n) (or (balanced n) '(nil)))
                                  res)))
      (every #'cdr res))))

(defmethod node->dot ((object node))
  (labels ((nodes ()
             (append
              (list
               `(:node ((:id ,(format nil "~a" (data object)))
                              (:label ,(format nil "~ap~a" (data object)
                                               (data (parent object)))))))
              (if (not (leafp (left object)))
                (node->dot (left object))
                (list
                 `(:node ((:id ,(format nil "nil-l~a" (data object)))
                              (:label "nil")))))
              (if (not (leafp (right object)))
                (node->dot (right object))
                (list
                 `(:node ((:id ,(format nil "nil-r~a" (data object)))
                          (:label "nil")))))))
           (edges ()
             (append
              (if (data (left object))
                (list `(:edge
                        ((:from ,(format nil "~a" (data object)))
                         (:to ,(format nil "~a" (data (left object)))))))
                (list `(:edge
                        ((:from ,(format nil "~a" (data object)))
                         (:to ,(format nil "nil-l~a" (data object)))))))
              (if (data (right object))
                (list `(:edge
                        ((:from ,(format nil "~a" (data object)))
                         (:to ,(format nil "~a" (data (right object)))))))
                (list `(:edge
                        ((:from ,(format nil "~a" (data object)))
                         (:to ,(format nil "nil-r~a" (data object))))))))))
    (append (nodes) (edges))))


(defmethod reconstruct-parent ((object node) &optional (parent (parent object)))
  (with-accessors ((data data) (left left) (right right)) object
    (if (leafp object)
        (make-leaf parent)
        (make-node data
                   (reconstruct-parent left object)
                   (reconstruct-parent right object) parent))))

(defmethod find-max-node ((object node))
  (if (leafp (right object))
      object
      (find-max-node (right object))))
