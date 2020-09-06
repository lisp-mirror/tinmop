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

(in-package :rb-tree)

(alexandria:define-constant +rb-red+         :red         :test #'eq)

(alexandria:define-constant +rb-black+       :black       :test #'eq)

(alexandria:define-constant +rb-neg-black+   :neg-black   :test #'eq)

(alexandria:define-constant +rb-black-black+ :black-black :test #'eq)

(alexandria:define-constant +rb-color+       :color       :test #'eq)

(defun incf-black (color)
  (cond
    ((eq color +rb-black+)
     +rb-black-black+)
    ((eq color +rb-red+)
     +rb-black+)
    ((eq color +rb-neg-black+)
     +rb-red+)))

(defun decf-black (color)
  (cond
    ((eq color +rb-black-black+)
     +rb-black+)
    ((eq color +rb-black+)
     +rb-red+)
    ((eq color +rb-red+)
     +rb-neg-black+)))

(defclass rb-node (node)
  ((color
    :initarg :color
    :initform +rb-black+
    :accessor color)))

(defgeneric balance (object))

(defgeneric bubble (object))

(defgeneric balancedp (object))

(defgeneric left-balance (object))

(defgeneric right-balance (object))

(defgeneric remove-max-node (object key key-datum compare equal))

(defgeneric %remove-node (object needle key key-datum compare equal))

(defgeneric remove-node (object needle &key key key-datum compare equal))

(defmethod node->string ((object rb-node))
  (if (null (data object))
      (format nil "leaf color ~a" (color object))
      (format nil "~a (~a)~% [~a] [~a]"
              (data object)
              (color object)
              (node->string (left object))
              (node->string (right object)))))

(defun make-rb-node (color data left right parent)
  (make-instance 'rb-node :color color :left left :right right :data data :parent parent))

(defun make-rb-leaf (color parent)
  (make-instance 'rb-node :color color :parent parent :left nil :right nil))

(defun make-root-rb-node (datum color)
  (let* ((tree (make-rb-node color datum nil nil nil))
         (l-leaf (make-rb-leaf +rb-black+ tree))
         (r-leaf (make-rb-leaf +rb-black+ tree)))
    (setf (left tree) l-leaf
          (right tree) r-leaf)
    tree))

(defmethod insert ((object rb-node) datum &key
                                            (key #'identity) (key-datum #'identity)
                                            (compare #'<) (equal #'=))
  (macrolet ((make-leaf-node (datum left right parent)
               `(make-rb-node +rb-red+ ,datum ,left ,right ,parent))
             (make-leaf (new-node)
               `(make-rb-leaf +rb-black+ ,new-node))
             (make-node (data left right parent)
               `(make-rb-node (color node) ,data ,left ,right ,parent)))
    (with-insert-local-function (make-node make-node make-leaf-node make-leaf
                                           left-balance right-balance)
      (let ((balanced (%insert object datum key key-datum compare equal)))
        (setf (color balanced) +rb-black+)
        balanced))))

(defmacro with-match-tree ((color left data right) tree &body body)
  `(and (or (not ,color)
            (eq ,color (color ,tree)))
        (let ((,data (data ,tree)))
          (declare (ignorable ,data))
          ,(if (consp left)
               `(with-match-tree ,left (left ,tree)
                  ,(if (consp right)
                       `(with-match-tree ,right (right ,tree)
                          ,@body)
                       `(let ((,right (right ,tree)))
                          (declare (ignorable ,right))
                          ,@body)))
               `(let ((,left (left ,tree)))
                  (declare (ignorable ,left))
                  ,(if (consp right)
                       `(with-match-tree ,right (right ,tree)
                          ,@body)
                       `(let ((,right (right ,tree)))
                          (declare (ignorable ,right))
                          ,@body)))))))

(defmethod balance ((object rb-node))
  (macrolet ((setf-parent (new-node)
               `(setf (parent (left ,new-node)) ,new-node
                      (parent (right ,new-node)) ,new-node
                      (parent (left (left ,new-node))) (left ,new-node)
                      (parent (right (left ,new-node))) (left ,new-node)
                      (parent (right (right ,new-node))) (right ,new-node)
                      (parent (left (right ,new-node))) (right ,new-node))))
    (with-match-tree (+rb-black+ (+rb-red+ (+rb-red+ a x b) y c) z d) object
      (return-from balance
        (let ((new-node (make-rb-node +rb-red+ y
                                   (make-rb-node +rb-black+ x a b nil)
                                   (make-rb-node +rb-black+ z c d nil)
                                   (parent object))))
          (setf-parent new-node)
          new-node)))

    (with-match-tree (+rb-black+ (+rb-red+ a x (+rb-red+ b y c)) z d) object
      (return-from balance
        (let ((new-node (make-rb-node +rb-red+ y
                                   (make-rb-node +rb-black+ x a b nil)
                                   (make-rb-node +rb-black+ z c d nil)
                                   (parent object))))
          (setf-parent new-node)
          new-node)))

    (with-match-tree (+rb-black+ a x (+rb-red+ (+rb-red+ b y c) z d)) object
      (return-from balance
        (let ((new-node (make-rb-node +rb-red+ y
                                   (make-rb-node +rb-black+ x a b nil)
                                   (make-rb-node +rb-black+ z c d nil)
                                   (parent object))))
          (setf-parent new-node)
          new-node)))

    (with-match-tree (+rb-black+ a x (+rb-red+ b y (+rb-red+ c z d))) object
      (return-from balance
        (let ((new-node (make-rb-node +rb-red+ y
                                   (make-rb-node +rb-black+ x a b nil)
                                   (make-rb-node +rb-black+ z c d nil)
                                   (parent object))))
          (setf-parent new-node)
          new-node)))
    ;; for deletion
    (with-match-tree (+rb-black-black+ (+rb-red+ a x (+rb-red+ b y c)) z d)  object
      (return-from balance
        (let ((new-node (make-rb-node +rb-black+
                                      y
                                      (make-rb-node +rb-black+ x a b nil)
                                      (make-rb-node +rb-black+ z c d nil)
                                      (parent object))))
          (setf-parent new-node)
          new-node)))

    (with-match-tree (+rb-black-black+ (+rb-red+ (+rb-red+ a x b) y c) z d) object
      (return-from balance
        (let ((new-node (make-rb-node +rb-black+
                                      y
                                      (make-rb-node +rb-black+ x a b nil)
                                      (make-rb-node +rb-black+ z c d nil)
                                      (parent object))))
          (setf-parent new-node)
          new-node)))

    (with-match-tree (+rb-black-black+ a x (+rb-red+ (+rb-red+ b y c) z d)) object
      (return-from balance
        (let ((new-node (make-rb-node +rb-black+
                                      y
                                      (make-rb-node +rb-black+ x a b nil)
                                      (make-rb-node +rb-black+ z c d nil)
                                      (parent object))))
          (setf-parent new-node)
          new-node)))

    (with-match-tree (+rb-black-black+ a x (+rb-red+ b y (+rb-red+ c z d))) object
      (return-from balance
        (let ((new-node (make-rb-node +rb-black+
                                      y
                                      (make-rb-node +rb-black+ x a b nil)
                                      (make-rb-node +rb-black+ z c d nil)
                                      (parent object))))
          (setf-parent new-node)
          new-node)))
    (with-match-tree (+rb-black-black+ (+rb-neg-black+ (+rb-black+ a w b)
                                                       x
                                                       (+rb-black+ c y d))
                                       z
                                       e) object
      (return-from balance
        (let ((new-node (make-rb-node +rb-black+
                                      y
                                      (balance (make-rb-node +rb-black+
                                                             x
                                                             (make-rb-node +rb-red+ w a b nil)
                                                             c
                                                             nil))
                                      (make-rb-node +rb-black+ z d e nil)
                                      (parent object))))
          (setf-parent new-node)
          (balance new-node))))

    (with-match-tree (+rb-black-black+ a
                                       x
                                       (+rb-neg-black+ (+rb-black+ b y c)
                                                       z
                                                       (+rb-black+ j n i))) object

      (return-from balance
        (let ((new-node (make-rb-node +rb-black+
                                      y
                                      (make-rb-node +rb-black+
                                                    x
                                                    a
                                                    b
                                                    nil)
                                      (balance (make-rb-node +rb-black+
                                                             z
                                                             c
                                                             (make-rb-node +rb-red+ n j i nil)
                                                             nil))
                                      (parent object))))
          (setf-parent new-node)
          (balance new-node))))

    object))

(defmethod bubble ((object rb-node))
  (if (or (eq (color (left object))  +rb-black-black+)
          (eq (color (right object)) +rb-black-black+))
      ;; TODO reconstruct-parent!
      (balance (make-rb-node (incf-black (color object))
                             (data object)
                             (make-rb-node (decf-black (color (left object)))
                                           (data  (left object))
                                           (left  (left object))
                                           (right (left object))
                                           nil)
                             (make-rb-node (decf-black (color (right object)))
                                           (data  (right object))
                                           (left  (right object))
                                           (right (right object))
                                           nil)
                             nil))
      (balance object)))

(defmethod left-balance ((object rb-node))
  (macrolet ((setf-parent (new-node)
               `(setf (parent (left ,new-node)) ,new-node
                      (parent (right ,new-node)) ,new-node
                      (parent (left (left ,new-node))) (left ,new-node)
                      (parent (right (left ,new-node))) (left ,new-node)
                      (parent (right (right ,new-node))) (right ,new-node)
                      (parent (left (right ,new-node))) (right ,new-node))))
    (with-match-tree (+rb-black+ (+rb-red+ (+rb-red+ a x b) y c) z d) object
      (return-from left-balance
        (let ((new-node (make-rb-node +rb-red+ y
                                   (make-rb-node +rb-black+ x a b nil)
                                   (make-rb-node +rb-black+ z c d nil)
                                   (parent object))))
          (setf-parent new-node)
          new-node)))
    (with-match-tree (+rb-black+ (+rb-red+ a x (+rb-red+ b y c)) z d) object
      (return-from left-balance
        (let ((new-node (make-rb-node +rb-red+ y
                                   (make-rb-node +rb-black+ x a b nil)
                                   (make-rb-node +rb-black+ z c d nil)
                                   (parent object))))
          (setf-parent new-node)
          new-node)))
    object))

(defmethod right-balance ((object rb-node))
  (macrolet ((setf-parent (new-node)
               `(setf (parent (left ,new-node)) ,new-node
                      (parent (right ,new-node)) ,new-node
                      (parent (left (left ,new-node))) (left ,new-node)
                      (parent (right (left ,new-node))) (left ,new-node)
                      (parent (right (right ,new-node))) (right ,new-node)
                      (parent (left (right ,new-node))) (right ,new-node))))
    (with-match-tree (+rb-black+ a x (+rb-red+ (+rb-red+ b y c) z d)) object
      (return-from right-balance
        (let ((new-node (make-rb-node +rb-red+ y
                                   (make-rb-node +rb-black+ x a b nil)
                                   (make-rb-node +rb-black+ z c d nil)
                                   (parent object))))
          (setf-parent new-node)
          new-node)))
    (with-match-tree (+rb-black+ a x (+rb-red+ b y (+rb-red+ c z d))) object
      (return-from right-balance
        (let ((new-node (make-rb-node +rb-red+ y
                                   (make-rb-node +rb-black+ x a b nil)
                                   (make-rb-node +rb-black+ z c d nil)
                                   (parent object))))
          (setf-parent new-node)
          new-node)))
    object))

(defmethod map ((object rb-node) function)
  (with-accessors ((color color) (data data) (left left) (right right)) object
    (if (leafp object)
        (make-rb-leaf color nil)
        (make-rb-node color (funcall function data)
                      (map left function)
                   (map right function) nil))))

(defmethod map-node ((object rb-node) function)
  (with-accessors ((color color) (data data) (left left) (right right)) object
    (if (leafp object)
        (funcall function object (make-rb-leaf color object))
        (funcall function object (make-rb-node color data
                                            (map-node left function)
                                            (map-node right function) nil)))))


(defmethod reconstruct-parent ((object rb-node) &optional (parent (parent object)))
  (with-accessors ((color color) (data data) (left left) (right right)) object
    (if (leafp object)
        (make-rb-leaf color parent)
        (make-rb-node color data
                   (reconstruct-parent left object)
                   (reconstruct-parent right object) parent))))

(defmethod remove-max-node ((object node) key key-datum compare equal)
  (if (leafp (right object))
      (%remove-node object (data object) key key-datum compare equal)
      (bubble (make-rb-node (color object)
                            (data object)
                            (left object)
                            (remove-max-node (right object)
                                              key
                                              key-datum
                                              compare
                                              equal)
                            nil))))

(defmethod %remove-node ((object node) needle key key-datum compare equal)
  (cond
    ((leafp object)
     object)
    ((funcall equal (%key key (data object)) (%key key-datum needle)) ;; equal, delete
     (cond
       ((and (all-children-leaf-p object)  ; no children, red color
             (eq (color object) +rb-red+))
        (make-rb-leaf +rb-black+ nil))
       ((and (all-children-leaf-p object)  ; no children, black color
             (eq (color object) +rb-black+))
        (make-rb-leaf +rb-black-black+ nil))
       ((and (leafp (left object))         ;; one child on right
             (not (leafp (right object))))
        (make-rb-node +rb-black+
                      (data (right object))
                      (left (right object))
                      (right (right object))
                      nil))
       ((and (leafp (right object))        ; one child on left
             (not (leafp (left object))))
        (make-rb-node +rb-black+
                      (data (left object))
                      (left  (left object))
                      (right (left object))
                      nil))
       (t                                      ; two children
        (let* ((max-node (find-max-node (left object)))
               (max-data (data max-node)))
          (bubble (make-rb-node (color object)
                                max-data
                                (remove-max-node (left object)
                                                 key
                                                 key
                                                 compare
                                                 equal)
                                (right object)
                                nil)))))) ;; end removing block
    ((funcall compare (%key key-datum needle) (%key key (data object)))
     (bubble (make-rb-node (color object)
                           (data object)
                           (%remove-node (left object) needle key key-datum compare equal)
                           (right object)
                           nil)))
    (t ; go right, needle is greater then this node
     (bubble (make-rb-node (color object)
                           (data object)
                           (left object)
                           (%remove-node (right object) needle key key-datum compare equal)
                           nil)))))

(defmethod remove-node ((object rb-node) needle &key
                                                  (key #'identity) (key-datum #'identity)
                                                  (compare #'<) (equal #'=))
  (let ((new (%remove-node object needle key key-datum compare equal)))
    (setf (color new) +rb-black+)
    new))

(defmethod node->dot ((object rb-node))
  (labels ((color->hex (node)
             (cond
               ((eq (color node) +rb-red+)
                "#ff0000")
               ((eq (color node) +rb-black-black+)
                "#ff00ff")
               ((eq (color node) +rb-neg-black+)
                "#a1a1a1")
               (t
                "#ffffff")))
           (nodes ()
             (append
              (list
               `(:node ((:id ,(format nil "~a" (data object)))
                        (:label ,(format nil "~ap~a" (data object)
                                         (data (parent object))))
                        (:style "filled")
                        (:fillcolor ,(color->hex object)))))
              (if (not (leafp (left object)))
                (node->dot (left object))
                (list
                 `(:node ((:id ,(format nil "nil-l~a" (data object)))
                          (:label "nil")
                          (:style "filled")
                          (:fillcolor ,(color->hex (left object)))))))
              (if (not (leafp (right object)))
                  (node->dot (right object))
                  (list
                   `(:node ((:id ,(format nil "nil-r~a" (data object)))
                            (:label "nil")
                            (:style "filled")
                            (:fillcolor ,(color->hex (right object)))))))))
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

(defmethod to-sexp ((object rb-node))
  (let ((*print-circle* t))
    (list +data+ (to-sexp (data object))
          +rb-color+ (color object)
          +left+ (to-sexp (left object))
          +right+ (to-sexp (right object))
          +parent+ (to-sexp (data (parent object))))))

(defmethod from-sexp ((object rb-node) sexp)
  (declare (ignorable object))
  (labels ((%from-sexp (sexp)
             (if (null sexp)
                 (make-rb-leaf +rb-black+ nil)
                 (make-rb-node (getf sexp +rb-color+)
                            (getf sexp +data+)
                            (from-sexp object (getf sexp +left+))
                            (from-sexp object (getf sexp +right+)) nil))))
    (let ((new-tree (%from-sexp sexp)))
      (reconstruct-parent new-tree))))

(defmethod balancedp ((object rb-node))
  (let ((black-paths-count '()))
    (labels ((all-red-child-black-p (node)
               (map-node node #'(lambda (n b)
                                  (declare (ignore b))
                                  (if (or (eq (color n) +rb-black+)
                                          (and (eq (color n) +rb-red+)
                                               (eq (color (left n)) +rb-black+)
                                               (eq (color (right n)) +rb-black+)))
                                      t
                                      (return-from all-red-child-black-p nil)))))
             (all-path-black-same-length (node &optional (ct 0))

                 (when (eq (color node) +rb-black+)
                   (incf ct))
                 (if (leafp node)
                     (progn
                       (push ct black-paths-count)
                       (decf ct))
                     (progn
                       (all-path-black-same-length (left node) ct)
                       (all-path-black-same-length (right node) ct)))))
      (all-path-black-same-length object)
      (and (every #'(lambda (a) (= a (elt black-paths-count 0))) black-paths-count)
           (all-red-child-black-p object)))))
