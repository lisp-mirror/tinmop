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

;; uses code from

;; niccolo': a chemicals inventory
;; Copyright (C) 2016  Universita' degli Studi di Palermo

;; This  program is  free  software: you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published  by  the  Free  Software Foundation,  version  3  of  the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :misc-utils)

;; debug utils

(defparameter *debug* nil)

(defmacro when-debug (&body body)
  `(when (not (null *debug*))
     ,@body))

(defun debug-log (format-string &rest parameters)
  (when (not (log:debug))
    (log4cl:remove-all-appenders log4cl:*root-logger*)
    (log:config :debug :nopackage
                :daily (text-utils:strcat (res:home-datadir)
                                          "tinmop.log")
                :backup nil))
  (let ((message (apply #'format nil format-string parameters)))
    (log:debug message)))

(defun dbg (format-string &rest parameters)
  (apply #'debug-log format-string parameters))

(defun dbg-and-quit (format-string &rest parameters)
  (apply #'dbg format-string parameters)
  (uiop:quit))

(defun dbg-stdout (format-string &rest parameters)
  ((lambda (a b)
     (apply #'format t a b))
   (concatenate 'string format-string "~%")
   parameters))

(defun dump-hash-table (table)
  (let ((res '()))
    (maphash (lambda (k v) (push (format nil "~s -> ~s~%" k v) res)) table)
    res))

(defgeneric dump-hashtable (table))

(defmethod dump-hashtable ((table hash-table))
  (maphash (lambda (k v) (misc:dbg "~s -> ~s" k v)) table))

(defmethod dump-hashtable (table)
  (dbg "~s"table))

(defmacro with-messages-start-end ((start-message end-message
                                                  &key (print-only-if-debug-mode t)) &body body)
  (alexandria:with-gensyms (res)
    (let* ((debug-p     (find :debug-mode *features*))
           (print-msg-p (or (not print-only-if-debug-mode)
                            debug-p)))
      `(progn
         ,(when print-msg-p
            `(dbg ,start-message))
         (let ((,res (progn ,@body)))
           ,(when print-msg-p
              `(dbg ,end-message))
           ,res)))))

;; macro utils

(defmacro format-fn-symbol (package format &rest format-args)
  `(alexandria:format-symbol ,package ,(concatenate 'string "~:@(" format "~)")
                             ,@format-args))

(defun format-keyword (thing)
  (alexandria:make-keyword (format nil "~:@(~a~)" thing)))

(defun check-body-keywords (body ammitted)
  (let ((all-keywords (loop
                         for ct from 1
                         for i in body when (and (oddp ct)
                                                 (keywordp i))
                         collect i)))
    (loop for i in all-keywords do
         (when (not (find i ammitted :test #'eq))
           (error (format nil "keyword must be one of ~a, but ~a was found"
                          ammitted
                          i))))))

;; functions utils

(defun function-name (data)
  "Implementation dependent"
  (assert (functionp data))
  (multiple-value-bind (x y name)
      (function-lambda-expression data)
    (declare (ignore x y))
    (if name
        (string-downcase (symbol-name name))
        data)))

(defmacro fn-delay (a)
  (if (symbolp a)
      `(lambda (&rest p) (apply (function ,a) p))
      `(lambda (&rest p) (apply ,a p))))

(defun unsplice (form)
  (and form
       (list form)))

(defmacro defalias (alias &body (def &optional docstring))
  "Define a value as a top-level function.
     (defalias string-gensym (compose #'gensym #'string))
Like (setf (fdefinition ALIAS) DEF), but with a place to put
documentation and some niceties to placate the compiler.
Name from Emacs Lisp."
  `(progn
     ;; Give the function a temporary definition at compile time so
     ;; the compiler doesn't complain about it's being undefined.
     (eval-when (:compile-toplevel)
       (unless (fboundp ',alias)
         (defun ,alias (&rest args)
           (declare (ignore args)))))
     (eval-when (:load-toplevel :execute)
       (compile ',alias ,def)
       ,@(unsplice
          (when docstring
            `(setf (documentation ',alias 'function) ,docstring))))
     ',alias))

(defun a->function (a)
  (cond
    ((functionp a)
     a)
    ((symbolp a)
     (symbol-function a))))

(defmacro gen-type-p (name)
  (alexandria:with-gensyms (a)
    (let ((fname (if (cl-ppcre:scan "-" (symbol-name name))
                     (alexandria:format-symbol t "~:@(~a-p~)"
                                               (symbol-name name))
                     (alexandria:format-symbol t "~:@(~ap~)"
                                               (symbol-name name)))))
      `(defun ,fname (,a)
         (eql (type-of ,a) ',name)))))

(defmacro define-compiler-macros (name &body args)
  (alexandria:with-gensyms (low-level-function-name)
    (let* ((function-name (alexandria:format-symbol t "~:@(~a~)" name)))
      `(progn
         (defalias ,low-level-function-name #',function-name)
         (define-compiler-macro ,function-name (&whole form ,@args)
           (let ((low-funname ',low-level-function-name))
             (if (every #'constantp (list ,@args))
                 (funcall (symbol-function low-funname) ,@args)
                 (progn
                   form))))))))

(defmacro definline (name arg &rest body)
  (let* ((function-name (alexandria:format-symbol t "~:@(~a~)" name)))
    `(progn
       (declaim (inline ,function-name))
       (defun ,function-name (,@arg) ,@body))))

(defmacro defun-inline-function (name arg &body body)
  (let* ((function-name (alexandria:format-symbol t "~:@(~a~)" name))
         (low-level-function-name (alexandria:format-symbol t "~:@(%~a~)" name)))
    `(progn
       (declaim (inline ,function-name))
       (defun ,function-name (,@arg) (,low-level-function-name ,@arg))
       (defun ,low-level-function-name (,@arg) ,@body))))

(defmacro defmethod-inline-function (name arg &body body)
  (let* ((function-name (alexandria:format-symbol t "~:@(~a~)" name))
         (low-level-function-name (alexandria:format-symbol t "~:@(%~a~)" name)))
    `(progn
       (declaim (inline ,function-name))
       (defgeneric ,low-level-function-name (,@(loop for i in arg collect
                                                    (if (atom i)
                                                        i
                                                        (first i)))))

       (defmethod ,function-name (,@arg) (,low-level-function-name
                                          ,@(loop for i in arg collect
                                                 (if (atom i)
                                                     i
                                                     (first i)))))
       (defmethod ,low-level-function-name (,@arg) ,@body))))

(alexandria:define-constant +cache-invalid-value+ :invalid-cache-value :test #'eq)

(defmacro defcached (name (arg &key (test 'equalp) (clear-cache nil))
                     declaration
                     (&body body))
  (let* ((function-clear-cache-name (format-fn-symbol t "~a-clear-cache" name))
         (function-name             (format-fn-symbol t "~:@(~a~)" name))
         (cache-name                (format-fn-symbol t "~:@(cache~)")))
    `(let ((,cache-name (make-hash-table :test (quote ,test))))
       (defun ,function-clear-cache-name () (clrhash ,cache-name))
       (defun ,function-name (,@arg) ,(if declaration
                                  declaration
                                          `(declare (optimize (speed 0) (safety 3) (debug 3))))

         (and ,clear-cache (setf ,cache-name (make-hash-table :test (quote ,test))))
         ,@(list body)))))

(defmacro defcached-list (name (args &key (equal-fn #'=))
                          &body body)
  "Uses a list as cache storage, good only with few elements!"
  (let* ((function-clear-cache-name  (format-fn-symbol t "~a-clear-cache" name))
         (function-search-cache-name (format-fn-symbol t "~a-search-cache" name))
         (function-ins-cache-name    (format-fn-symbol t "~a-insert-cache" name))
         (function-name              (format-fn-symbol t "~:@(~a~)" name)))
    (multiple-value-bind (forms declaration)
        (alexandria:parse-body body)
      (alexandria:with-gensyms (cache)
        `(let ((,cache '()))
           (defun ,function-clear-cache-name ()
             (setf ,cache '()))
           (defun ,function-search-cache-name (d)
             (find d ,cache :test ,equal-fn))
           (defun ,function-ins-cache-name (d)
             (pushnew d ,cache :test ,equal-fn))
           (defun ,function-name (,@args)
             ,@declaration
             ,@forms))))))

(defun nest-expressions (data &optional (leaf nil))
  (if (null data)
      (list leaf)
      (append (first data) (if (rest data)
                               (list (nest-expressions (rest data) leaf))
                               (nest-expressions (rest data) leaf)))))

(defun replace-e! (expr num)
  (if (null (first expr))
      nil
      (if (atom (first expr))
          (append (list
                   (if (eq (first expr) :e!)
                       num
                       (first expr)))
                  (replace-e! (rest expr) num))
          (append (list (replace-e! (first expr) num))
                  (replace-e! (rest expr) num)))))

(alexandria:define-constant +nil-equiv-bag+ '(:none :false :nil) :test #'equalp)

(defun build-plist (params)
  (let ((keywords (mapcar #'alexandria:make-keyword
                          (loop for i from 0 below (length params) when (oddp (1+ i))
                             collect (elt params i))))
        (vals (mapcar #'(lambda (a)
                          (typecase a
                            (symbol (let ((key (alexandria:make-keyword a)))
                                      (and (not (find key +nil-equiv-bag+ :test #'eq))
                                           key)))
                            (cons   (list a))
                            (otherwise a)))
                      (loop for i from 0 below (length params) when (evenp (1+ i))
                         collect (elt params i)))))
    (mapcar #'(lambda (a b) (cons a b)) keywords vals)))


(defmacro build-assocs-chain (path start)
  (if (null path)
      start
      `(cdr (assoc ,(first path) (build-assocs-chain ,(rest path) ,start)))))

(defmacro gen-trivial-plist-predicate (name class var get-fn)
  (let ((name-fn (alexandria:format-symbol t "~:@(~a-p~)" name)))
    `(progn
       (defgeneric ,name-fn (object))
       (defmethod  ,name-fn ((object ,class))
         (funcall ,get-fn object ,var)))))

(defmacro gen-trivial-plist-predicates (class get-fn &rest vars)
  `(progn
     ,@(loop for v in vars collect
            `(gen-trivial-plist-predicate ,(alexandria:symbolicate (string-trim "+" v))
                                          ,class
                                          ,v
                                          (function ,get-fn)))))

(defmacro gen-trivial-plist-get (function-name-prefix name class var get-fn)
  (let ((name-fn (alexandria:format-symbol t "~:@(~a-~a~)" function-name-prefix name)))
    `(progn
       (defgeneric ,name-fn (object))
       (defmethod  ,name-fn ((object ,class))
         (funcall ,get-fn object ,var)))))

(defmacro gen-trivial-plist-gets (class get-fn function-name-prefix &rest vars)
  `(progn
     ,@(loop for v in vars collect
            `(gen-trivial-plist-get ,function-name-prefix
                                    ,(alexandria:symbolicate (string-trim "+" v))
                                    ,class
                                    ,v
                                    (function ,get-fn)))))

;; plist

(defun recursive-assoc (path start)
  (if (null path)
      start
      (recursive-assoc (rest path) (cdr (assoc (first path) start)))))

(defun recursive-assoc-just-before (path start)
  (if (= (length path) 1)
      start
      (recursive-assoc-just-before (rest path) (cdr (assoc (first path) start)))))

(defun n-setf-path-value (db path new-value)
  (let* ((ptr (recursive-assoc-just-before path db))
         (last-key (alexandria:last-elt path))
         (last-cons (assoc last-key ptr)))
    (if last-cons
        (values (setf (cdr last-cons) new-value) t)
        (values nil nil))))

(defun plist-path-value (db path)
  (let* ((ptr (recursive-assoc-just-before path db))
         (last-key (alexandria:last-elt path))
         (last-cons (assoc last-key ptr)))
    (if last-cons
        (values (cdr last-cons) t)
        (values nil nil))))

;; misc

(defun not-null-p (a)
  (not (null a)))

(definline code->char (code &key (limit-to-ascii nil))
  (code-char (if limit-to-ascii
                 (alexandria:clamp code 0 127)
                 code)))

(definline char->code (code)
  (char-code code))

(defmacro swap (a b)
  `(rotatef ,a ,b))

;;;; binary files utils
;;;; big endian...

(defun 2byte->word (byte1 byte2) ;; little endian
  (let ((res #x00000000))
    (boole boole-ior
           (boole boole-ior byte1 res)
           (ash byte2 8))))

(defun 2word->int (word1 word2)
  (let ((res #x00000000))
    (boole boole-ior
           (ash (boole boole-ior word1 res) 16)
           word2)))

(defun byte->int (bytes)
  (let ((res #x0000000000000000)
        (ct  0))
    (map nil #'(lambda (a)
                 (setf res (boole boole-ior
                                  (ash a ct)
                                  res))
                 (incf ct 8))
         bytes)
    res))

(defmacro gen-intn->bytes (bits)
  (let ((function-name (alexandria:format-symbol t "~:@(int~a->bytes~)" bits)))
    `(defun ,function-name (val &optional (count 0) (res '()))
       (if (>= count ,(/ bits 8))
           (reverse res)
           (,function-name (ash val -8)
                           (1+ count)
                           (push (boole boole-and val #x00ff)
                                 res))))))

(gen-intn->bytes 16)

(gen-intn->bytes 32)

(defun bytes->string (bytes)
  (coerce (mapcar #'code-char bytes) 'string))

(defun read-ieee-float-32 (stream)
  (let ((bytes (make-fresh-list 4)))
    (read-sequence bytes stream)
    (let ((bits (byte->int bytes)))
      (ieee-floats:decode-float32 bits))))

(defmacro define-offset-size (package prefix &rest name-offset-size)
   `(progn
      ,@(loop for i in name-offset-size collect
             `(progn
                (alexandria:define-constant
                    ,(alexandria:format-symbol package "~@:(+~a-~a-offset+~)" prefix (first i))
                    ,(second i) :test #'=)
                ,(when (= (length i) 3)
                       `(alexandria:define-constant
                            ,(alexandria:format-symbol package "~@:(+~a-~a-size+~)" prefix
                                                       (first i))
                            ,(third i) :test #'=))))))

(defmacro define-parse-header-chunk ((name offset size object &optional (slot name)))
  (alexandria:with-gensyms (bytes)
    `(progn
       (defgeneric ,(alexandria:format-symbol t "PARSE-~:@(~a~)" name) (,object stream))
       (defmethod ,(alexandria:format-symbol t "PARSE-~:@(~a~)" name) ((object ,object) stream)
         (file-position stream ,offset)
         (let* ((,bytes (make-fresh-list ,size)))
           (read-sequence ,bytes stream)
           ,(when (not (null slot))
                  `(setf (,slot object) ,bytes))
           (values ,bytes object))))))

(defun read-list (stream size &key (offset nil))
  (when offset
    (file-position stream offset))
  (let* ((bytes (misc-utils:make-fresh-list size)))
    (read-sequence bytes stream)
    bytes))

(defun read-array (stream size &key (offset nil))
  (when offset
    (file-position stream offset))
  (let* ((bytes       (misc-utils:make-array-frame size 0 '(unsigned-byte 8) t))
         (read-so-far (read-sequence bytes stream)))
    (values bytes read-so-far)))

(defun read-all (stream)
  "Read all the octent from stream ad returns them as array"
  (let ((raw (loop
                for c = (read-byte stream nil nil)
                while c
                collect c)))
    (coerce raw '(vector (unsigned-byte 8)))))

(defun read-line-into-array (stream &key (add-newline-stopper t))
  "Read a line as array of unsigned octets or nil if stream is exausted.
if `add-newline-stopper' is  non nil a newline (ASCII  10) is appended
to the array"
  (let ((first-byte (read-byte stream nil nil)))
    (when first-byte
      (let ((raw (loop
                   for c = first-byte then (read-byte stream nil (char-code #\Newline))
                   while (/= c (char-code #\Newline))
                   collect c)))
        (when add-newline-stopper
          (let ((rev (reverse raw)))
            (push (char-code #\Newline) rev)
            (setf raw (reverse rev))))
        (misc:list->array raw '(unsigned-byte 8))))))

;; sequence utils

(defun safe-elt (sequence index)
  (and (>= index 0)
       (< index (length sequence))
       (elt sequence index)))

(defun safe-last-elt (sequence)
  (safe-elt sequence (1- (length sequence))))

(defun safe-subseq (sequence start &optional (end nil))
  (when sequence
    (when (or (null start)
              (< start 0))
      (setf start 0))
    (when (and (numberp end)
               (> end  (length sequence)))
      (setf end (length sequence)))
    (let* ((actual-start (alexandria:clamp start
                                           0
                                           (length sequence)))
           (actual-end   (and end
                              (max actual-start
                                   (min end
                                        (length sequence))))))
      (subseq sequence actual-start actual-end))))

(defgeneric sequence-empty-p (a))

(defmethod sequence-empty-p ((a vector))
  (vector-empty-p a))

(defmethod sequence-empty-p ((a sequence))
  (alexandria:emptyp a))

(defun vector-empty-p (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (vector v))
  (= (length v) 0))

(defun random-num-filled-vector (size max)
  (map-into (misc:make-array-frame size max (type-of max) t)
            #'(lambda () (num:lcg-next-upto max))))

(definline random-elt (seq)
  (elt seq (num:lcg-next-upto (length seq))))

(defun safe-random-elt (seq)
  "note: values nil if (or (null seq) (= (length seq) 0))"
  (and seq
       (> (length seq) 0)
       (elt seq (num:lcg-next-upto (length seq)))))

(defun make-fresh-list (size &optional (el nil))
  (map-into (make-list size)
            (if (functionp el)
                el
                #'(lambda () el))))

(defun seq->list (sequence)
  (if (listp sequence)
      (copy-list sequence)
      (map-into (make-list (length sequence)) #'identity sequence)))

(defmacro *cat (type-return input)
  `(reduce #'(lambda (a b) (concatenate ',type-return a b)) ,input))

(defun lcat (&rest v)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (*cat list v))

(defun vcat (&rest v)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (*cat vector v))

(defun fresh-list-insert@ (a v pos)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (list a))
  (lcat (subseq a 0 pos)
        (list v)
        (subseq a pos)))

(defun fresh-list-subst@ (a v pos)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (list a))
  (lcat (subseq a 0 pos)
        (list v)
        (subseq a (1+ pos))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-array-frame (size &optional (el nil) (type t) (simplep nil))
    "All elements points to the same address/reference!"
    (make-array size
                :fill-pointer (if (not simplep) size nil)
                :adjustable (if (not simplep) t nil)
                :initial-element el
                :element-type type)))

(defun make-fresh-array (size &optional (el nil) (type t) (simplep nil))
  (let ((res (make-array size
                         :fill-pointer (if (not simplep) size nil)
                         :adjustable (if (not simplep) t nil)
                         :initial-element el
                         :element-type type)))
    (map-into res #'(lambda (a) (setf a (cond
                                          ((functionp el)
                                           (funcall el))
                                          ((arrayp el)
                                           (alexandria:copy-array el))
                                          ((listp el)
                                           (copy-list el))
                                          (t
                                           el))))
              res)))

(defun list->array (the-list &optional (element-type t))
  (make-array (length the-list)
              :element-type     element-type
              :fill-pointer     (length the-list)
              :adjustable       t
              :initial-contents (copy-list the-list)))

(defun copy-list-into-array (from to)
  (assert (= (length from) (length to)))
  (loop
     for i in from
     for ct from 0 by 1 do
       (setf (elt to ct) i))
  to)

(defun array-slice (array start &optional (end nil))
  (let* ((new-size         (if end
                               (- end start)
                               (length array)))
         (new-fill-pointer (cond
                             ((array-has-fill-pointer-p array)
                              (if end
                                  new-size
                                  (fill-pointer array)))
                             (t
                              nil)))
         (new-array        (make-array new-size
                                       :element-type    (array-element-type array)
                                       :fill-pointer    new-fill-pointer
                                       :initial-element (alexandria:first-elt array)
                                       :adjustable      (adjustable-array-p array)))
         (end-iteration    (or end
                               (length array))))
    (loop
       for index-from from start below end-iteration
       for index-to   from 0
           do
         (setf (elt new-array index-to)
               (elt array     index-from)))
    new-array))

(defun list->simple-array (the-list start-type type)
  (let ((res (make-array-frame (length the-list) start-type type t)))
    (loop
       for element in the-list
       for i from 0 below (length the-list) do
         (setf (elt res i) element))
    res))

(defun permutation (li)
  (let ((res-partial '())
        (res '()))
    (labels ((perm (start tail)
               (let ((partial-tree '()))
                 (loop for i in start do
                      (loop for j in (set-difference tail i) do
                           (push (append  i (list j)) partial-tree)))
                 (setf res-partial (reverse (copy-tree partial-tree))))))
      (loop for ct in li do
           (do ((start (list (list ct)) res-partial))
               ((null (set-difference li (first start)))
                (progn
                  (setf res (append res res-partial))
                  (setf res-partial '())))
             (perm start li))))
    res))

(defun shuffle (sequence)
  (loop for i from (1- (length sequence)) downto 1 do
       (let ((rnd (num:lcg-next-upto (1+ i))))
         (swap (elt sequence rnd) (elt sequence i))))
  sequence)

(defun split-into-sublist (lst len)
  (if (or (= len 0)
          (< (length lst) len))
      (if (null lst)
          lst
          (list lst))
      (append (list (subseq lst 0 len)) (split-into-sublist (subseq lst len) len))))

(defun group-by (sequence &key (test #'=))
  (let ((distinct '()))
    (loop for i in sequence do
         (pushnew i distinct :test test))
    (loop for i in distinct collect
         (remove-if-not #'(lambda (a) (funcall test a i)) sequence))))

(defgeneric delete@ (sequence position))

(defgeneric safe-delete@ (sequence position)
  (:documentation "Return sequence if position is out of bound"))

(defmacro gen-delete@ ((sequence position) &body body)
  `(if (and (>= ,position 0)
            (< ,position (length ,sequence)))
       ,@body
      (error 'conditions:out-of-bounds :seq sequence :idx position)))

(defmethod delete@ ((sequence list) position)
  (gen-delete@
   (sequence position)
   (append (subseq sequence 0 position)
           (and (/= position (- (length sequence) 1))
                (subseq sequence (1+ position))))))

(defmethod delete@ ((sequence vector) position)
  (gen-delete@
   (sequence position)
    (make-array (1- (length sequence))
                :fill-pointer (1- (length sequence))
                :adjustable t
                :initial-contents (concatenate 'vector (subseq sequence 0 position)
                                               (and (/= position (- (length sequence) 1))
                                                    (subseq sequence (1+ position)))))))

(defmethod safe-delete@ ((sequence sequence) position)
  (restart-case
      (delete@ sequence position)
    (return-nil () nil)
    (return-whole () sequence)
    (new-index (i) (safe-delete@ sequence i))))

(defun safe-all-but-last-elt (sequence)
  (handler-bind ((conditions:out-of-bounds
                  #'(lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'return-nil))))
    (safe-delete@ sequence (1- (length sequence)))))

(defgeneric remove-compact-remap-sequence (sequence predicate))

(defmethod remove-compact-remap-sequence ((sequence list) predicate)
  (let ((nullified (loop
                      for i in sequence
                      for ct from 0 collect
                        (if (funcall predicate ct i)
                            nil
                            i)))
        (mapping nil)
        (results '()))
    (loop
       for i in nullified
       for pos from 0 do
         (when (not (null i))
           (push i results)
           (push (list pos (1- (length results))) mapping)))
    (values (reverse results) mapping)))

(defmethod remove-compact-remap-sequence ((sequence vector) predicate)
  (let ((nullified (loop for i from 0 below (length sequence) collect
                        (if (funcall predicate i (elt sequence i))
                            nil
                            (elt sequence i))))
        (mapping nil)
        (results (make-array-frame 0)))
    (loop for i from 0 below (length nullified) do
         (when (not (null (elt nullified i)))
           (vector-push-extend (elt nullified i) results)
           (push (list i (1- (length results))) mapping)))
    (values results mapping)))

(defun remove-if-null (a)
  (remove-if #'null a))

(defun remove-if-not-null (a)
  (remove-if #'(lambda (i) (not (null i))) a))

(defun copy-multiply (from to length source-step copy-num)
  (loop for ct from 0 below (* source-step length) by source-step
        for ct2 from 0 below (* length source-step copy-num) by (* source-step copy-num) do
       (loop for ct3 from 0 below (* source-step copy-num) by 1 do
            (setf (elt to (+ ct2 ct3))
                  (elt from (+ ct (mod ct3 source-step))))))
  to)

(defun all-but-last-elt (s)
  (if s
      (let ((length (length s)))
        (if (> length 0)
            (subseq s 0 (1- length))
            s))
      s))

(defgeneric intersperse (seq new-elt))

(defmethod intersperse ((seq list) new-elt)
  (loop for (item . rest) on seq
        if (null rest)
          collect item
        else collect item
             and collect new-elt))

(defmethod intersperse ((seq sequence) new-elt)
  (if (< (length seq) 2)
      (copy-seq seq)
      (let* ((len1 (length seq))
             (len2 (1- (* 2 len1)))
             (ret  (typecase seq
                       (string
                        (make-string len2))
                       (vector
                        (make-fresh-array len2))))
             (j 0))
        (loop for i below len2 do
          (if (oddp i)
              (setf (elt ret i) new-elt)
              (progn
                (setf (elt ret i) (elt seq j))
                (incf j))))
        ret)))

;; iterations

(defmacro do-while (declaration return-form &body body)
  "C-like \"do { ...} while (condition)\" statement: body is evaluated
  even if exit condition is t at the very first iteration"
  (alexandria:with-gensyms (first-iteration)
    `(do ,(append (list `(,first-iteration t nil))
                  declaration)
         ,(append (list `(if ,first-iteration
                             nil
                             ,(first return-form)))
                  (rest return-form))
       ,@body)))

(defmacro do-while* (declaration return-form &body body)
  "C-like \"do { ...} while (condition)\" statement: body is evaluated
  even if exit condition is t at the very first iteration"
  (alexandria:with-gensyms (first-iteration)
    `(do* ,(append (list `(,first-iteration t nil))
                   declaration)
          ,(append (list `(if ,first-iteration
                              nil
                              ,(first return-form)))
                   (rest return-form))
       ,@body)))

;; cg vectors

(defmacro gen-vec-comp ((prefix-name comp-name index) &rest declarations)
  (let ((name     (format-fn-symbol t "~a-~a" prefix-name comp-name))
        (set-name (format-fn-symbol t "%set-~a-~a" prefix-name comp-name))
        (arg      (format-fn-symbol t "v")))
    `(progn
       (defun ,set-name (vec value)
         (setf (elt vec ,index) value))
       (defsetf ,name ,set-name)
       (defun ,name (,arg)
         ,@declarations
         (elt ,arg ,index))
       (define-compiler-macros ,name ,arg))))

;; cffi

(definline make-null-pointer ()
  (cffi:null-pointer))

;; plugins, sort of

(defmacro with-load-forms-in-var ((special-var output-var file) &body body)
  `(let* ((,special-var nil))
     (load ,file)
     (let ((,output-var ,special-var))
       ,@body)))

;;;; derived from local-time library

(alexandria:define-constant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0)
  :test #'=)

(defun time-unix->universal (unix-timestamp)
  "Return the UNIVERSAL-TIME corresponding to the TIMESTAMP"
  ;; universal time is seconds from 1900-01-01T00:00:00Z
  ;; unix timestamp is seconds from 1970-01-01T00:00:00Z
  (+ unix-timestamp +unix-epoch+))

(defmacro gen-time-access (name pos)
  `(defun ,(format-fn-symbol t "time-~a-of" name) (time-list)
     (elt time-list ,pos)))

(defmacro gen-all-time-access (&rest name-pos)
  `(progn
     ,@(loop for i in name-pos collect
            `(gen-time-access ,(car i) ,(cdr i)))))

(gen-all-time-access (second     . 0)
                     (minutes    . 1)
                     (hour       . 2)
                     (date       . 3)
                     (month      . 4)
                     (year       . 5)
                     (day        . 6)
                     (daylight-p . 7)
                     (zone       . 8))

(defun year->timestamp (year)
  (local-time:encode-timestamp 0
                               0
                               0
                               0
                               1
                               1
                               (truncate (max 0
                                              (num:safe-parse-number year)))))

(defun current-year ()
  (local-time:timestamp-year (db-utils:local-time-obj-now)))

(defun extract-year-from-timestamp (ts)
  (local-time:timestamp-year ts))

(defun command-terminated-no-error-p (command-error-code)
  (= command-error-code 0))

(defun format-time (local-time-object format-control-list)
  (with-output-to-string (stream)
    (local-time:format-timestring stream local-time-object :format format-control-list)))

;; threads

(defmacro with-lock ((lock) &body body)
  `(bt:with-recursive-lock-held (,lock)
     ,@body))

(defmacro defun-w-lock (name parameters lock &body body)
  (multiple-value-bind (remaining-forms declarations doc-string)
      (alexandria:parse-body body :documentation t)
    `(defun ,name ,parameters
       ,doc-string
       ,declarations
       (with-lock (,lock)
         ,@remaining-forms))))

;; http

(defun get-url-content (url)
  (multiple-value-bind (stream response-code)
      (drakma:http-request url
                           :want-stream         t
                           :verify              :required
                           :external-format-out :utf8)
    (values stream response-code)))

(defun get-url-content-body (url)
  (drakma:http-request url
                       :want-stream         nil
                       :verify              :required
                       :external-format-out :utf8))

;; profiling

(defmacro with-profile-time (&body body)
  `(with-output-to-string (stream)
     (let ((*trace-output* stream))
       (time (progn ,@body)))))

(defmacro with-debug-print-profile-time ((&optional prefix) &body body)
  `(misc:dbg "~a ~a" ,prefix (with-profile-time ,@body)))

;; package building utils

#+quicklisp
(defun ql-system-equals (a b)
  (string= (ql::short-description a)
           (ql::short-description b)))

#+quicklisp
(defun remove-system-duplicates-test (systems)
  (remove-duplicates systems
                     :test #'string=))

#+quicklisp
(alexandria:define-constant +github-quicklisp-source-url-template+
  "https://raw.githubusercontent.com/quicklisp/quicklisp-projects/master/projects/~a/source.txt"
  :test #'string=)

#+quicklisp
(defun get-quicklisp-original-file (system-name)
  (multiple-value-bind (stream response-code)
      (get-url-content (format nil
                               +github-quicklisp-source-url-template+
                               system-name))
    (when (or (< response-code 400)
              (> response-code 499))
      (let* ((line   (babel:octets-to-string (read-line-into-array stream)))
             (fields (text-utils:split-words line)))
        fields))))

#+quicklisp
(defun asdf-depends-on (&optional (system-name config:+program-name+))
  (let ((symbol-system (alexandria:symbolicate (string-upcase system-name))))
    (asdf:system-depends-on (asdf:find-system symbol-system))))

#+quicklisp
(defun all-dependencies (system-name)
  (flet ((get-direct-dependencies (system-name)
           (remove-system-duplicates-test (asdf-depends-on system-name))))
    (let* ((direct  (get-direct-dependencies system-name))
           (results (copy-list direct)))
      (loop for i in direct do
        (let ((dependencies (get-direct-dependencies i)))
          (loop for j in dependencies do
            (pushnew j results :test #'string=)
            (all-dependencies i))))
      (sort results #'string<))))

(defun all-program-dependencies (&optional download)
  #+quicklisp
  (let* ((dependencies          (all-dependencies config:+program-name+))
         (clean-dependencies    (mapcar (lambda (a)
                                          (cond
                                            ((string= a "sqlite")
                                             "cl-sqlite")
                                            ((string= a "marshal")
                                             "cl-marshal")
                                            (t
                                             a)))
                                        dependencies)))
    (flet ((download-package (fields)
             (if (cl-ppcre:scan "git" (first fields))
                 (os-utils:run-external-program "git"
                                                (list "clone" (second fields))
                                                :search t)
                 (let ((data (get-url-content-body (second fields))))
                   (with-open-file (out-stream (fs:path-last-element (second fields))
                                               :direction :output
                                               :element-type '(unsigned-byte 8))
                     (write-sequence data out-stream))))))
      (loop for system-name in (sort clean-dependencies #'string<) do
        (let ((fields (get-quicklisp-original-file system-name)))
          (if fields
              (cond
                ((string= (first fields) "ediware-http")
                 (let ((url (format nil "https://github.com/edicl/~a.git" system-name)))
                   (format t "~a ~a ~a~%" system-name "git" url)
                   (when download
                     (download-package (list "git" url)))))
                ((string= (first fields) "kmr-git")
                 (let ((url (format nil "http://git.kpe.io/~a.git" system-name)))
                   (format t "~a ~a ~a~%" system-name "git" url)
                   (when download
                     (download-package (list (first fields) url)))))
                (t
                 (format t "~a ~a ~a~%" system-name (first fields) (second fields))
                 (when download
                   (download-package fields))))
              (format t "!error: ~a~%" system-name))))))
  #-quicklisp
  (format t
          "Unable to print dependencies because quicklisp was not loaded when compiling ~a.~%"
          config:+program-name+))
