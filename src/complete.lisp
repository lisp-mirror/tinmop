;;;; Copyright (c) 2003, 2004 Nikodemus Siivola, Julian Squires
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :complete)

(defparameter *complete-function* nil
  "A function that get an hint and return two values:
- a list of entries that match that hint
- the common prefix of such candidates.
See: complete:directory-complete")

(defun shortest-candidate (candidates)
  "candidates is a sorted list (by length) -> first of the list,
 otherwise return candidates"
  (and candidates
       (if (listp candidates)
           (first candidates)
           candidates)))

(defun reduce-to-common-prefix (items)
  (reduce #'text-utils:common-prefix items))

(defun pathname-directory-pathname (pathname)
  "convenience function to make a pathname object to a directory"
  (make-pathname :name nil :type nil
                 :defaults pathname))

(defun underlying-directory-p (pathname)
  "Find the actual directory of pathname (i.e. resolve file link"
  (case (file-kind pathname)
    (:directory t)
    (:symbolic-link
     (file-kind (merge-pathnames (read-link pathname) pathname)))))

;;; We can't easily do zsh-style tab-completion of ~us into ~user, but
;;; at least we can expand ~ and ~user.  The other bug here at the
;;; moment is that ~nonexistant will complete to the same as ~.
(defun tilde-expand-string (string)
  "Returns the supplied string, with a prefix of ~ or ~user expanded
to the appropriate home directory."
  (if (and (> (length string) 0)
           (eql (schar string 0) #\~))
      (flet ((chop (s)
               (subseq s 0 (1- (length s)))))
        (let* ((slash-index (loop for i below (length string)
                               when (eql (schar string i) #\/)
                               return i))
               (suffix (and slash-index (subseq string slash-index)))
               (uname (subseq string 1 slash-index))
               (homedir (or (cdr (assoc :home (user-info uname)))
                            (chop (namestring
                                   (or (probe-file (user-homedir-pathname))
                                       (return-from tilde-expand-string
                                         string)))))))
          (concatenate 'string homedir (or suffix ""))))
      string))

(defun directory-complete (string)
  "Return  two  values completion  of  'string'  (non  nil if  can  be
completed) and the common prefix of the completion string."
  (when (text-utils:string-not-empty-p string)
    (let* ((string  (tilde-expand-string string))
           (dir     (pathname-directory-pathname string))
           (namefun (if (relative-pathname-p string)
                        #'namestring
                        (lambda (x) (namestring (merge-pathnames x))))))
      (unless (and (underlying-directory-p dir)
                   (not (wild-pathname-p dir)))
        (return-from directory-complete (values nil 0)))
      (with-directory-iterator (next dir)
        (when-let* ((all        (loop
                                   for entry = (next)
                                   while entry collect
                                     (funcall namefun entry)))
                    (candidates (sort (remove-if-not (lambda (a)
                                                       (text-utils:string-starts-with-p string a))
                                                     all)
                                      (lambda (a b) (< (length a)
                                                       (length b))))))
          (values candidates
                  (reduce-to-common-prefix candidates)))))))

(defun starts-with-clsr (hint)
  (lambda (a)
    (text-utils:string-starts-with-p hint a)))

(defun contains-clsr (hint)
  (handler-case
      (let ((scanner (cl-ppcre:create-scanner hint)))
        (lambda (a)
          (cl-ppcre:scan scanner a)))
    (error ()
      (let ((scanner (cl-ppcre:create-scanner `(:sequence ,hint))))
        (lambda (a)
          (cl-ppcre:scan scanner a))))))

(defun remove-if-hidden (candidates)
  (remove-if #'db:hidden-recipient-p candidates))

(defun folder-complete (hint)
  "Virtual messages folder in db not filesystem directory"
  (when-let ((matching-folders (remove-if-hidden (remove-if-not (starts-with-clsr hint)
                                                                (db:all-folders)))))
    (values matching-folders
            (reduce-to-common-prefix matching-folders))))

(defun timeline-complete-fn (folder)
  "Complete a messages timeline prefix"
  (lambda (hint)
    (let* ((all-timelines      (if folder
                                   (db:all-timelines-in-folder folder
                                                               :include-default-timelines t)
                                   (db:default-timelines)))
           (matching-timelines (remove-if-hidden (remove-if-not (starts-with-clsr hint)
                                                                all-timelines))))
      (values matching-timelines
              (reduce-to-common-prefix matching-timelines)))))

(defmacro with-simple-complete (function-name all-choices-list-fn)
  "Generate a complete function using  function-name to build the name
the function  and `all-choices-list-fn' as  a function that  returns a
list af all possible candidtae for completion."
  (with-gensyms (matched)
    `(defun ,(misc:format-fn-symbol t "~a" function-name) (hint)
       (when-let ((,matched (remove-if-not (starts-with-clsr hint)
                                           (funcall (function ,all-choices-list-fn)))))
         (values ,matched
                 (reduce-to-common-prefix ,matched))))))

(with-simple-complete ignored-username-complete db:all-ignored-usernames)

(let ((memoized (make-hash-table :test #'equalp)))

  (defun set-username-cache-value (hint)
    (setf (gethash hint memoized)
          (remove-if-not (starts-with-clsr hint)
                         (db:all-usernames))))

  (defun initialize-complete-username-cache ()
    (set-username-cache-value "")
    (loop for i in (coerce "abcdefghijklmnopqrstuvwuxyz" 'list) do
      (set-username-cache-value (string i))))

  (defun username-complete (hint)
    (multiple-value-bind (matched found)
        (gethash hint memoized)
      (if (null found)
          (progn
            (set-username-cache-value hint)
            (username-complete hint))
          (values matched (reduce-to-common-prefix matched))))))

(with-simple-complete visibility-complete (lambda () swconf:*allowed-status-visibility*))

(with-simple-complete unfollowed-user-complete
  (lambda () (db:all-unfollowed-usernames :remove-ignored t)))

(with-simple-complete followed-user-complete db:all-followed-usernames)

(with-simple-complete tags-complete (lambda ()
                                      (mapcar #'db:tag->folder-name
                                              (db:all-subscribed-tags-name))))

(with-simple-complete conversation-folder db:all-conversation-folders)

(defun uri-matcher (scanner bag &optional (accum-strings '()) (accum-indices '()))
  (if (null bag)
      (values accum-strings accum-indices)
      (let ((tested (first bag)))
        (multiple-value-bind (start end)
            (cl-ppcre:scan scanner tested)
          (if start
              (uri-matcher scanner
                           (rest bag)
                           (push tested accum-strings)
                           (push (loop for i from start below end collect i)
                                 accum-indices))
              (uri-matcher scanner (rest bag) accum-strings accum-indices))))))

(defun make-complete-gemini-iri-fn (prompt)
  (lambda (hint)
    (when-let ((bag (remove-duplicates (funcall #'db:history-prompt->values
                                                prompt)
                                       :test #'string=)))
      (multiple-value-bind (matched-strings indices)
          (uri-matcher (cl-ppcre:create-scanner hint) bag)
        (when matched-strings
          (values matched-strings
                  (reduce-to-common-prefix matched-strings)
                  indices))))))

(defun complete-chat-message (hint)
  (append (username-complete hint)
          (directory-complete hint)))

(defun complete-always-empty (hint)
  (declare (ignore hint))
  nil)
