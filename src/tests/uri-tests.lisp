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

(in-package :uri-tests)

(defsuite uri-suite (all-suite))

(defun test-uri (uri results)
  (multiple-value-bind (x parsed)
      (uri-parse uri)
    (declare (ignore x))
    (tree-equal (mapcar #'text-utils:to-s parsed) results :test #'string=)))

(defparameter *test-cases*
  '(("file:///tmp/junk.txt" .
     ("file" nil nil nil "/tmp/junk.txt" nil nil))
    ("imap://mail.common-lisp.net/mbox1" .
     ("imap" nil "mail.common-lisp.net" nil "/mbox1" nil nil))
    ("mms://wms.sys.hinet.net/cts/Drama/09006251100.asf" .
     ("mms" nil "wms.sys.hinet.net" nil "/cts/Drama/09006251100.asf" nil nil))
    ("nfs://server/path/to/file.txt" .
     ("nfs" nil "server" nil "/path/to/file.txt" nil nil))
    ("svn+ssh://svn.zope.org/repos/main/ZConfig/trunk/" .
     ("svn+ssh" nil "svn.zope.org" nil "/repos/main/ZConfig/trunk/" nil nil))
    ("git+ssh://git@github.com/user/project.git" .
     ("git+ssh" "git" "github.com" nil "/user/project.git" nil nil))
    ("http://common-lisp.net" .
     ("http" nil "common-lisp.net" nil nil nil nil))
    ("http://common-lisp.net#abc" .
     ("http" nil "common-lisp.net" nil nil nil "abc"))
    ("http://common-lisp.net?q=abc" .
     ("http" nil "common-lisp.net" nil nil "q=abc" nil))
    ("http://common-lisp.net/#abc" .
     ("http" nil "common-lisp.net" nil "/" nil "abc"))
    ("http://a/b/c/d;p?q#f" .
     ("http" nil "a" nil "/b/c/d;p" "q" "f"))
    ("http" .
     (nil nil nil nil "http" nil nil))
    ("http://" .
     ("http" nil nil nil nil nil nil))
    ;; ("http:" .
    ;;  ("http" nil nil nil nil nil))
    ("ldap://[2001:db8::7]/c=GB?objectClass?one" .
     ("ldap" nil "[2001:db8::7]" nil "/c=GB" "objectClass?one" nil))
    ("http://[dead:beef::]:111/foo/" .
     ("http" nil "[dead:beef::]" "111" "/foo/" nil nil))
    ("//foo.bar:198/".
     (NIL NIL "foo.bar" "198" "/" NIL NIL))))

(deftest test-parsing (uri-suite)
  (loop for (a . b) in *test-cases* do
    (assert-true (test-uri a b) a)))

(defun normalize (path expected)
  (string= (fs:normalize-path path)
           expected))

(deftest test-normalize-path (uri-suite)
  (assert-true (normalize "/a/x" "/a/x"))
  (assert-true (normalize "/a/../b/x" "/b/x"))
  (assert-true (normalize "/a/../b/x/.." "/b/"))
  (assert-true (normalize "/a/../b/x/." "/b/x/"))
  (assert-true (normalize "/a/b/c/./../../g" "/a/g")))
