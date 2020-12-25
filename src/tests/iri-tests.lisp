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

(in-package :iri-tests)

(defsuite iri-suite (all-suite))

(defun test-iri (iri results)
  (multiple-value-bind (x parsed)
      (iri-parse iri)
    (declare (ignore x))
    (tree-equal (mapcar #'text-utils:to-s parsed) results :test #'string=)))

(defparameter *test-cases*
  '(("file:///tmp/perché.txt" .
     ("file" nil nil nil "/tmp/perché.txt" nil nil))
    ("imap://mail.common-lisp.net/mbox1" .
     ("imap" nil "mail.common-lisp.net" nil "/mbox1" nil nil))
    ("mms://wms.つづく.sys.hinet.net/cts/Drama/09006251100.asf" .
     ("mms" nil "wms.つづく.sys.hinet.net" nil "/cts/Drama/09006251100.asf" nil nil))
    ("nfs://server/path/to/file.txt" .
     ("nfs" nil "server" nil "/path/to/file.txt" nil nil))
    ("svn+ssh://svn.èéçòìùzope.org/repos/main/ZConfig/trunk/" .
     ("svn+ssh" nil "svn.èéçòìùzope.org" nil "/repos/main/ZConfig/trunk/" nil nil))
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
    ("ldap://[2001:db8::7]/c=GB?objectClass?one" .
     ("ldap" nil "[2001:db8::7]" nil "/c=GB" "objectClass?one" nil))
    ("http://[dead:beef::]:111/foo/" .
     ("http" nil "[dead:beef::]" "111" "/foo/" nil nil))
    ("//foo.bar:198/".
     (nil nil "foo.bar" "198" "/" nil nil))
    ("//fo°o.bar:198/baz.gmi?a=b&b=c#a-fragment".
     (nil nil "fo°o.bar" "198" "/baz.gmi" "a=b&b=c" "a-fragment"))
    ("/bar/baz/baz.gmi?a=b&b=c#a-fràgment".
     (nil nil nil nil "/bar/baz/baz.gmi" "a=b&b=c" "a-fràgment"))
    ("http://" .
     ("http" nil nil nil nil nil nil))
    ("http" .
     (nil nil nil nil "http" nil nil))))

(deftest test-parsing (iri-suite)
  (loop for (a . b) in *test-cases* do
    (assert-true (test-iri a b) a)))
