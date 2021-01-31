;; tinmop: an humble gemini and pleroma client
;; Copyright (C) 2021  cage

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

(in-package :idn-tests)

(defsuite idn-suite (all-suite))

(defun test-encode (unicode expected)
  (string= (idn::host-unicode->ascii unicode)
           expected))

(deftest test-unicode->ascii (idn-suite)
  (assert-true (test-encode "إختبار"  "xn--kgbechtv"))
  (assert-true (test-encode "آزمایشی" "xn--hgbk6aj7f53bba"))
  (assert-true (test-encode "测试"  "xn--0zwm56d"))
  (assert-true (test-encode "測試" "xn--g6w251d"))
  (assert-true (test-encode "испытание" "xn--80akhbyknj4f"))
  (assert-true (test-encode "परीक्षा" "xn--11b5bs3a9aj6g"))
  (assert-true (test-encode "δοκιμή" "xn--jxalpdlp"))
  (assert-true (test-encode "테스트" "xn--9t4b11yi5a" ))
  (assert-true (test-encode "טעסט" "xn--deba0ad"))
  (assert-true (test-encode "テスト" "xn--zckzah"))
  (assert-true (test-encode "பரிட்சை" "xn--hlcj6aya9esc7a")))
