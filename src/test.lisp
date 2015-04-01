 #| NABU - Prototype palaeographic chart builder
    Copyright (C) 2014 Pierre Thierry <pierre@nothos.net>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>. |#

(defpackage :nothos.net/2014.11.nabu-test
  (:use :cl :hu.dwim.stefil :nabu :scheme)
  (:export #:all)
  (:nicknames :nabu-test))

(in-package :nothos.net/2014.11.nabu-test)

(defsuite* all)

(in-suite all)
(defsuite* misc)

(deftest read-all ()
  (is (equal '(1 2 3) (nabu::read-all-from-string "1 2 3")))
  (is (equal '(nil 1 #\a "foo" -1 nil) (nabu::read-all-from-string "nil 1 #\\a \"foo\" -1 nil")))
  (is (not (nabu::read-all-from-string ""))))

(deftest swap ()
  (let ((list '(1 2 3 4 5 6)))
    (is (equal list (nabu::swap-left 0 list)))
    (is (equal list (nabu::swap-left 1 list)))
    (is (equal '(2 1 3 4 5 6) (nabu::swap-left 2 list)))
    (is (equal '(1 2 3 4 6 5) (nabu::swap-left 6 list)))
    (is (equal list (nabu::swap-right 0 list)))
    (is (equal '(2 1 3 4 5 6) (nabu::swap-right 1 list)))
    (is (equal '(1 3 2 4 5 6) (nabu::swap-right 2 list)))
    (is (equal list (nabu::swap-right 6 list)))))

(in-suite all)
(defsuite* date)

(deftest within ()
  (dolist (number '(1 2 3 4))
    (is (nabu::within? number '(1 4))))
  (dolist (number '(0 5))
    (is (not (nabu::within? number '(1 4))))))

(deftest ranges ()
  (dolist (range '((4 5) (3 5) (3 4) (2 3) (1 2) (0 2) (0 1) (1 4) (0 5)))
    (is (nabu::range-overlap? '(1 4) range)))
  (dolist (range '((-1 0) (6 7)))
    (is (not (nabu::range-overlap? '(1 4) range)))))

(deftest date-check ()
  (let ((date-check (nabu::make-dates 1 4 (5 6))))
    (dolist (good '(1 4 (0 3) (0 4) (3 5) nil))
      (is (funcall date-check good)))
    (dolist (bad '(0 3 7 (-1 0) (2 3) (7 8)))
      (is (not (funcall date-check bad))))))

(in-suite all)
(defsuite* glyph)

(deftest pos-sort ()
  (is (nabu::pos<= '(1 2 3) '(1 2 4)))
  (is (nabu::pos<= '(1 2 3) '(1 2 4 1)))
  (is (nabu::pos<= '(1 2 3) '(1 2 3)))
  (is (not (nabu::pos<= '(1 2 3) '(1 2 2))))
  (is (nabu::pos<= '(1 2 2) '(2 1 1)))
  (is (not (nabu::pos<= '(2 1 1) '(1 2 2)))))


(in-suite all)
(defsuite* shell)

(deftest path (&optional shell)
  (unless shell
    (setf shell (make-hash-table :test 'equal)))
  (setf (nabu::shell-object shell "foo") 1)
  (is (= 1 (nabu::shell-object shell "foo")))
  (is (not (nabu::shell-object shell "fubar")))
  (signals nabu::not-shell (nabu::shell-object shell "fubar" "bar"))
  (nabu::shell-mksub! shell "bar")
  (nabu::shell-mksub! shell "bar" "baz")
  (setf (nabu::shell-object shell "bar" "baz" "quux") 2)
  (is (= 2 (nabu::shell-object shell "bar" "baz" "quux")))
  (signals nabu::not-shell (nabu::shell-mksub! shell "foo")))
