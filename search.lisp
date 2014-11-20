 #| NABU - Prototype palaeographic table builder
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

(in-package :nothos.net/2014.05.nabu)

(defun make-string= (string1)
  (lambda (string2)
    (string= string1 string2)))

(defparameter *default-search-synonyms* '(("year" . "years")))

(defun make-field-matcher (field matcher)
  (lambda (object)
    (funcall matcher (gethash field (nabu-metadata object)))))

(defgeneric make-search-operator (operator operands))

(defun make-search-matcher (query)
  (let* ((key (first query)))
    (typecase key
      (symbol (make-search-operator key (rest query)))
      (string (make-field-matcher key (second query))))))

(defmethod make-search-operator ((operator (eql 'and)) operands)
  (lambda (object)
    (every (lambda (matcher) (funcall matcher object)) (mapcar #'make-search-matcher operands))))

(defmethod make-search-operator ((operator (eql 'or)) operands)
  (lambda (object)
    (some (lambda (matcher) (funcall matcher object)) (mapcar #'make-search-matcher operands))))

(defmethod make-search-operator ((operator (eql 'not)) operands)
  (lambda (object)
    (not (funcall (make-search-matcher (first operands)) object))))
