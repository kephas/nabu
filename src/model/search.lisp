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
  (if (listp query)
      (let* ((key (first query)))
	(typecase key
	  (symbol (make-search-operator key (rest query)))
	  (string (make-field-matcher key (second query)))
	  (t (constantly nil))))
      (constantly nil)))

(defmethod make-search-operator (operator operands)
  (declare (ignore operator operands))
  (constantly nil))

(defmethod make-search-operator ((operator (eql 'and)) operands)
  (lambda (object)
    (every (lambda (matcher) (funcall matcher object)) (mapcar #'make-search-matcher operands))))

(defmethod make-search-operator ((operator (eql 'or)) operands)
  (lambda (object)
    (some (lambda (matcher) (funcall matcher object)) (mapcar #'make-search-matcher operands))))

(defmethod make-search-operator ((operator (eql 'not)) operands)
  (lambda (object)
    (not (funcall (make-search-matcher (first operands)) object))))


#| Human-typable sexp search expression engine |#

(defparameter +noop-query+ (list "" (constantly nil)))

(defun sexp->query-ast (sexp &optional (top-level? t))
  (let ((sexp (if (and top-level? (not (symbolp (first sexp))))
		  (cons 'and sexp) sexp)))
    (let ((operator (first sexp))
	  (operands (rest sexp)))
      (typecase operator
	(symbol (cons operator
		      (mapcar (lambda (sxp)
				(sexp->query-ast sxp nil))
			      operands)))
	(string
	 (let ((value (first operands)))
	   (cond
	     ((stringp value)
	      (list operator (make-string= value)))
	     ((or (integerp value)
		  (and (listp value)
		       (integerp (first value))
		       (integerp (second value))))
	      (list operator (%make-date-checker (list value))))
	     (t +noop-query+))))
	(t +noop-query+)))))

#| Actual search |#

(deftag $sexpr sexpr)
(deftag $query-ast ast)

(defun do-search (query objects &key key)
  (let ((predicate (match query
		     ((tag $sexpr sexpr)
		      (make-search-matcher (sexp->query-ast sexpr)))
		     ((tag $query-ast ast)
		      (make-search-matcher ast))
		     ((type function) query))))
    (remove-if (complement predicate) objects :key key)))


#| Searches not for search engine |#

(defun find-existing-unit (uid uri)
  (find uri (shell-list *root-shell* "users" uid "units")
	:key (lambda (item) (unit-uri (second item))) :test #'string=))

(defun find-unit-charts (uid unit)
  (mapcan (lambda (entry)
	    (when (find unit (cmb-units (second entry)))
	      (list entry)))
	  (shell-list *root-shell* "users" uid "combineds")))
