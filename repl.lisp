 #| NABU - Prototype palaeographic table builder
    Copyright (C) 2013 Pierre Thierry <pierre@nothos.net>

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


(defun commatize (list &optional (separator ",") (end ""))
  "Prepare LIST to be pretty-printed through ~{~a~a~}"
  (let@ rec ((list list)
	     (result nil))
    (if list
	(rec (rest list)
	     (cons separator (cons (first list) result)))
	(reverse (cons end (rest result))))))

(defmethod print-object ((object manuscript) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a~:[~;~1:* (~{~a~a~})~]" (ms-name object) (commatize (hash-keys (nabu-metadata object))))))

(defmethod print-object ((object glyph) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~a" (glyph-char object) (glyph-pos object))))

(defmethod print-object ((object table) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (tbl-name object) stream)))


#| Text UI |#

(defgeneric tui-print (object))

(defmethod tui-print ((object manuscript))
  (format *query-io* "~a[~a] || " (ms-name object) (length (ms-glyphs object)))
  (maphash (lambda (key value) (format *query-io* "~a=~a | " key value)) (nabu-metadata object))
  (terpri *query-io*))

(defun tui-list (objects)
  (dolist (object objects)
    (tui-print object)))

(defun tui-search (query objects)
  (tui-list (remove-if (complement (make-search-matcher query)) objects)))
