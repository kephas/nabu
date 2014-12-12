 #| NABU - Prototype palaeographic chart builder
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


(defmethod print-object ((object unit) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a~:[~;~1:* (~{~a~a~})~]" (unit-name object) (commatize (hash-keys (nabu-metadata object))))))

(defmethod print-object ((object glyph) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~a" (glyph-char object) (glyph-pos object))))

(defmethod print-object ((object combined) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (cmb-name object) stream)))


#| Text UI |#

(defgeneric tui-print (object))

(defmethod tui-print ((object unit))
  (format *query-io* "~a[~a] || " (unit-name object) (length (unit-glyphs object)))
  (maphash (lambda (key value) (format *query-io* "~a=~a | " key value)) (nabu-metadata object))
  (terpri *query-io*))

(defun tui-list (objects)
  (dolist (object objects)
    (tui-print object)))

(defun tui-search (query objects)
  (tui-list (do-search query objects)))
