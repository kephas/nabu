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

(defmethod print-object ((object manuscript) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~a,~a" (ms-name object) (ms-place object) (ms-years object))))

(defmethod print-object ((object glyph) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~a" (glyph-char object) (glyph-pos object))))

(defmethod print-object ((object table) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (tbl-name object) stream)))
