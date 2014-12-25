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

(defroute "/shell" ()
  (nabu-page "Shell"
    ((:ul :class "list-unstyled")
     (let@ rec ((object *bad-default-shell*)
		(path))
       (let ((path-string (format nil "~:[~;~1:*~{/~a~}~]~a" path (if (shell? object) "/" ""))))
	 (if (shell? object)
	     (progn
	       (htm
		(:li (:code (str path-string))))
	       (%map-shell object (lambda (k v)
				    (rec v (append path (list k))))))
	     (let ((object-string (format nil "~s" object)))
	       (htm
		 (:li (:code (str path-string)) " = " (:code (esc object-string)))))))))))
