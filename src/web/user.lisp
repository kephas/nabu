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



(defvar *default-nav-links* '(("/units" "Units")
			      ("/charts" "Charts")
			      ("/shell" "Shell")))

(defun user-links (uid)
  (mapcar (lambda (link)
	    (list (format nil "/usr/~a#~a" uid (first link)) (second link)))
	  *default-nav-links*))

(defroute "/user/:uid" (&key uid)
  (let ((*nav-links* (user-links uid)))
    (nabu-page ("{{title}}")
      :ng-view)))