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

(defparameter *manuscripts* (list *ms-1908*))

(define-easy-handler (show-mss :uri "/mss") ()
  (with-html-output-to-string (out)
    (htm 
     (:html
      (:head
       (:title "NABU - Manuscripts"))
      (:body
       (:h1 "Manuscripts")
       (:form :method "GET" :action "/mss2tbl"
	      (dolist (ms *manuscripts*)
		(htm
		 (:p (:input :type "checkbox" :name (ms-name ms) (str (ms-name ms))))))
	      (:p (:input :name "--NAME"))
	      (:input :type "submit" :value "Create table")))))))

(define-easy-handler (mss2tbl :uri "/mss2tbl") ()
  (let ((table (make-table (get-parameter "--NAME")
			   (mapcan (lambda (name)
				     (if-let (ms (find name *manuscripts* :key #'ms-name :test #'equal))
				       (list ms)))
				   (mapcan (lambda (param)
					     (if (equal "on" (cdr param)) (list (car param))))
					   (get-parameters*))))))
    (setf (gethash (tbl-name table) *tables*) table)
    (with-html-output-to-string (out)
      (who:htm
       (:html
	(:head
	 (:title "NABU - New table"))
	(:body
	 (:h1 "New table")
	 (let ((url (format nil "/tbl?name=~a" (tbl-name table))))
	   (htm (:a :href url (str (tbl-name table)))))))))))