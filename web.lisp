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

(defvar *manuscripts* nil)


(define-easy-handler (bootstrap-css :uri "/bootstrap.min.css") ()
  (handle-static-file "/home/pierre/Development/nabu/bootstrap.min.css"))

(define-easy-handler (bootstrap-js :uri "/bootstrap.min.js") ()
  (handle-static-file "/home/pierre/Development/nabu/bootstrap.min.js"))

(defmacro nabu-page (title &body body)
  `(with-html-output-to-string (out)
     (:html
      (:head
       (:title (fmt "NABU - ~a" ,title))
       (:link :href "/bootstrap.min.css" :rel "stylesheet")
       (:style :type "text/css" "body {margin: 4em 2em;}"))
      (:body
       ((:div :class "navbar navbar-inverse navbar-fixed-top" :role "navigation")
	((:div :class "container")
	 ((:div :class "navbar-header")
	  ((:a :class "navbar-brand" :href "#") "NABU"))
	 ((:div :class "collapse navbar-collapse")
	  ((:ul :class "nav navbar-nav")
	   (:li (:a :href "/mss" "Manuscripts"))
	   (:li (:a :href "/tables" "Tables"))))))
       ((:div :class "container")
	(:h1 (str ,title))
	,@body
	(:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js")
	(:script :src "/bootstrap.min.js"))))))

(define-easy-handler (show-mss :uri "/mss") ()
  (nabu-page "Manuscripts"
    (:form :method "GET" :action "/mss2tbl"
	   (dolist (ms *manuscripts*)
	     (htm
	      (:p (:input :type "checkbox" :name (ms-name ms) (str (ms-name ms))))))
	   (:p (:input :name "--NAME"))
	   (:input :type "submit" :value "Create table"))
    (:hr)
    (:h2 "Add manuscript")
    ((:form :method "GET" :action "/addms")
     (:p "URI " (:input :name "uri"))
     (:input :type "submit" :value "Add"))))

(define-easy-handler (add-ms :uri "/addms") (uri)
  (let ((new (read-images-manifest uri))) body...)
    (push new *manuscripts*)
    (nabu-page "Manuscript added"
      (:p (str (ms-name new))))))

(defparameter *tables* (make-hash-table :test 'equal))

(define-easy-handler (show-tables :uri "/tables") ()
  (nabu-page "Tables"
    (:ul
     (maphash-keys (lambda (name)
		     (let ((url (format nil "/tbl?name=~a" name)))
		       (htm (:li (:a :href url (str name))))))
		   *tables*))))

(define-easy-handler (mss2tbl :uri "/mss2tbl") ()
  (let ((table (make-table (get-parameter "--NAME")
			   (mapcan (lambda (name)
				     (if-let (ms (find name *manuscripts* :key #'ms-name :test #'equal))
				       (list ms)))
				   (mapcan (lambda (param)
					     (if (equal "on" (cdr param)) (list (car param))))
					   (get-parameters*))))))
    (setf (gethash (tbl-name table) *tables*) table)
    (nabu-page "New table"
      (let ((url (format nil "/tbl?name=~a" (tbl-name table))))
	(htm (:a :href url (str (tbl-name table))))))))

(defun glyph-url (glyph)
  (bind (((kind datum) (glyph-img glyph)))
    (case kind
      (:file (format nil "/img?path=~a" datum))
      (:uri datum))))

(define-easy-handler (baaad-img :uri "/img") (path)
  (handle-static-file path))

(define-easy-handler  (show-table :uri "/tbl") (name)
  (if-let (table (gethash name *tables*))
    (progn
      (nabu-page (tbl-name table)
	(let ((url (format nil "/edit-tbl?name=~a" name)))
	  (htm (:p (:a :href url "Edit"))))
	(:table
	 (maphash (lambda (char images)
		    (htm (:tr
			  (:td (str char))
			  (:td
			   (dolist (img images)
			     (htm (:img :src img)))))))
		  (tbl-ab table)))))
    (progn
      (setf (return-code*) +http-not-found+)
      (nabu-page "Table not found"))))

(define-easy-handler (new-table :uri "/new-tbl") ()
  (let ((ab (make-hash-table :test 'equal)))
    (dolist (param (post-parameters*))
      (when (equal (cdr param) "on")
	(bind (((:values _ regs) (scan-to-strings "(.):(.*)" (car param)))
	       (#(char path) regs))
	  (push path (gethash char ab)))))
    (let ((table (make-instance 'table
				:name (post-parameter "--NAME")
				:ab ab)))
      (setf (gethash (tbl-name table) *tables*) table)
      (nabu-page "New table"
	(let ((url (format nil "/tbl?name=~a" (tbl-name table))))
	  (htm (:p (:a :href url (str (tbl-name table))))))))))
