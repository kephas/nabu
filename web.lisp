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


(defun %checked-parameters (params)
  (mapcan (lambda (param)
	    (if (equal "on" (cdr param)) (list (car param))))
	  params))

(defun get-checked-parameters ()
  (%checked-parameters (get-parameters*)))

(defun post-checked-parameters ()
  (%checked-parameters (post-parameters*)))


(define-easy-handler (bootstrap-css :uri "/bootstrap.min.css") ()
  (handle-static-file "/home/pierre/Development/nabu/bootstrap.min.css"))

(define-easy-handler (bootstrap-js :uri "/bootstrap.min.js") ()
  (handle-static-file "/home/pierre/Development/nabu/bootstrap.min.js"))

(defmacro nabu-page (title &body body)
  `(with-html-output-to-string (out)
     (:html
      (:head
       (:title (fmt "NABU - ~a" ,title))
       (:meta :name "viewport" :content "width=device-width")
       (:link :href "/bootstrap.min.css" :rel "stylesheet")
       (:style :type "text/css" "body {margin: 4em 2em;}"))
      (:body
       ((:div :class "navbar navbar-inverse navbar-fixed-top" :role "navigation")
	((:div :class "container")
	 ((:div :class "navbar-header")
	  ({collapse-btn} ".nabu-navbar-collapse")
	  ((:a :class "navbar-brand" :href "#") "NABU"))
	 ((:div :class "collapse navbar-collapse nabu-navbar-collapse")
	  ((:ul :class "nav navbar-nav")
	   (:li (:a :href "/mss" "Manuscripts"))
	   (:li (:a :href "/tables" "Tables"))))))
       ((:div :class "container")
	(:h1 (str ,title))
	,@body
	(:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js")
	(:script :src "/bootstrap.min.js"))))))

(define-easy-handler (home :uri "/") ()
  (redirect (if (zerop (hash-table-count *tables*))
		"/mss" "/tables")))

(defparameter +s-filter+ "s-filter")

(defun filtering? ()
  (get-parameter +s-filter+))

(defun filter-mss (manuscripts)
  (let ((query (if-let (s-filter (get-parameter +s-filter+))
		 (let ((sexpr (read-all-from-string s-filter)))
		   (if (null sexpr)
		       (constantly t)
		       ($sexpr sexpr)))
		 (constantly t))))
    (do-search query manuscripts)))

(define-easy-handler (show-mss :uri "/mss") ()
  (nabu-page "Manuscripts"
    (if-let (current-filter (filtering?))
      (htm (:p "Current filter:"
	       (:code (esc current-filter))
	       ({active} ("success" :size "sm") "/mss" "View all manuscripts")))
      (htm (:form :role "form" :method "GET" :action "/mss"
		  ({row}
		    (:div (:label "Add filter:")))
		  ({row}
		    ({col} 8 10 (:input :class "form-control" :type "text" :name "s-filter"))
		    ({col} 4 2 ({submit} "default" "Filter"))))))
    (:hr)
    (:form :role "form" :method "GET" :action "/mss2tbl"
	   (:div :class "form-group"
		 (dolist (ms (filter-mss *manuscripts*))
		   (htm
		    ({row}
		      ({col} 12 12 ({checkbox} (ms-name ms) (str (ms-name ms)))))))
		 ({row}
		   ({col} 12 6
		     (:div :class "input-group"
			   (:label :class "input-group-addon" "Table name:")
			   (:input :class "form-control" :name "--NAME")))
		   ({col} 4 6
		     ({submit} "primary" "Create table")))))
    (:hr)
    (:h2 "Add manuscript")
    ((:form :role "form" :method "GET" :action "/addms")
     ({row}
       ({col} 12 10
	 (:div :class "input-group"
	       (:label :class "input-group-addon" "URI ")
	       (:input :class "form-control" :type "url" :name "uri")))
       ({col} 12 2
	 ({submit} "primary" "Add"))))))

(define-easy-handler (add-ms :uri "/addms") (uri)
  (let ((new (read-images-manifest uri)))
    (push new *manuscripts*)
    (nabu-page "Manuscript added"
      (:p (str (ms-name new))))))

(defparameter *tables* (make-hash-table :test 'equal))

(define-easy-handler (show-tables :uri "/tables") ()
  (nabu-page "Tables"
    (:form :role "form" :method "GET" :action "/tbls"
	   (maphash-keys (lambda (name)
			   ({checkbox} name
			     ({active} ("default" :size "lg") (format nil "/tbl?name=~a" name) (str name))
			     " " ({active} ("warning" :size "sm") (format nil "/edit-tbl?name=~a" name) "Edit")
			     " " ({active} ("danger" :size "sm") (format nil "/rm-tbl?name=~a&redirect=t" name) "Remove")))
			 *tables*)
	   ({submit} "primary" "Compare tables"))))

(define-easy-handler (mss2tbl :uri "/mss2tbl") ()
  (let ((table (make-table (get-parameter "--NAME")
			   (mapcan (lambda (name)
				     (if-let (ms (find name *manuscripts* :key #'ms-name :test #'equal))
				       (list ms)))
				   (get-checked-parameters)))))
    (setf (gethash (tbl-name table) *tables*) table)
    (nabu-page "New table"
      (let ((url (format nil "/tbl?name=~a" (tbl-name table))))
	(htm (:a :href url (str (tbl-name table))))))))

(defun glyph-url (glyph)
  (bind (((kind datum) (glyph-img glyph)))
    (case kind
      (:uri datum))))

(defun table-404 (name)
  (setf (return-code*) +http-not-found+)
  (nabu-page "Table not found"
    ({alert} ("warning") "Table " (:code (str name)) " not found.")))

(define-easy-handler  (show-table :uri "/tbl") (name)
  (if-let (table (gethash name *tables*))
    (progn
      (nabu-page (tbl-name table)
	({row} ({active} ("warning") (format nil "/edit-tbl?name=~a" name) "Edit") " "
	       ({active} ("danger") (format nil "/rm-tbl?name=~a" name) "Remove"))
	:hr
	({row}
	  (:table :class "table table-hover"
		  (maphash (lambda (char images)
			     (htm (:tr
				   (:td (str char))
				   (:td
				    (dolist (img images)
				      (htm (:img :src img)))))))
			   (tbl-ab table))))))
    (progn
      (table-404 name))))

(define-easy-handler (rm-table :uri "/rm-tbl") (name redirect)
  (if (remhash name *tables*)
      (if redirect
	  (redirect (format nil "/tbl?removed=~a" name))
	  (nabu-page "Table removed"
	    ({alert} ("success") (:code (str name)) "removed.")))
      (table-404 name)))

(define-easy-handler (new-table :uri "/new-tbl") ()
  (let ((ab (make-hash-table :test 'equal)))
    (dolist (param (post-checked-parameters))
      (bind (((:values _ regs) (scan-to-strings "(.):(.*)" param))
	     (#(char path) regs))
	(push path (gethash char ab))))
    (let ((table (make-instance 'table
				:name (post-parameter "--NAME")
				:ab ab)))
      (setf (gethash (tbl-name table) *tables*) table)
      (nabu-page "New table"
	(let ((url (format nil "/tbl?name=~a" (tbl-name table))))
	  (htm (:p (:a :href url (str (tbl-name table))))))))))

(defun web-start (port)
  (start (make-instance 'easy-acceptor :port port)))
