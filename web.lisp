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

(define-easy-handler (sticky-tabs-js :uri "/sticky-tabs.js") ()
  (handle-static-file "/home/pierre/Development/nabu/sticky-tabs.js"))

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
	   (:li (:a :href "/units" "Units"))
	   (:li (:a :href "/charts" "Charts"))))))
       ((:div :class "container")
	(:h1 (str ,title))
	,@body
	(:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js")
	(:script :src "/bootstrap.min.js")
	(:script :src "/sticky-tabs.js"))))))

(define-easy-handler (home :uri "/") ()
  (redirect (if (zerop (hash-table-count *combineds*))
		"/units" "/charts")))

(defparameter +s-filter+ "s-filter")

(defun filtering? ()
  (get-parameter +s-filter+))

(defun filter-units (units)
  (let ((query (if-let (s-filter (get-parameter +s-filter+))
		 (let ((sexpr (read-all-from-string s-filter)))
		   (if (null sexpr)
		       (constantly t)
		       ($sexpr sexpr)))
		 (constantly t))))
    (do-search query units)))

(define-easy-handler (show-units :uri "/units") ()
  (nabu-page "Units"
    (if-let (current-filter (filtering?))
      (htm (:p "Current filter:"
	       (:code (esc current-filter))
	       ({active} ("success" :size "sm") "/units" "View all units")))
      (htm (:form :role "form" :method "GET" :action "/units"
		  ({row}
		    (:div (:label "Add filter:")))
		  ({row}
		    ({col} 8 10 (:input :class "form-control" :type "text" :name "s-filter"))
		    ({col} 4 2 ({submit} "default" "Filter"))))))
    (:hr)
    (:form :role "form" :method "GET" :action "/units2cmb"
	   (:div :class "form-group"
		 (dolist (ms (filter-units (shell-object *bad-default-shell* "units")))
		   (htm
		    ({row}
		      ({col} 12 12 ({checkbox} (unit-name ms) (str (unit-name ms)))))))
		 ({row}
		   ({col} 12 6
		     (:div :class "input-group"
			   (:label :class "input-group-addon" "Chart name:")
			   (:input :class "form-control" :name "--NAME")))
		   ({col} 4 6
		     ({submit} "primary" "Combine units")))))
    (:hr)
    (:h2 "Add unit")
    ((:form :role "form" :method "GET" :action "/addunit")
     ({row}
       ({col} 12 10
	 (:div :class "input-group"
	       (:label :class "input-group-addon" "URI ")
	       (:input :class "form-control" :type "url" :name "uri")))
       ({col} 12 2
	 ({submit} "primary" "Add"))))))

(defparameter *bad-default-shell* (make-instance 'shell))
(shell-ensure *bad-default-shell* "combineds")
(defparameter *combineds* (shell-object *bad-default-shell* "combineds"))

(define-easy-handler (add-unit :uri "/addunit") (uri)
  (let ((new (read-images-manifest uri)))
    (push new (shell-object *bad-default-shell* "units"))
    (setf (shell-object *bad-default-shell* "combineds" (make-oid))
	  (make-combined (unit-name new) (list new)))
    (nabu-page "Unit added"
      (:p (str (unit-name new))))))

(define-easy-handler (show-combineds :uri "/charts") (removed)
  (nabu-page "Charts"
    (when removed
      (htm ({alert} ("warning" t) "Combined chart " (str removed) " removed")))
    (:form :role "form" :method "GET" :action "/compare"
	   (maphash (lambda (oid chart)
		      ({checkbox} oid
			({active} ("default" :size "lg") (format nil "/chart?oid=~a" (urlencode oid)) (str (cmb-name chart)))
			" " ({active} ("warning" :size "sm") (format nil "/edit-chart?oid=~a" (urlencode oid)) "Edit")
			" " ({active} ("danger" :size "sm") (format nil "/rm-chart?oid=~a&redirect=t" (urlencode oid)) "Remove")))
		    *combineds*)
	   (unless (zerop (hash-table-count *combineds*))
	     (htm ({submit} "primary" "Compare charts"))))))


(define-easy-handler (units2cmb :uri "/units2cmb") ()
  (let ((combined (make-combined (get-parameter "--NAME")
			   (mapcan (lambda (name)
				     (if-let (ms (find name (shell-object *bad-default-shell* "units") :key #'unit-name :test #'equal))
				       (list ms)))
				   (get-checked-parameters))))
	(oid (make-oid)))
    (setf (gethash oid *combineds*) combined)
    (nabu-page "New combined chart"
      (let ((url (format nil "/chart?oid=~a" (urlencode oid))))
	(htm (:a :href url (str (cmb-name combined))))))))

(defun glyph-url (glyph)
  (bind (((kind datum) (glyph-img glyph)))
    (case kind
      (:uri datum))))

(defun combined-404 (oid)
  (setf (return-code*) +http-not-found+)
  (nabu-page "Chart not found"
    ({alert} ("warning") "Chart " (:code (str oid)) " not found.")))

(define-easy-handler  (show-chart :uri "/chart") (oid)
  (if-let (combined (gethash oid *combineds*))
    (progn
      (nabu-page (cmb-name combined)
	({row} ({active} ("warning") (format nil "/edit-chart?oid=~a" (urlencode oid)) "Edit") " "
	       ({active} ("danger") (format nil "/rm-chart?oid=~a" (urlencode oid)) "Remove"))
	:hr
	({row}
	  (:table :class "table table-hover"
		  (maphash (lambda (char images)
			     (htm (:tr
				   (:td (str char))
				   (:td
				    (dolist (img images)
				      (htm (:img :src img)))))))
			   (cmb-ab combined))))))
    (progn
      (combined-404 oid))))

(define-easy-handler (compare-combineds :uri "/compare") ()
  (let ((request-oids (get-checked-parameters)))
    (bind (((:values combineds oids+names)
	    (let@ rec ((oids request-oids)
		       (combineds)
		       (oids+names))
	      (if oids
		  (let ((oid (first oids)))
		    (if-let (combined (gethash oid *combineds*))
		      (rec (rest oids)
			   (cons combined combineds)
			   (cons (list oid (cmb-name combined)) oids+names))
		      (rec (rest oids) combineds oids+names)))
		  (values (reverse combineds) (reverse oids+names)))))
	   (union (ab-union combineds)))
      (nabu-page "Comparative chart"
	(:div (dolist (oid+name oids+names)
		(htm ({active} ("default" :size "sm") (format nil "/chart?oid=~a" (urlencode (first oid+name))) (str (second oid+name))) " ")))
	:hr
	((:table :class "table table-hover table-bordered")
	 (:thead
	  (:tr
	   (:th " ")
	   (dolist (oid+name oids+names)
	     (htm (:th (str (second oid+name)))))))
	 (:tbody
	  (dolist (char union)
	    (htm (:tr
		  (:td (str char))
		  (dolist (combined combineds)
		    (htm (:td
			  (dolist (img (gethash char (cmb-ab combined)))
			    (htm (:img :src img)))))))))))))))

(define-easy-handler (rm-chart :uri "/rm-chart") (oid redirect)
  (if-let (combined (gethash oid *combineds*))
    (progn
      (remhash oid *combineds*)
      (if redirect
	  (redirect (format nil "/charts?removed=~a" (cmb-name combined)))
	  (nabu-page "Chart removed"
	    ({alert} ("warning") "Chart " (str (cmb-name combined)) " removed."))))
    (combined-404 oid)))

(defun web-start (port)
  (start (make-instance 'easy-acceptor :port port)))
