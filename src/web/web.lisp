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


(defclass <nabu> (caveman2:<app>)())
(defparameter *app* (make-instance '<nabu>))


(defun get-parsed (name parsed)
  (cdr (assoc name parsed :test #'string=)))


(defvar *bad-default-shell* nil)

(defun open-storage ()
  (case (config* :storage)
    (:memory
     (setf *bad-default-shell* (make-hash-table :test 'equal)))
    (:elephant
     (ele:open-store (config* :ele-store))
     (let ((shell (ele:get-from-root "bad-default-shell")))
       (unless shell
	 (setf shell (ele:make-btree))
	 (ele:add-to-root "bad-default-shell" shell))
       (setf *bad-default-shell* shell)))))

(defmacro nabu-page (title &body body)
  `(with-html-output-to-string (out nil :indent t)
     (:html
      (:head
       (:title (fmt "NABU - ~a" ,title))
       (:meta :name "viewport" :content "width=device-width")
       (:link :href "/static/css/bootstrap.min.css" :rel "stylesheet")
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
	   (:li (:a :href "/charts" "Charts"))
	   (:li (:a :href "/shell" "Shell"))))))
       ((:div :class "container")
	(:h1 (str ,title))
	,@body
	(:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js")
	(:script :src "/static/js/bootstrap.min.js")
	(:script :src "/static/js/local.js")
	(:script :src "/static/js/sticky-tabs.js"))))))

(defroute "/" ()
  (redirect *response* (if (shell-list *bad-default-shell* "combineds")
			   "/charts" "/units")))

(defparameter +s-filter+ "S-FILTER")

(defun filtering? ()
  (query-parameter *request* +s-filter+))

(defun filter-units (oid+units)
  (handler-case
      (let ((query (if-let (s-filter (query-parameter *request* +s-filter+))
		     (let ((sexpr (read-all-from-string s-filter)))
		       (if (null sexpr)
			   (constantly t)
			   ($sexpr sexpr)))
		     (constantly t))))
	(do-search query oid+units :key #'second))
    (error ())))

(defroute "/units" (&key created uptodate updated)
  (nabu-page "Units"
    (macrolet ((notice (var ok action)
		 `(when ,var
		    (handler-case
			(let ((name (unit-name (shell-object *bad-default-shell* "units" ,var))))
			  ({alert} ("success" t) "Unit " (str name) ,ok))
		      (error ()
			({alert} ("danger" t) "Error after " ,action " of unit " (:code (str ,var))))))))
      (notice created " created." "creation")
      (notice uptodate " is up-to-date." "update")
      (notice updated " has been updated." "update"))
    (if-let (current-filter (filtering?))
      (htm (:p "Current filter:"
	       (:code (esc current-filter))
	       ({active} ("success" :size "sm") "/units" "View all units")))
      (htm (:form :role "form" :method "GET" :action "/units"
		  ({row}
		    (:div (:label "Add filter:")))
		  ({row}
		    ({col} 8 10 (:input :class "form-control" :type "text" :name "S-FILTER"))
		    ({col} 4 2 ({submit} ("default") "Filter"))))))
    (:hr)
    (:form :role "form" :method "POST" :action "/units2cmb"
	   (:div :class "form-group"
		 (dolist (oid+unit (filter-units (shell-list *bad-default-shell* "units")))
		   (htm
		    ({row}
		      ({col} 12 12 ({checkbox} "UNITS[]" (first oid+unit)
				     (str (unit-name (second oid+unit))))))))
		 ({row}
		   ({col} 12 6
		     (:div :class "input-group"
			   (:label :class "input-group-addon" "Chart name:")
			   (:input :class "form-control" :name "NAME")))
		   ({col} 4 6
		     ({submit} ("primary") "Combine units")))))
    (:hr)
    (:h2 "Add unit")
    ((:form :role "form" :method "POST" :action "/addunit")
     ({row}
       ({col} 12 10
	 (:div :class "input-group"
	       (:label :class "input-group-addon" "URI ")
	       (:input :class "form-control" :type "url" :name "URI")))
       ({col} 12 2
	 ({submit} ("primary") "Add"))))))

(defroute ("/addunit" :method :POST) (&key uri)
  (let ((new (http-manifest->object uri))
	(unit-oid (make-oid)))
    (flet ((store-new (cmb?)
	     (setf (shell-object *bad-default-shell* "units" unit-oid) new)
	     (if cmb?
		 (setf (shell-object *bad-default-shell* "combineds" (make-oid))
		       (build-combined (unit-name new) (list new)))))
	   (notice (var oid)
	     (redirect *response* (format nil "/units?~a=~a" var (urlencode oid)))))
      (if-let (existing (find-existing-unit uri))
	(bind (((old-oid old-unit) existing))
	  (if (string= (unit-manifest old-unit) (unit-manifest new))
	      (notice "UPTODATE" old-oid)
	      (progn
		(let@ rec ((entries (find-unit-charts old-unit)))
		  (if entries
		      (bind ((((cmb-oid cmb) &rest remaining) entries))
			(shell-remove! *bad-default-shell* "combineds" cmb-oid)
			(setf (shell-object *bad-default-shell* "combineds" (make-oid))
			      (build-combined (cmb-name cmb) (substitute new old-unit (cmb-units cmb))))
			(rec remaining))
		      (progn
			(shell-remove! *bad-default-shell* "units" old-oid)
			(store-new nil)
			(notice "UPDATED" unit-oid)))))))
	(progn
	  (store-new t)
	  (notice "CREATED" unit-oid))))))

(defroute "/charts" (&key created removed)
  (nabu-page "Charts"
    (when removed
      (htm ({alert} ("warning" t) "Chart " (str removed) " removed")))
    (when created
      (handler-case
	  (let ((uri (format nil "/chart?OID=~a" (urlencode created)))
		(name (cmb-name (shell-object *bad-default-shell* "combineds" created))))
	    ({alert} ("success" t) "Combined chart " (:a :href uri (str name)) " created!"))
	(error ()
	  ({alert} ("danger" t) "Error after creation of chart " (:code (str created))))))
    (:form :role "form" :method "GET" :action "/compare"
	   (map nil (lambda (oid+chart)
		      (bind (((oid chart) oid+chart))
			({col} 12 12
			  ({checkbox} "OIDS[]" oid
			    ({active} ("default" :size "lg") (format nil "/chart?OID=~a" (urlencode oid)) (str (cmb-name chart)))
			    " " ({active} ("warning" :size "sm") (format nil "/edit-chart?OID=~a" (urlencode oid)) "Edit")
			    " " ({active} ("danger" :size "sm") (format nil "/rm-chart?OID=~a&REDIRECT=t" (urlencode oid)) "Remove")))))
		(remove-used-unit-charts (shell-list *bad-default-shell* "combineds") :key #'second))
	   (when (shell-list *bad-default-shell* "combineds")
	     (htm ({submit} ("primary") "Compare charts"))))))


(defroute ("/units2cmb" :method :POST) (&key name _parsed)
  (let ((combined (build-combined name
			   (mapcan (lambda (oid)
				     (if-let (unit (shell-object *bad-default-shell* "units" oid))
				       (list unit)))
				   (get-parsed :units _parsed))
			   :allow-unit nil))
	(oid (make-oid)))
    (setf (shell-object *bad-default-shell* "combineds" oid) combined)
    (redirect *response* (format nil "/charts?CREATED=~a" (urlencode oid)))))

(defun glyph-url (glyph)
  (bind (((kind datum) (glyph-img glyph)))
    (case kind
      (:uri datum))))

(defun combined-404 (oid)
  (setf (clack.response:status *response*) 404)
  (nabu-page "Chart not found"
    ({alert} ("warning") "Chart " (:code (str oid)) " not found.")))

(defmacro {glyph} (glyph)
  `(let ((uri (glyph-url ,glyph))
	 (pos (handler-case
		  (format nil "~:[~;~1:*~{~a~a~}~]"
			  (commatize (glyph-pos/display ,glyph) "."))
		(error () ""))))
     (htm (:img :data-toggle "tooltip" :data-placement "right"
		:title pos :src uri))))

(defroute "/chart" (&key oid)
  (if-let (combined (shell-object *bad-default-shell* "combineds" oid))
    (progn
      (nabu-page (cmb-name combined)
	({row} ({active} ("warning") (format nil "/edit-chart?OID=~a" (urlencode oid)) "Edit") " "
	       ({active} ("danger") (format nil "/rm-chart?OID=~a" (urlencode oid)) "Remove"))
	:hr
	({row}
	  (:table :class "table table-hover"
		  (maphash (lambda (char glyphs)
			     (htm (:tr
				   (:td (str char))
				   (:td
				    (dolist (glyph (sort-by-pos glyphs))
				      ({glyph} glyph))))))
			   (cmb-ab combined))))))
    (progn
      (combined-404 oid))))

(defun oids->query (param oids)
  (format nil "~{~a=~a&~}" (mapcan (lambda (oid) (list param (urlencode oid))) oids)))

(defroute "/compare" (&key _parsed)
  (let ((request-oids (get-parsed :oids _parsed)))
    (bind (((:values combineds oids+names)
	    (let@ rec ((oids request-oids)
		       (combineds)
		       (oids+names))
	      (if oids
		  (let ((oid (first oids)))
		    (if-let (combined (shell-object *bad-default-shell* "combineds" oid))
		      (rec (rest oids)
			   (cons combined combineds)
			   (cons (list oid (cmb-name combined)) oids+names))
		      (rec (rest oids) combineds oids+names)))
		  (values (reverse combineds) (reverse oids+names)))))
	   (oids (mapcar #'first oids+names))
	   (union (ab-union combineds)))
      (flet ((other-uri (oids)
	       (format nil "/compare?~a" (oids->query "OIDS[]" oids))))
	(nabu-page "Comparative chart"
	  (:div
	   (let@ rec ((oid (caar oids+names))
		      (name (cadar oids+names))
		      (next (cdr oids+names))
		      (first t))
	     (unless first
	       ({active} ("default" :size "xs") (other-uri (swap-left oid oids)) "<"))
	     (htm ({active} ("default" :size "sm") (format nil "/chart?OID=~a" (urlencode oid)) (str name)) " ")
	     (when next
	       ({active} ("default" :size "xs") (other-uri (swap-right oid oids)) ">")
	       (htm "   ")
	       (rec (caar next) (cadar next) (cdr next) nil))))
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
			  (dolist (glyph (sort-by-pos (gethash char (cmb-ab combined))))
			    ({glyph} glyph)))))))))))))))

(defroute "/rm-chart" (&key oid redirect)
  (if-let (combined (shell-object *bad-default-shell* "combineds" oid))
    (progn
      (shell-remove! *bad-default-shell* "combineds" oid)
      (if redirect
	  (redirect *response* (format nil "/charts?REMOVED=~a" (cmb-name combined)))
	  (nabu-page "Chart removed"
	    ({alert} ("warning") "Chart " (str (cmb-name combined)) " removed."))))
    (combined-404 oid)))

(defun clackup (port &optional config-file)
  (read-configuration! config-file)
  (open-storage)
  (shell-mksub! *bad-default-shell* "units")
  (shell-mksub! *bad-default-shell* "combineds")
  (setf drakma:*drakma-default-external-format* :utf8)
  (clack:clackup
   (clack.builder:builder
    (clack.middleware.static:<clack-middleware-static>
     :path "/static/"
     :root (merge-pathnames #p"static/" (asdf:system-source-directory "nabu")))
    *app*) :port port :debug (config* :debug) :server (config* :server)))
