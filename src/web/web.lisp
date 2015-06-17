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


(defmacro {setf-angular} (var value &optional (string? t))
  "Create an element that will set a variable in AngularJS"
  `(htm
    (:span :style "display:none"
	   :ng-init ,(if string?
			 `(format nil "~a=\"~a\"" ,var ,value)
			 `(format nil "~a=~a" ,var ,value)))))


(defvar *bad-default-shell* nil)
(defvar *public-shell* nil)

(defmacro ensure-shell (var key)
  `(let ((shell (ele:get-from-root ,key)))
     (unless shell
       (setf shell (ele:make-btree))
       (ele:add-to-root ,key shell))
     (setf ,var shell)))

(defun open-storage ()
  (case (config* :storage)
    (:memory
     (setf *bad-default-shell* (make-hash-table :test 'equal))
     (setf *public-shell* (make-hash-table :test 'equal)))
    (:elephant
     (ele:open-store (config* :ele-store))
     (ensure-shell *bad-default-shell* "bad-default-shell")
     (ensure-shell *public-shell* "public-shell"))))

(defmacro nabu-page ((title &key public?) &body body)
  `(with-html-output-to-string (out nil :indent t)
     (:html :ng-app "nabuApp"
      (:head
       (:title (fmt "NABU - ~a" ,title))
       (:meta :name "viewport" :content "width=device-width")
       (:link :href "/static/css/bootstrap.min.css" :rel "stylesheet")
       (:link :href "/static/css/local.css" :rel "stylesheet"))
      (:body
       ((:div :class "navbar navbar-inverse navbar-fixed-top" :role "navigation")
	((:div :class "container")
	 ((:div :class "navbar-header")
	  ({collapse-btn} ".nabu-navbar-collapse")
	  ((:a :class "navbar-brand" :href "#") "NABU"))
	 ,(unless public?
	    `(htm
	      ((:div :class "collapse navbar-collapse nabu-navbar-collapse")
	       ((:ul :class "nav navbar-nav")
		(:li (:a :href "/units" "Units"))
		(:li (:a :href "/charts" "Charts"))
		(:li (:a :href "/shell" "Shell"))))))))
       ((:div :class "container")
	(:h1 (str ,title))
	,@body
	(:script :src "/static/js/jquery.js")
	(:script :src "/static/js/bootstrap.js")
	(:script :src "/static/js/angular.js")
	(:script :src "/static/js/angular-animate.js")
	(:script :src "/static/js/angular-aria.js")
	(:script :src "/static/js/angular-cookies.js")
	(:script :src "/static/js/angular-messages.js")
	(:script :src "/static/js/angular-route.js")
	(:script :src "/static/js/angular-sanitize.js")
	(:script :src "/static/js/alerts.js")
	(:script :src "/static/js/test.js")
	(:script :src "/static/js/nabu.js"))))))

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

(defroute "/units" ()
  (nabu-page ("Units")
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
    (:div :ng-controller "unitsCtrl"
	  (:nabu-alerts)
	  (:form :role "form" :method "POST" :action "/units2cmb"
		 (:div :class "form-group"
		       ({row} :ng-repeat "item in shellList"
			      ({col} 12 12 ({checkbox} "UNITS[]" "{{item[0]}}" "{{item[1].name}}")))
		       ({row} :ng-show "shellList.length > 1"
			 ({col} 12 6
			   (:div :class "input-group"
				 (:label :class "input-group-addon" "Chart name:")
				 (:input :class "form-control" :name "NAME")))
			 ({col} 4 6
			   ({submit} ("primary") "Combine units")))))
	  (:hr)
	  (:h2 "Add unit")
	  ((:form :role "form")
	   ({row}
	     ({col} 12 10
	       (:div :class "input-group"
		     (:label :class "input-group-addon" "URI ")
		     (:input :ng-model "manifestUri" :class "form-control" :type "url" :name "URI")))
	     ({col} 12 2
	       ({button} ("primary") :ng-click "submit()" "Add")))))))

(defroute "/charts" (&key created removed)
  (nabu-page ("Charts")
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
			    " " ({active} ("danger" :size "sm") (format nil "/rm-chart?OID=~a&REDIRECT=t" (urlencode oid)) :disabled "disabled" "Remove")))))
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
  (nabu-page ("Chart not found")
    ({alert} ("warning") "Chart " (:code (str oid)) " not found.")))

(defmacro {glyph} (glyph)
  `(let ((uri (glyph-url ,glyph))
	 (pos (glyph-pos/display* ,glyph)))
     (htm (:img :data-toggle "tooltip" :data-placement "right"
		:title pos :src uri))))

(defroute "/chart" (&key oid)
  (nabu-page ("{{name}}")
    (:div :ng-controller "chartCtrl"
	  ({setf-angular} "chartOid" oid)
	  (:span :ng-init "refresh()")
	  ({row} ({active} ("warning") (format nil "/edit-chart?OID=~a" (urlencode oid)) "Edit") " "
		 ({active} ("danger") (format nil "/rm-chart?OID=~a" (urlencode oid)) "Remove"))
	  :hr
	  (:table :class "table table-hover"
		  (:tr :ng-repeat "entry in chart.alphabet" :ng-hide "entry.inactive"
		       (:td "{{entry.char}}")
		       (:td :style "display:flex;align-items:flex-end"
			    (:nabu-glyph :ng-repeat "glyph in entry.glyphs" :ng-show "glyph.active")))))))

(defroute "/edit-chart" (&key oid)
  (nabu-page ("{{name}}")
    (:div :ng-controller "chartEditCtrl"
	  ({setf-angular} "chartOid" oid)
	  (:span :ng-init "refresh()")
	  (:nabu-alerts)
	  ({row} ({active} ("info") (format nil "/chart?OID=~a" (urlencode oid)) "View") " "
		 ({button} ("warning") :ng-click "submit()" "Save modifications")
		 ({button} ("warning") :ng-hide "chart.publicOid" :ng-click "publish()" "Make public")
		 (:span :ng-show "chart.publicOid"
			({button} ("warning") :ng-click "unpublish()" "Remove public view")
			({active} ("info" :ng t) "/pub/chart/{{chart.publicOid}}" "Public URL")))
	  :hr
	  #|(:div :class "input-group"
		     (:label :class "input-group-addon" "Scale ")
		     (:input :type "number" :ng-model "chart.scale"))|#
	  (:table :class "table table-hover"
		  (:tr :ng-repeat "entry in chart.alphabet"
		       (:td "{{entry.char}}")
		       (:td (:nabu-glyph-edit :ng-repeat "glyph in entry.glyphs")))))))

(defun oids->query (param oids)
  (format nil "~{~a=~a&~}" (mapcan (lambda (oid) (list param (urlencode oid))) oids)))

(defroute "/compare" (&key _parsed)
  (let ((oids (get-parsed :oids _parsed)))
    (nabu-page ("Comparative chart")
      (:div :ng-controller "chartCompareCtrl"
	    ({setf-angular} "oidsParams" (with-output-to-string (out)
					   (dolist (oid oids)
					     (format out "OIDS[]=~a&" (urlencode oid)))))
	    (:span :ng-init "download()")
	    (:chart-reorder :ng-repeat "chart in comparison.charts")
	    :hr
	    ((:table :class "table table-hover table-bordered")
	     (:thead
	      (:tr
	       (:th "")
	       (:th :ng-repeat "chart in comparison.charts" "{{chart.name}}"))
	      (:tr :ng-repeat "char in comparison.chars"
		   :ng-init "entry={maxBaselineOffset: comparison.maxBaselineOffsets[char]}"
		   (:td "{{char}}")
		   (:td :ng-repeat "chart in comparison.charts"
			(:nabu-glyph :ng-repeat "glyph in chart.alphabet[char].glyphs")))))))))

(defroute "/rm-chart" (&key oid redirect)
  (if-let (combined (shell-object *bad-default-shell* "combineds" oid))
    (progn
      (shell-remove! *bad-default-shell* "combineds" oid)
      (if redirect
	  (redirect *response* (format nil "/charts?REMOVED=~a" (cmb-name combined)))
	  (nabu-page ("Chart removed")
	    ({alert} ("warning") "Chart " (str (cmb-name combined)) " removed."))))
    (combined-404 oid)))

#|

Public read-only

|#

(defroute "/pub/chart/:oid" (&key oid)
  (nabu-page ("{{name}}" :public? t)
    (:div :ng-controller "chartCtrl"
	  ({setf-angular} "chartOid" oid)
	  ({setf-angular} "public" "true" nil)
	  (:span :ng-init "refresh()")
	  (:table :class "table table-hover"
		  (:tr :ng-repeat "entry in chart.alphabet" :ng-hide "entry.inactive"
		       (:td "{{entry.char}}")
		       (:td :style "display:flex;align-items:flex-end"
			    (:nabu-glyph :ng-repeat "glyph in entry.glyphs" :ng-show "glyph.active")))))))


(defun clackup (port &optional config-file)
  (read-configuration! config-file)
  (open-storage)
  (shell-mksub! *bad-default-shell* "units")
  (shell-mksub! *bad-default-shell* "combineds")
  (shell-mksub! *public-shell* "combineds")
  (setf drakma:*drakma-default-external-format* :utf8)
  (clack:clackup
   (clack.builder:builder
    (clack.middleware.static:<clack-middleware-static>
     :path "/static/"
     :root (merge-pathnames #p"static/" (asdf:system-source-directory "nabu")))
    *app*) :port port :debug (config* :debug) :server (config* :server)))
