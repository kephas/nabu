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


(defun open-storage ()
  (case (config* :storage)
    (:memory
     (setf *root-shell* (make-hash-table :test 'equal)))
    (:elephant
     (ele:open-store (config* :ele-store))
     (let ((shell (ele:get-from-root "root-shell")))
       (unless shell
	 (setf shell (ele:make-btree))
	 (ele:add-to-root "root-shell" shell))
       (setf *root-shell* shell)))))

(defroute "/" ()
  (let ((*nav-links*))
    (nabu-page ("Welcome")
      (:p "Welcome to NABU!")
      (:p "This is an alpha-test version, you should contact developers to get a user account."))))

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

(defun oids->query (param oids)
  (format nil "~{~a=~a&~}" (mapcan (lambda (oid) (list param (urlencode oid))) oids)))



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

User single-page app

|#

(defun user-nav-links (uid)
  (mapcar (lambda (spec)
	    (list (format nil (first spec) uid) (second spec)))
	  '(("/user/~a/units" "Units")
	    ("/user/~a/charts" "Charts")
	    ("/user/~a/setup" "Setup"))))

(defroute "/user/:uid/*" (&key uid)
  (if-let (user (shell-object *root-shell* "users" uid))
    (let ((*nav-links* (user-nav-links uid)))
      (nabu-page ((if-let (user-name (shell-object user "settings" "name"))
		    user-name uid))
	(:base :href "/")
       	(:ng-view)))
    (list 404 nil
	  (list (let ((*nav-links*))
		  (nabu-page ("User not found")))))))

(defmacro with-cachable-html ((var &key (max-age (seconds :days 1))) &body body)
  `(list
    200
    (list :cache-control (format nil "public, max-age=~a" ,max-age))
    (list (with-html-output-to-string (,var nil :indent t) ,@body))))

(defroute "/ng/units" ()
  (with-cachable-html (out)
    (:h2 "Units")
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
    :hr
    (:h3 "Add unit")
    ((:form :role "form")
     ({row}
       ({col} 12 10
	 (:div :class "input-group"
	       (:label :class "input-group-addon" "URI ")
	       (:input :ng-model "manifestUri" :class "form-control" :type "url" :name "URI")))
       ({col} 12 2
	 ({button} ("primary") :ng-click "submit()" "Add"))))))

(defroute "/ng/charts" ()
  (with-html-output-to-string (out nil :indent t)
    (:h2 "Charts")
    (:span :ng-init "initialize()")
    (:form :role "form" :method "GET" :action "{{action}}" ;"/user/{{uid}}/compare" ;=> pb with $sce
	   ({col} 12 12 :ng-repeat "chart in charts"
		  ({checkbox} "OIDS[]" "{{chart.oid}}"
		    ({active} ("default" :size "lg" :ng t) "/user/{{uid}}/chart?oid={{chart.oid}}" "{{chart.name}}") " "
		    ({active} ("warning" :size "sm" :ng t) "/user/{{uid}}/edit-chart?oid={{chart.oid}}" "Edit") " "
		    ({active} ("danger" :size "sm" :ng t) "/user/{{uid}}/rm-chart?oid={{chart.oid}}&REDIRECT=t" :disabled "disabled" "Remove")))
	   ({submit} ("primary") :ng-show "charts.length > 1" "Compare charts"))))

(defroute "/ng/chart" ()
  (with-html-output-to-string (out nil :indent t)
    (:h2 "{{chart.name}}")
    ({row} ({active} ("warning" :ng t) "/user/{{uid}}/edit-chart?oid={{chart.oid}}" "Edit") " "
	   ({active} ("danger") "/user/{{uid}}/rm-chart?oid={{chart.oid}}" "Remove"))
    :hr
    (:table :class "table table-hover"
	    (:tr :ng-repeat "entry in chart.alphabet" :ng-hide "entry.inactive"
		 (:td "{{entry.char}}")
		 (:td :style "display:flex;align-items:flex-end"
		      (:nabu-glyph :ng-repeat "glyph in entry.glyphs" :ng-show "glyph.active"))))))

(defroute "/ng/edit-chart" ()
  (with-html-output-to-string (out nil :indent t)
    (:span :ng-init "refresh()")
    (:nabu-alerts)
    ({row} ({active} ("info" :ng t) "/user/{{uid}}/chart?oid={{chart.oid}}" "View") " "
	   ({button} ("warning") :ng-click "submit()" "Save modifications")
	   ({button} ("warning") :ng-hide "chart.publicOid" :ng-click "publish()" "Make public")
	   (:span :ng-show "chart.publicOid"
		  ({button} ("warning") :ng-click "unpublish()" "Remove public view")
		  ({active} ("info" :ng t) "/pub/chart/{{chart.publicOid}}" "Public URL")))
    :hr
    ({button} ("warning") :ng-hide "scaling" :ng-click "activateScaling()" "Scale images")
    (:div :class "input-group" :ng-show "scaling"
	  (:label :class "input-group-addon" "Scale ")
	  (:input :type "number" :ng-model "chart.scale"))
    :hr
    (:table :class "table table-hover"
	    (:tr :ng-repeat "entry in chart.alphabet"
		 (:td "{{entry.char}}")
		 (:td (:nabu-glyph-edit :ng-repeat "glyph in entry.glyphs"))))))

(defroute "/ng/compare" ()
  (with-html-output-to-string (out nil :indent t)
    (:nabu-alerts)
    (:h2 "Comparative chart")
    (:div
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
		 (:nabu-glyph :ng-repeat "glyph in chart.alphabet[char].glyphs"))))))))

(defroute "/ng/setup" ()
  (with-html-output-to-string (out nil :indent t)
    (:nabu-alerts)
    (:h2 "Session setup")
    (:form :role "form"
	   ({row} (:div :class "input-group"
			(:label :class "input-group-addon" "Name ")
			(:input :type "text" :ng-model "setup.name")))
	   ({row} (:div :class "input-group"
			({button} ("primary") :ng-click "save()" "Save"))))))

#|

Public read-only

|#

(defroute "/pub/chart/*" ()
  (let ((*nav-links*))
    (nabu-page ("")
      (:base :href "/")
      (:ng-view))))


(defun clackup (port &optional config-file)
  (read-configuration! config-file)
  (open-storage)
  (shell-ensure-hierarchy! *root-shell* '(("public" ("combineds"))("users")))
  (setf drakma:*drakma-default-external-format* :UTF-8)
  (clack:clackup
   (clack.builder:builder
    (clack.middleware.static:<clack-middleware-static>
     :path "/static/"
     :root (merge-pathnames #p"static/" (asdf:system-source-directory "nabu")))
    *app*) :port port :debug (config* :debug) :server (config* :server)))
