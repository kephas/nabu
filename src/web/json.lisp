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


(defmethod encode-json ((object (eql :false)) &optional stream)
  (princ "false" stream))


(defun serve-json (json &key (status 200))
  `(,status
    (:content-type "application/json; charset=utf-8")
    (,json)))

(defun serve-json* (object)
  (serve-json (encode-json-to-string object)))


(defroute "/api/user/:uid/units" (&key uid)
  (serve-json
   (if-let (units (shell-list *root-shell* "users" uid "units"))
     (encode-json-to-string units)
     "[]")))

(defroute ("/api/user/:uid/units" :method :POST) (&key uid uri)
  (labels ((result (success? format &rest arguments)
	     (encode-json-plist-to-string
	      (list :type (case success? (:ok "success") (:err "danger"))
		    :message (apply #'format nil format arguments))))
	   (oid->name (oid)
	     (values (unit-name (shell-object *root-shell* "users" uid "units" oid)) t))
	   (done (oid ok action)
	     (handler-case
		 (result :ok "Unit ~a ~a." (oid->name oid) ok)
	       (error (e)
		 (declare (ignore e))
		 (result :err "An error occurred after ~a of unit <code>~a</code>"
			 action oid)))))
    (bind (((:values new creation-error)
	    (handler-case
		(http-manifest->object uri)
	      (error (e)
		(declare (ignore e))
		(values nil (result :err "Nothing could be created from manifest at <a href='~a'><code>~a</code></a>." uri uri)))))
	   (unit-oid (make-oid)))
      (flet ((store-new (cmb?)
	       (setf (shell-object *root-shell* "users" uid "units" unit-oid) new)
	       (if cmb?
		   (setf (shell-object *root-shell* "users" uid "combineds" (make-oid))
			 (build-combined (unit-name new) (list new))))))
	(if creation-error
	    creation-error
	    (if-let (existing (find-existing-unit uid uri))
	      (bind (((old-oid old-unit) existing))
		(if (string= (unit-manifest old-unit) (unit-manifest new))
		    (done old-oid "is up-to-date" "update")
		    (progn
		      (let@ rec ((entries (find-unit-charts uid old-unit)))
			(if entries
			    (bind ((((cmb-oid cmb) &rest remaining) entries))
			      (shell-remove! *root-shell* "users" uid "combineds" cmb-oid)
			      (setf (shell-object *root-shell* "users" uid "combineds" (make-oid))
				    (build-combined (cmb-name cmb) (substitute new old-unit (cmb-units cmb))))
			      (rec remaining))
			    (progn
			      (shell-remove! *root-shell* "users" uid "units" old-oid)
			      (store-new nil)
			      (done unit-oid "has been updated" "update")))))))
	      (progn
		(store-new t)
		(done unit-oid "has been created" "creation"))))))))

(defun encode-chart-to-json (combined oid &optional ab/obj?)
  (let ((max-baselines (make-hash-table :test 'equal)))
    (labels ((glyphs-list (glyphs)
	       (let ((max-baseline 0)
		     (any-active? nil))
		 (with-array ()
		   (dolist (glyph glyphs)
		     (let ((active? (glyph-active-in-chart? glyph combined)))
		       (when (> (glyph-bl glyph) max-baseline)
			 (setf max-baseline (glyph-bl glyph)))
		       (when active?
			 (setf any-active? t))
		       (as-array-member ()
			 (with-object ()
			   (encode-object-member :id (glyph-id glyph))
			   (encode-object-member :active (if active? t :false))
			   (encode-object-member :url (glyph-url glyph))
			   (encode-object-member :pos (glyph-pos/display* glyph))
			   (encode-object-member :height (glyph-height glyph))
			   (encode-object-member :width (glyph-width glyph))
			   (encode-object-member :baseline-offset (glyph-bl glyph)))))))
		 (values max-baseline any-active?))))
      (values
       (with-output-to-string (*json-output*)
	 (with-object ()
	   (encode-object-member :name (cmb-name combined))
	   (encode-object-member :oid oid)
	   (encode-object-member :scale (cmb-scale combined))
	   (encode-object-member :public-oid (cmb-public combined))
	   (as-object-member (:alphabet)
	     (if ab/obj?
		 (with-object ()
		   (maphash (lambda (char glyphs)
			      (let ((max-baseline)
				    (any-active?))
				(as-object-member (char)
				  (with-object ()
				    (as-object-member (:glyphs)
				      (bind (((:values mb aa) (glyphs-list glyphs)))
					(setf max-baseline mb
					      any-active? aa)))
				    (encode-object-member :inactive (not any-active?))
				    (encode-object-member :max-baseline-offset max-baseline)))
				(setf (gethash char max-baselines) max-baseline)))
			    (cmb-ab combined)))
		 (with-array ()
		   (maphash (lambda (char glyphs)
			      (let ((max-baseline)
				    (any-active?))
				(as-array-member ()
				  (with-object ()
				    (encode-object-member :char char)
				    (as-object-member (:glyphs)
				      (bind (((:values mb aa) (glyphs-list glyphs)))
					(setf max-baseline mb
					      any-active? aa)))
				    (encode-object-member :inactive (not any-active?))
				    (encode-object-member :max-baseline-offset max-baseline)))
				(setf (gethash char max-baselines) max-baseline)))
			    (cmb-ab combined)))))))
       max-baselines))))

(defmacro with-json-error (&body body)
  `(handler-case
       (progn ,@body)
     (not-shell () (serve-json "{}" :status 404))
     (error (e)
       (if (config* :debug)
	   (error e)
	   (serve-json "{}" :status 500)))))

(defroute "/api/user/:uid/charts" (&key uid)
  (with-json-error
    (serve-json
     (with-output-to-string (*json-output*)
       (with-array ()
	 (dolist (entry (shell-list *root-shell* "users" uid "combineds"))
	   (as-array-member () (write (encode-chart-to-json (second entry) (first entry))
				      :stream *json-output* :escape nil))))))))

(defmacro if-user-chart (chart uid oid &body body)
  `(with-json-error
     (if-let (,chart (shell-object *root-shell* "users" ,uid "combineds" ,oid))
       (progn ,@body)
       (serve-json "{}" :status 404))))

(defroute "/api/user/:uid/charts/:oid" (&key uid oid)
  (if-user-chart combined uid oid
    (serve-json (encode-chart-to-json combined oid))))

(defroute "/api/public/charts/:oid" (&key oid)
  (with-json-error
    (if-let (combined (shell-object *root-shell* "public" "combineds" oid))
      (serve-json (encode-chart-to-json combined oid))
      (serve-json "{}" :status 404))))

(defroute ("/api/user/:uid/charts/:oid" :method :POST) (&key uid oid)
  (if-user-chart combined uid oid
    (let* ((%alphabet (body-parameter *request* "alphabet"))
	   (%glyphs (getjso "glyphs" (first %alphabet))))
      (setf (cmb-scale combined) (body-parameter *request* "scale"))
      (let@ rec ((%glyph (first %glyphs))
		 (%glyphs (rest %glyphs))
		 (chars (rest %alphabet))
		 (inactives nil))
	(let* ((glyph (cmb-find-glyph combined (getjso "id" %glyph)))
	       (inactives (if (from-json-bool (getjso "active" %glyph)) inactives (cons glyph inactives))))
	  (setf (glyph-bl glyph) (getjso "baselineOffset" %glyph)
		(glyph-height glyph) (getjso "height" %glyph)
		(glyph-width glyph) (getjso "width" %glyph))
	  (if %glyphs
	      (rec (first %glyphs) (rest %glyphs) chars inactives)
	      (if chars
		  (let ((%glyphs (getjso "glyphs" (first chars))))
		    (rec (first %glyphs) (rest %glyphs) (rest chars) inactives))
		  (progn
		    (setf (cmb-inactive combined) inactives)
		    (serve-json "{}")))))))))

(defroute ("/api/user/:uid/charts/:oid/publish" :method :POST) (&key uid oid)
  (if-user-chart combined uid oid
    (serve-json
     (if-let (public-oid (cmb-public combined))
       (encode-json-plist-to-string (list :public-oid public-oid))
       (let ((new-oid (make-oid)))
	 (setf (shell-object *root-shell* "public" "combineds" new-oid) combined
	       (cmb-public combined) new-oid)
	 (encode-json-plist-to-string (list :public-oid new-oid)))))))

(defroute ("/api/user/:uid/charts/:oid" :method :DELETE) (&key uid oid)
  (if-user-chart combined uid oid
    (progn
      (if-let (public-oid (cmb-public combined))
	(shell-remove! *root-shell* "public" "combineds" public-oid))
      (shell-remove! *root-shell* "users" uid "combineds" oid))))

(defroute "/api/user/:uid/comparative-chart" (&key uid _parsed)
  (let ((oids (get-parsed :oids _parsed))
	(errors nil)
	(max-baselines (make-hash-table :test 'equal))
	(charts nil))
    (with-output-to-string (*json-output*)
      (with-object ()
	(as-object-member ("charts")
	  (with-array ()
	    (dolist (oid oids)
	      (if-let (combined (shell-object *root-shell* "users" uid "combineds" oid))
		(bind (((:values chart local-max-baselines) (encode-chart-to-json combined oid t)))
		  (push combined charts)
		  (maphash (lambda (char value)
			     (when (> value (gethash char max-baselines 0))
			       (setf (gethash char max-baselines) value)))
			   local-max-baselines)
		  (as-array-member () (princ chart *json-output*)))
		(push oid errors)))))
	(encode-object-member "maxBaselineOffsets" max-baselines)
	(encode-object-member "chars" (ab-union charts))
	(encode-object-member "errors" errors)))))

(defroute "/api/user/:uid/setup" (&key uid)
  (with-json-error
    (serve-json
     (encode-json-plist-to-string
      (list :name (shell-object *root-shell* "users" uid "settings" "name"))))))

(defroute ("/api/user/:uid/setup" :method :POST) (&key uid)
  (with-json-error
    (let ((%name (body-parameter *request* "name")))
      (setf (shell-object *root-shell* "users" uid "settings" "name") %name))))
