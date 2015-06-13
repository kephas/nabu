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


(defmethod encode-json ((object puri:uri) &optional stream)
  (princ #\" stream)
  (puri:render-uri object stream)
  (princ #\" stream))


(defroute "/units.json" ()
  (if-let (units (shell-list *bad-default-shell* "units"))
    (json:encode-json-to-string units)
    "[]"))

(defroute ("/addunit.json" :method :POST) (&key uri)
  (labels ((result (success? format &rest arguments)
	     (encode-json-plist-to-string
	      (list :type (case success? (:ok "success") (:err "danger"))
		    :message (apply #'format nil format arguments))))
	   (oid->name (oid)
	     (values (unit-name (shell-object *bad-default-shell* "units" oid)) t))
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
	       (setf (shell-object *bad-default-shell* "units" unit-oid) new)
	       (if cmb?
		   (setf (shell-object *bad-default-shell* "combineds" (make-oid))
			 (build-combined (unit-name new) (list new))))))
	(if creation-error
	    creation-error
	    (if-let (existing (find-existing-unit uri))
	      (bind (((old-oid old-unit) existing))
		(if (string= (unit-manifest old-unit) (unit-manifest new))
		    (done old-oid "is up-to-date" "update")
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
			      (done unit-oid "has been updated" "update")))))))
	      (progn
		(store-new t)
		(done unit-oid "has been created" "creation"))))))))

(defroute "/chart.json" (&key oid)
  (if-let (combined (shell-object *bad-default-shell* "combineds" oid))
    (with-output-to-string (*json-output*)
      (with-object ()
	(encode-object-member :name (cmb-name combined))
	(as-object-member (:alphabet)
	  (with-array ()
	    (maphash (lambda (char glyphs)
		       (let ((max-baseline 0))
			 (as-array-member ()
			   (with-object ()
			     (encode-object-member :char char)
			     (as-object-member (:glyphs)
			       (with-array ()
				 (dolist (glyph glyphs)
				   (when (> (glyph-bl glyph) max-baseline)
				     (setf max-baseline (glyph-bl glyph)))
				   (push (glyph-bl glyph) *leak*)
				   (as-array-member ()
				     (with-object ()
				       (encode-object-member :id (glyph-id glyph))
				       (encode-object-member :active (glyph-active-in-chart? glyph combined))
				       (encode-object-member :url (glyph-url glyph))
				       (encode-object-member :pos (glyph-pos/display* glyph))
				       (encode-object-member :baseline-offset (glyph-bl glyph)))))))
			     (encode-object-member :max-baseline-offset max-baseline)))))
		     (cmb-ab combined))))))
    (progn
      (combined-404 oid))))
