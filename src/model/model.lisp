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

(defclass unit ()
  ((name :initarg :name :reader unit-name)
   (glyphs :initarg :glyphs :accessor unit-glyphs)
   (uri :initarg :uri :reader unit-uri)
   (metadata :initarg :meta :accessor nabu-metadata))
  (:default-initargs :glyphs nil :meta (make-hash-table :test 'equal)))

(defclass glyph ()
  ((char :initarg :char :reader glyph-char)
   (pos :initarg :pos :reader glyph-pos)
   (image :initarg :img :reader glyph-img)))

(defun read-images-manifest (uri)
  (with-input-from-string (manifest (drakma:http-request uri))
    (let ((description (read manifest)))
      (let@ rec ((spec (read manifest nil))
		 (glyphs))
	(if spec
	    (rec (read manifest nil)
		 (cons (make-instance 'glyph
				      :img `(:uri ,(puri:merge-uris (car spec) uri))
				      :char (cadr spec)
				      :pos (cddr spec))
		       glyphs))
	    (make-instance 'unit :name (first description) :glyphs (reverse glyphs) :uri uri
			   :meta (alist->hash (second description))))))))

(defclass combined ()
  ((name :initarg :name :reader cmb-name)
   (units :initarg :units :reader cmb-units)
   (alphabet :initarg :ab :reader cmb-ab)))

(defclass unit-chart (combined) ())

(defun make-combined (name units &key (allow-unit t))
  (make-instance (if (and allow-unit (= 1 (length units))) 'unit-chart 'combined)
		 :name name :units units
		 :ab (let ((combined (make-hash-table :test 'equal)))
		       (dolist (unit units combined)
			 (dolist (glyph (unit-glyphs unit))
			   (push glyph (gethash (glyph-char glyph) combined)))))))

(defun ab-union (combineds)
  "Return the union of the alphabets of several combineds, as a sorted list of chars"
  (let ((union (make-hash-table :test 'equal)))
    (dolist (combined combineds)
      (dolist (entry (hash-table-keys (cmb-ab combined)))
	(setf (gethash entry union) t)))
    (sort (hash-table-keys union) #'string<)))

(defun remove-used-unit-charts (charts &key key)
  "Remove from a list of charts all unit charts present in combined charts"
  (with-lisp1 (key)
    (let ((used (mapcan #'cmb-units (remove-if (lambda (chart)
						 (typep chart 'unit-chart))
					       (mapcar key charts)))))
      (mapcan (lambda (chart)
		(unless (and (typep (key chart) 'unit-chart)
			     (find (unit-name (first (cmb-units (key chart)))) used :key #'unit-name :test #'equal))
		  (list chart)))
	      charts))))


#| Shell

   A shell is a container for objects. It is meant to be the larger
   object from which a user accesses the system |#

(defclass shell ()
  ((containers :initarg :cnt))
  (:default-initargs :cnt (make-hash-table :test 'equal)))

(ele:defpclass ele-shell (shell)
  ((containers :initarg :cnt))
  (:default-initargs :cnt (ele:make-btree)))


(defgeneric %get-shell-value (shell context key))
(defgeneric %set-shell-value (shell context key value))
(defgeneric %rm-shell-value (shell context key))
(defgeneric %make-shell-container (shell))
(defgeneric %shell-container-count (shell context))
(defgeneric %map-shell-container (shell context function))
(defgeneric %shell-container? (shell value))

(define-condition shell-path-not-traversable (error)
  ((shell :initarg :shell)
   (path :initarg :path))
  (:report (lambda (condition stream)
	     (with-slots (shell path) condition
	       (format stream "~a ~{~a~a~}" shell (commatize path "/"))))))

(defmacro with-path-error (shell path &body body)
  (once-only (shell path)
    `(handler-case
	 (progn ,@body)
       (error ()
	 (error (make-condition 'shell-path-not-traversable :shell ,shell :path ,path))))))

(defun %do-shell-path (shell path thunk)
  (with-path-error shell path
    (let@ rec ((object (slot-value shell 'containers))
	       (path path))
      (if (= 1 (length path))
	  (funcall thunk object (first path))
	  (rec (%get-shell-value shell object (first path)) (rest path))))))

(defun shell-object (shell &rest path)
  (handler-case
      (if path
	  (%do-shell-path shell path
			  (lambda (context key)
			    (%get-shell-value shell context key)))
	  (slot-value shell 'containers))
    (error ()
      (values nil nil))))

(defun (setf shell-object) (value shell &rest path)
  (%do-shell-path shell path
		  (lambda (context key)
		    (%set-shell-value shell context key value))))

(defun shell-ensure (shell &rest path)
  "Ensure that a shell path designates a container"
  (with-path-error shell path
    (let@ rec ((object (slot-value shell 'containers))
	       (path path))
      (when path
	(bind (((:values _ found?) (%get-shell-value shell object (first path))))
	  (unless found?
	    (%set-shell-value shell object (first path) (%make-shell-container shell)))
	  (rec (%get-shell-value shell object (first path)) (rest path)))))))

(defun shell-remove (shell &rest path)
  "Remove an entry from a shell"
  (%do-shell-path shell path
		  (lambda (context key)
		    (%rm-shell-value shell context key))))

(defun shell-count (shell &rest path)
  "Count the objects in a shell container"
  (%shell-container-count shell (apply #'shell-object (cons shell path))))

(defun shell-map (shell function &rest path)
  "Apply a function to all objects in a shell container"
  (%map-shell-container shell (apply #'shell-object (cons shell path)) function))

(defun shell-list (shell &rest path)
  "Return all key/object pairs contained in a shell container"
  (handler-case
      (let ((pairs))
	(%map-shell-container shell (apply #'shell-object shell path) (lambda (k v) (push (list k v) pairs)))
	(reverse pairs))
    (error ()
      (values))))


#| In-memory shell |#

(defmethod %get-shell-value ((shell shell) context key)
  (gethash key context))

(defmethod %set-shell-value ((shell shell) context key value)
  (setf (gethash key context) value))

(defmethod %rm-shell-value ((shell shell) context key)
  (remhash key context))

(defmethod %make-shell-container ((shell shell))
  (make-hash-table :test 'equal))

(defmethod %shell-container-count ((shell shell) context)
  (hash-table-count context))

(defmethod %map-shell-container ((shell shell) context function)
  (maphash function context))

(defmethod %shell-container? ((shell shell) value)
  (typep value 'hash-table))


#| Persistent shell |#

(defmethod %get-shell-value ((shell ele-shell) context key)
  (ele:get-value key context))

(defmethod %set-shell-value ((shell ele-shell) context key value)
  (setf (ele:get-value key context) value))

(defmethod %rm-shell-value ((shell ele-shell) context key)
  (ele:remove-kv key context))

(defmethod %make-shell-container ((shell ele-shell))
  (ele:make-btree))

(defmethod %shell-container-count ((shell ele-shell) context)
  (let ((count 0))
    (ele:map-btree (lambda (k v)
		     (declare (ignore k v))
		     (incf count))
		   context)
    count))

(defmethod %map-shell-container ((shell ele-shell) context function)
  (ele:map-btree function context))

(defmethod %shell-container? ((shell ele-shell) value)
  (typep value 'ele:btree))