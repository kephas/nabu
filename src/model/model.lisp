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


(defun alternate-class (class)
  (if-let (alternate (getf (config* :alternate-classes) class))
    alternate
    class))

(defmacro define-alternate-maker (function class-name)
  `(defun ,function (&rest initargs)
     (apply #'make-instance (alternate-class ',class-name) initargs)))

(defmacro with-transaction ((&rest txn-args) &body body)
  `(if (eq (config* :storage) :elephant)
       (ele:with-transaction ,txn-args ,@body)
       (progn
	 ,@body)))


(defclass w/metadata ()
  ((metadata :initarg :meta :accessor nabu-metadata))
  (:default-initargs :meta (make-hash-table :test 'equal)))


(defclass unit (w/metadata)
  ((name :initarg :name :reader unit-name)
   (glyphs :initarg :glyphs :accessor unit-glyphs)
   (uri :initarg :uri :reader unit-uri)
   (manifest :initarg :manifest :reader unit-manifest))
  (:default-initargs :glyphs nil))

(define-alternate-maker make-unit unit)

(defclass glyph (w/metadata)
  ((char :initarg :char :reader glyph-char)
   (pos :initarg :pos :reader glyph-pos)
   (image :initarg :img :reader glyph-img)
   (id :initarg :id :reader glyph-id)
   (baseline :initarg :bl :accessor glyph-bl))
  (:default-initargs :id (make-oid) :bl 0))

(define-alternate-maker make-glyph glyph)

(defun pos<= (pos1 pos2)
  (let@ rec ((coord1 (first pos1))
	     (coord2 (first pos2))
	     (coords1 (rest pos1))
	     (coords2 (rest pos2)))
    (if (< coord1 coord2)
	t
	(if (> coord1 coord2)
	    nil
	    (if (or (null coords1) (null coords2))
		t
		(rec (first coords1) (first coords2) (rest coords1) (rest coords2)))))))

(defun glyph-pos/values (glyph)
  (mapcar (lambda (coord) (if (atom coord) coord (second coord))) (glyph-pos glyph)))

(defun glyph-pos/display (glyph)
  (mapcar (lambda (coord) (if (atom coord) coord (first coord))) (glyph-pos glyph)))

(defun glyph-pos/display* (glyph)
  (handler-case
      (format nil "~:[~;~1:*~{~a~a~}~]" (commatize (glyph-pos/display glyph) "."))
    (error () "")))

(defun sort-by-pos (glyphs)
  (sort (copy-seq glyphs) #'pos<= :key #'glyph-pos/values))

(defun digits? (string)
  (ppcre:scan "[0-9]+" string))

(defun parse-glyph-pos (pos)
  (mapcar (lambda (string)
	    (if (digits? string)
		(parse-integer string)
		(list string (parse-roman (string-upcase string)))))
	  (split-sequence #\. pos)))


(defgeneric %manifest->object (kind manifest-data uri manifest))

(defun manifest->object (uri manifest)
  "Take a manifest and create the described object"
  (let ((manifest-form (read-from-string manifest)))
    (%manifest->object (first manifest-form) (rest manifest-form) uri manifest)))

(defmethod %manifest->object ((kind (eql 'unit0)) manifest-data uri manifest)
  (with-transaction ()
    (let ((description (car manifest-data)))
      (let@ rec ((spec (cadr manifest-data))
		 (specs (cddr manifest-data))
		 (glyphs))
	(if spec
	    (rec (first specs)
		 (rest specs)
		 (cons (make-glyph
			:img `(:uri ,(puri:merge-uris (car spec) uri))
			:char (cadr spec)
			:pos (cddr spec))
		       glyphs))
	    (make-unit :name (first description) :glyphs (reverse glyphs) :uri uri
		       :manifest manifest
		       :meta (alist->hash (second description))))))))

(defmethod %manifest->object ((kind (eql 'unit1)) manifest-data uri manifest)
  (with-transaction ()
    (let ((description (car manifest-data)))
      (let@ rec ((spec (cadr manifest-data))
		 (specs (cddr manifest-data))
		 (glyphs))
	(if spec
	    (rec (first specs)
		 (rest specs)
		 (cons (make-glyph
			:img `(:uri ,(puri:merge-uris (first spec) uri))
			:char (second spec)
			:pos (parse-glyph-pos (third spec))
			:meta (alist->hash (fourth spec)))
		       glyphs))
	    (make-unit :name (first description) :glyphs (reverse glyphs) :uri uri
		       :manifest manifest
		       :meta (alist->hash (second description))))))))

(defun http-manifest->object (uri)
  (manifest->object uri (drakma:http-request uri)))

(defclass combined ()
  ((name :initarg :name :reader cmb-name)
   (units :initarg :units :reader cmb-units)
   (alphabet :initarg :ab :reader cmb-ab)
   (inactive-glyphs :initform nil :accessor cmb-inactive)))

(defclass unit-chart (combined) ())

(define-alternate-maker make-combined combined)
(define-alternate-maker make-unit-chart unit-chart)

(defun build-combined (name units &key (allow-unit t))
  (funcall (if (and allow-unit (= 1 (length units))) #'make-unit-chart #'make-combined)
		 :name name :units units
		 :ab (let ((combined (make-hash-table :test 'equal)))
		       (dolist (unit units combined)
			 (dolist (glyph (unit-glyphs unit))
			   (push glyph (gethash (glyph-char glyph) combined)))))))

(defun glyph-active-in-chart? (glyph chart)
  (not (find glyph (cmb-inactive chart))))

(defun cmb-find-glyph (cmb id)
  (let* ((units (cmb-units cmb))
	 (glyphs (unit-glyphs (first units))))
    (let@ rec ((glyph (first glyphs))
	       (glyphs (rest glyphs))
	       (units (rest units)))
      (if (string= id (glyph-id glyph))
	  glyph
	  (if glyphs
	      (rec (first glyphs) (rest glyphs) units)
	      (if units
		  (let ((glyphs (unit-glyphs (first units))))
		    (rec (first glyphs) (rest glyphs) (rest units)))
		  nil))))))

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


(ele:defpclass w/metadata/ele ()
  ((metadata :initarg :meta :accessor nabu-metadata))
  (:default-initargs :meta (ele:make-btree)))

(ele:defpclass unit/ele (unit w/metadata/ele)
  ((name :initarg :name :reader unit-name)
   (glyphs :initarg :glyphs :accessor unit-glyphs)
   (uri :initarg :uri :reader unit-uri)
   (manifest :initarg :manifest :reader unit-manifest))
  (:default-initargs :glyphs nil))

(ele:defpclass glyph/ele (glyph w/metadata/ele)
  ((char :initarg :char :reader glyph-char)
   (pos :initarg :pos :reader glyph-pos)
   (image :initarg :img :reader glyph-img)
   (id :initarg :id :reader glyph-id)
   (baseline :initarg :bl :accessor glyph-bl))
  (:default-initargs :id (make-oid) :bl 0))

(ele:defpclass combined/ele (combined)
  ((name :initarg :name :reader cmb-name)
   (units :initarg :units :reader cmb-units)
   (alphabet :initarg :ab :reader cmb-ab)
   (inactive-glyphs :initform nil :accessor cmb-inactive)))

(ele:defpclass unit-chart/ele (combined/ele unit-chart) ())
