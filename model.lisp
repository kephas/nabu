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

(defun make-combined (name units)
  (make-instance (if (= 1 (length units)) 'unit-chart 'combined)
		 :name name :units units
		 :ab (let ((combined (make-hash-table :test 'equal)))
		       (dolist (unit units combined)
			 (dolist (glyph (unit-glyphs unit))
			   (push (glyph-url glyph) (gethash (glyph-char glyph) combined)))))))

(defun ab-union (combineds)
  "Return the union of the alphabets of several combineds, as a sorted list of chars"
  (let ((union (make-hash-table :test 'equal)))
    (dolist (combined combineds)
      (dolist (entry (hash-table-keys (cmb-ab combined)))
	(setf (gethash entry union) t)))
    (sort (hash-table-keys union) #'string>)))


#| Shell

   A shell is a container for objects. It is meant to be the larger
   object from which a user accesses the system |#

(defclass shell ()
  ((containers :initarg :cnt))
  (:default-initargs :cnt (make-hash-table :test 'equal)))

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
       (type-error ()
	 (error (make-condition 'shell-path-not-traversable :shell ,shell :path ,path))))))

(defun shell-object (shell &rest path)
  (with-path-error shell path
    (let@ rec ((object (slot-value shell 'containers))
	       (path path))
      (if (null path)
	  object
	  (rec (gethash (first path) object) (rest path))))))

(defun (setf shell-object) (value shell &rest path)
  (with-path-error shell path
    (let@ rec ((object (slot-value shell 'containers))
	       (path path))
      (if (= 1 (length path))
	  (setf (gethash (first path) object) value)
	  (rec (gethash (first path) object) (rest path))))))

(defun shell-ensure (shell &rest path)
  "Ensure that a shell path designates a container"
  (with-path-error shell path
    (let@ rec ((object (slot-value shell 'containers))
	       (path path))
      (when path
	(bind (((:values _ found?) (gethash (first path) object)))
	  (unless found?
	    (setf (gethash (first path) object) (make-hash-table :test 'equal)))
	  (rec (gethash (first path) object) (rest path)))))))
