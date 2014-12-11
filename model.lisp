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

(defun make-combined (name units)
  (make-instance 'combined :name name :units units
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
