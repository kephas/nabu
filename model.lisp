 #| NABU - Prototype palaeographic table builder
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

(defclass manuscript ()
  ((name :initarg :name :reader ms-name)
   (glyphs :initarg :glyphs :accessor ms-glyphs)
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
	    (make-instance 'manuscript :name (first description) :glyphs (reverse glyphs)
			   :meta (alist->hash (second description))))))))

(defclass table ()
  ((name :initarg :name :reader tbl-name)
   (alphabet :initarg :ab :reader tbl-ab)))

(defun make-table (name mss)
  (make-instance 'table :name name
		 :ab (let ((table (make-hash-table :test 'equal)))
		       (dolist (ms mss table)
			 (dolist (glyph (ms-glyphs ms))
			   (push (glyph-url glyph) (gethash (glyph-char glyph) table)))))))

(defun ab-union (tables)
  "Return the union of the alphabets of several tables, as a sorted list of chars"
  (let ((union (make-hash-table :test 'equal)))
    (dolist (table tables)
      (dolist (entry (hash-table-keys (tbl-ab table)))
	(setf (gethash entry union) t)))
    (sort (hash-table-keys union) #'string>)))
