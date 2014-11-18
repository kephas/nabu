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
   (years :initarg :years :accessor ms-years)
   (material :initarg :mat :accessor ms-mat)
   (ligature :initarg :lig :accessor ms-lig)
   (place :initarg :place :accessor ms-place))
  (:default-initargs :glyphs nil :years nil :mat nil :lig nil :place nil))

(defclass glyph ()
  ((char :initarg :char :reader glyph-char)
   (pos :initarg :pos :reader glyph-pos)
   (image :initarg :img :reader glyph-img)))

(defun split-image-name (name)
  (bind (((:values _ regs) (scan-to-strings "c\\.(.)\\.p\\.([ivxlcm.0-9]*)" name)))
    (bind ((#(char pos) regs))
      (list char (split-sequence #\. pos)))))

(defun read-images-dir (dirname &optional name)
  (let ((name (if name name (pathname-name (pathname-as-file dirname)))))
    (make-instance 'manuscript :name name
		   :glyphs (mapcan (lambda (pathname)
				     (handler-case
					 (bind (((char pos) (split-image-name (pathname-name pathname))))
					   (list (make-instance 'glyph :img `(:file ,pathname) :char char :pos pos)))
				       (error ()
					 nil)))
				   (list-directory dirname)))))

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
	    (apply #'make-instance 'manuscript :name (first description) :glyphs (reverse glyphs) (rest description)))))))

(defclass table ()
  ((name :initarg :name :reader tbl-name)
   (alphabet :initarg :ab :reader tbl-ab)))

(defun make-table (name mss)
  (make-instance 'table :name name
		 :ab (let ((table (make-hash-table :test 'equal)))
		       (dolist (ms mss table)
			 (dolist (glyph (ms-glyphs ms))
			   (push (glyph-url glyph) (gethash (glyph-char glyph) table)))))))
