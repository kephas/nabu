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
   (glyphs :initarg :glyphs :accessor ms-glyphs))
  (:default-initargs :glyphs nil))

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
		   :glyphs (mapcar (lambda (pathname)
				     (bind (((char pos) (split-image-name (pathname-name pathname))))
				       (make-instance 'glyph :img pathname :char char :pos pos)))
				   (list-directory dirname)))))
