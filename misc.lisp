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


(defun hash-keys (hash-table)
  (let ((keys))
    (maphash (lambda (key value)
	       (declare (ignore value))
	       (push key keys))
	     hash-table)
    keys))

(defun alist->hash (alist &key (test 'equal) (key #'cadr))
  (let ((ht (make-hash-table :test test :size (length alist))))
    (let@ rec ((pairs alist))
      (if pairs
	  (let ((pair (first pairs)))
	    (setf (gethash (car pair) ht) (funcall key pair))
	    (rec (rest pairs)))
	  ht))))

(defun read-all-from-string (string)
  (let@ rec ((values nil)
	     (start 0))
    (multiple-value-bind (value offset)
	(read-from-string string nil nil :start start)
      (if (= start offset)
	  (reverse values)
	  (rec (cons value values) offset)))))

(defun string-copy (string)
  (map 'string #'identity string))

(defun commatize (list &optional (separator ",") (end ""))
  "Prepare LIST to be pretty-printed through ~{~a~a~}"
  (let@ rec ((list list)
	     (result nil))
    (if list
	(rec (rest list)
	     (cons (string-copy separator) (cons (first list) result)))
	(reverse (cons end (rest result))))))

(defun make-oid ()
  (base64:usb8-array-to-base64-string (uuid:uuid-to-byte-array (uuid:make-v4-uuid))))
