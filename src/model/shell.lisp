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


(defgeneric %get-shell-value (shell key))
(defgeneric %set-shell-value (shell key value))
(defgeneric %rm-shell-value (shell key))
(defgeneric %make-shell (shell))
(defgeneric %map-shell (shell function))


(define-condition not-shell (error)
  ((culprit :initarg :c)
   (shell :initarg :shell)
   (path :initarg :path))
  (:default-initargs :shell nil :path nil))

(defgeneric shell? (object))
(defmethod shell? (object) nil)

(defun %do-shell-path (shell path thunk)
  (let@ rec ((context shell)
	     (sub-path path))
    (if (shell? context)
	(if (= 1 (length sub-path))
	    (funcall thunk context (first sub-path))
	    (rec (%get-shell-value context (first sub-path)) (rest sub-path)))
	(error 'not-shell :c context :shell shell :path path))))

(defun shell-object (shell &rest path)
  (if path
      (%do-shell-path shell path
		      (lambda (shell key)
			(%get-shell-value shell key)))
      shell))

(defun (setf shell-object) (value shell &rest path)
  (%do-shell-path shell path
		  (lambda (shell key)
		    (%set-shell-value shell key value))))

(defun shell-mksub (shell &rest path)
  (%do-shell-path shell path
		  (lambda (shell key)
		    (bind (((:values sub found?) (shell-object shell key)))
		      (if found?
			  (if (shell? sub)
			      sub
			      (error 'not-shell :shell shell :path path :c sub))
			  (%set-shell-value shell key (%make-shell shell)))))))

(defun shell-remove (shell &rest path)
  "Remove an entry from a shell"
  (%do-shell-path shell path
		  (lambda (shell key)
		    (%rm-shell-value shell key))))

(defun shell-list (shell &rest path)
  "Return all key/object pairs contained in a shell container"
  (let ((sub-shell (apply #'shell-object shell path)))
    (if (shell? sub-shell)
	(handler-case
	    (let ((pairs))
	      (%map-shell sub-shell (lambda (k v)
				      (handler-case
					  (push (list k v) pairs)
					(error ()))))
	      (reverse pairs))
	  (error ()
	    (values)))
	(error 'not-shell :shell shell :path path :c sub-shell))))

(defun shell-empty (shell &rest path)
  (dolist (entry (apply #'shell-list shell path))
    (apply #'shell-remove shell (append path (list (car entry))))))

#| In-memory shell |#

(defmethod %get-shell-value ((shell hash-table) key)
  (gethash key shell))

(defmethod %set-shell-value ((shell hash-table) key value)
  (setf (gethash key shell) value))

(defmethod %rm-shell-value ((shell hash-table) key)
  (remhash key shell))

(defmethod %make-shell ((shell hash-table))
  (make-hash-table :test 'equal))

(defmethod %map-shell ((shell hash-table) function)
  (maphash function shell))

(defmethod shell? ((object hash-table)) t)

#| Persistent shell |#

(defmethod %get-shell-value ((shell ele:btree) key)
  (ele:get-value key shell))

(defmethod %set-shell-value ((shell ele:btree) key value)
  (setf (ele:get-value key shell) value))

(defmethod %rm-shell-value ((shell ele:btree) key)
  (ele:remove-kv key shell))

(defmethod %make-shell ((shell ele:btree))
  (ele:make-btree))

(defmethod %map-shell ((shell ele:btree) function)
  (ele:map-btree function shell))

(defmethod shell? ((object ele:btree)) t)
