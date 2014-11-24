 #| NABU - Prototype palaeographic table builder
    Copyright (C) 2014 Pierre Thierry <pierre@nothos.net>

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

#| Type tagger |#

(defmacro deftag (name &rest args)
  (with-gensyms (object-var)
  `(progn
     (defclass ,name nil
       ,(mapcar (lambda (arg) (list arg)) args))
     (defun ,name ,args
       (let ((,object-var (make-instance ',name)))
	 ,@(mapcar (lambda (arg)
		     `(setf (slot-value ,object-var ',arg) ,arg))
		   args)
	 ,object-var)))))

(defpattern tag (name &rest slots)
  `(and (type ,name) (slots ,@slots)))
