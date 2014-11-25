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

(defmacro {row} (&body body)
  `(htm (:div :class "row" ,@body)))

(defmacro {col} (xs md &body body)
  (let ((class (format nil "col-xs-~a col-md-~a" xs md)))
    `(htm (:div :class ,class ,@body))))

(defmacro {submit} (btn-class &body body)
  (let ((class (format nil "btn btn-~a" btn-class)))
    `(htm (:button :type "submit" :class ,class ,@body))))

(defmacro {glyph} (name)
  (let ((class (format nil "glyphicon glyphicon-~a" name)))
    `(htm (:span :class ,class :aria-hidden "true"))))
