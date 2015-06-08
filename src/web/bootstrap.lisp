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

(defmacro {row} (&body body)
  `(htm (:div :class "row" ,@body)))

(defmacro {col} (xs md &body body)
  (let ((class (format nil "col-xs-~a col-md-~a" xs md)))
    `(htm (:div :class ,class ,@body))))

(defmacro {checkbox} (id value &body body)
  `(htm (:div :class "checkbox"
	      (:input :type "checkbox" :name ,id :value ,value ,@body))))

(defmacro {submit} ((btn-class &key size (classes "")) &body body)
  (let ((class (format nil "btn btn-~a ~:[~;~:*btn-~a~] ~a" btn-class size classes)))
    `(htm (:button :type "submit" :class ,class ,@body))))

(defmacro {active} ((btn-class &key size (classes "")) href &body body)
  (let ((class (format nil "btn btn-~a ~:[~;~:*btn-~a~] active ~a" btn-class size classes)))
    (once-only (href)
      `(htm (:a :class ,class :href ,href ,@body)))))

(defmacro {button} ((btn-class &key size (classes "")) &body body)
  (let ((class (format nil "btn btn-~a ~:[~;~:*btn-~a~] ~a" btn-class size classes)))
    `(htm (:button :class ,class ,@body))))

(defmacro {alert} ((alert-class &optional dismiss?) &body body)
  (let ((class (format nil "alert alert-~a ~a" alert-class (if dismiss? "alert-dismissible" ""))))
    `(htm ((:div :class ,class :role "alert")
	   (when ,dismiss?
	     (htm ((:button :type "button" :class "close" :data-dismiss "alert")
		   (:span :aria-hidden "true" "×")
		   (:span :class "sr-only" "Close"))))
	   ,@body))))

(defmacro {collapse-btn} (target &optional (text "Toggle navigation"))
  `(htm (:button :class "navbar-toggle collapsed" :type "button"
		 :data-toggle "collapse" :data-target ,target
		 (:span :class "sr-only" ,text)
		 (:span :class "icon-bar")
		 (:span :class "icon-bar")
		 (:span :class "icon-bar"))))

(defmacro {glyph} (name)
  (let ((class (format nil "glyphicon glyphicon-~a" name)))
    `(htm (:span :class ,class :aria-hidden "true"))))
