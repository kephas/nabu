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


(defmacro {setf-angular} (var value &optional (string? t))
  "Create an element that will set a variable in AngularJS"
  `(htm
    (:span :style "display:none"
	   :ng-init ,(if string?
			 `(format nil "~a=\"~a\"" ,var ,value)
			 `(format nil "~a=~a" ,var ,value)))))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *js-scripts* '("jquery" "bootstrap" "angular" "angular-animate" "angular-aria"
			       "angular-sanitize" "angular-route" "angular-resource" "angular-cookie"
			       "alerts" "test" "nabu")))

(eval-when (:compile-toplevel :load-toplevel)
  (defun js-scripts ()
    (let@ rec ((scripts (reverse *js-scripts*))
	       (markup))
      (if scripts
	  (rec (rest scripts)
	       (cons (list :script :src (format nil "/static/js/~a.js" (first scripts))) markup))
	  markup))))

(defvar *nav-links* '(("/units" "Units")
		      ("/charts" "Charts")
		      ("/shell" "Shell")))

(defun nav-links (stream)
  (dolist (link *nav-links*)
    (with-html-output (out stream)
      (:li (:a :href (first link) (str (second link)))))))

(defmacro nabu-page ((title) &body body)
  `(with-html-output-to-string (out nil :indent t)
     (:html :ng-app "nabuApp"
	    (:head
	     (:title (fmt "NABU - ~a" ,title))
	     (:meta :name "viewport" :content "width=device-width")
	     (:link :href "/static/css/bootstrap.min.css" :rel "stylesheet")
	     (:link :href "/static/css/local.css" :rel "stylesheet"))
	    (:body
	     ((:div :class "navbar navbar-inverse navbar-fixed-top" :role "navigation")

	      ((:div :class "container")
	       ((:div :class "navbar-header")
		({collapse-btn} ".nabu-navbar-collapse")
		((:a :class "navbar-brand" :href "#") "NABU"))
	       ((:div :class "collapse navbar-collapse nabu-navbar-collapse")
		((:ul :class "nav navbar-nav")
		 (nav-links out)
		 (:li
		  ((:div :class "user-connect" :ng-controller "userCtrl")
		   ({button} ("primary")
		     :ng-hide "remembered" :ng-click "remember()" "Remember me!")
		   ({button} ("primary")
		     :ng-show "remembered" :ng-click "forget()" "Forget me!")))))))
	     ((:div :class "container")
	      (:h1 (str ,title))
	      ,@body
	      ,@(js-scripts))))))
