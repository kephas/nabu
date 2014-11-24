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


(defparameter *fake-mss*
  (list (make-instance 'manuscript :name "Q1" :meta (alist->hash '(("place" "Qumran") ("material" "papyrus") ("years" 20))))
	(make-instance 'manuscript :name "Q2" :meta (alist->hash '(("place" "Qumran") ("material" "velum") ("years" 200))))
	(make-instance 'manuscript :name "Q3" :meta (alist->hash '(("place" "Qumran") ("material" "velum") ("years" (50 150)))))
	(make-instance 'manuscript :name "B1" :meta (alist->hash '(("place" "Bethleem") ("material" "velum") ("years" 30))))
	(make-instance 'manuscript :name "B2" :meta (alist->hash '(("place" "Bethleem") ("material" "papyrus") ("years" (40 80)))))))

(defparameter *query1* `(and ("material" ,(make-string= "velum")) ("years" ,(make-dates (20 60)))))
(defparameter *sexpr1* '(("material" "velum") ("years" (20 60))))
(defparameter *expr1* "material:velum  years:20-60")

(defparameter *query2* `(and ("years" ,(make-dates (0 40)))))
(defparameter *sexpr2* '(("years" (0 40))))
(defparameter *expr2* "years:0-40")
