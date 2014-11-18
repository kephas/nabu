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

#| A year-spec is either a natural integer for a specific year or a
   list of two natural integers for a range of years (including its
   limits) |#

(defun within? (number range)
  (and (>= number (first range)) (<= number (second range))))

(defun range-overlap? (range1 range2)
  (or (and (>= (second range1) (first range2))
	   (<= (first range1) (second range2)))
      (and (>= (second range2) (first range1))
	   (<= (first range2) (second range1)))))

(defun %make-date-checker (my-year-specs)
  (lambda (&rest their-year-specs)
    (let@ rec-y ((year-specs their-year-specs))
      (when year-specs
	(let ((year-spec (first year-specs)))
	  (let@ rec-c ((candidates my-year-specs))
	    (if candidates
		(let ((candidate (first candidates))
		      (next (rest candidates)))
		  (if (integerp year-spec)
		      (if (integerp candidate)
			  (if (eql year-spec candidate) t (rec-c next))
			  (if (within? year-spec candidate) t (rec-c next)))
		      (if (integerp candidate)
			  (if (within? candidate year-spec) t (rec-c next))
			  (if (range-overlap? year-spec candidate) t (rec-c next)))))
		(rec-y (rest year-specs)))))))))

(defmacro make-dates (&body dates)
  `(%make-date-checker ',dates))
