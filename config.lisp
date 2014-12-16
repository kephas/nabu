 #| NABU - Prototype palaeographic chart builder
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

(setf (config-env-var) "NABU")

(defun config* (&optional key)
  (config (package-name *package*) key))


(defconfig dev
    `(:storage :elephant
      :ele-store (:clsql (:sqlite3 ,(merge-pathnames #p"nabu.sqlite"
						     (asdf:component-pathname (asdf:find-system "nabu")))))))

(defconfig no-db
    '(:storage :memory))
