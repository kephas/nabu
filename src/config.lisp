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
  (config :nothos.net/2014.05.nabu key))

(defconfig elephant
    '(:alternate-classes
      (unit unit/ele glyph glyph/ele
       combined combined/ele unit-chart unit-chart/ele)))

(defconfig dev
    `(:storage :elephant
      :server :hunchentoot
      :debug t
      :ele-store (:clsql (:sqlite3 ,(merge-pathnames #p"nabu.sqlite"
						     (asdf:component-pathname (asdf:find-system "nabu")))))
      ,@elephant))

(defconfig preprod
    `(:debug nil
      ,@dev))

(defconfig dev-mem
    `(:storage :memory
      ,@dev))
