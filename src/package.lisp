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

(defpackage :nothos.net/2014.05.nabu
  (:use :common-lisp :alexandria :scheme :cl-ppcre :split-sequence :metabang-bind :who
	:cl-match)
  (:import-from :cl-fad #:pathname-as-file #:list-directory)
  (:import-from :do-urlencode #:urlencode #:urldecode)
  (:import-from :caveman2 #:defroute #:*request* #:*response*)
  (:import-from #:clack.request #:query-parameter #:body-parameter)
  (:import-from #:clack.response #:redirect)
  (:import-from :json #:*json-output* #:encode-json
		#:encode-json-to-string #:encode-json-plist-to-string
		#:with-array #:as-array-member
		#:with-object #:encode-object-member #:as-object-member)
  (:import-from :st-json #:getjso #:from-json-bool)
  (:nicknames :nabu))
