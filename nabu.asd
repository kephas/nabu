(defpackage :nothos.net/2014.05.nabu-system
  (:use :common-lisp :asdf))

(in-package :nothos.net/2014.05.nabu-system)

(defsystem "nabu"
  :description "Prototype palaeographic table builder"
  :version "0.1"
  :author "Pierre Thierry <pierre@nothos.net>"
  :licence "AGPL"
  :depends-on ("scheme" "alexandria" "cl-fad" "cl-ppcre" "split-sequence" "metabang-bind" "cl-who" "hunchentoot" "drakma"
			"hu.dwim.stefil" "cl-match")
  :components ((:file "package")
	       (:file "misc")
	       (:file "tag")
	       (:file "model")
	       (:file "date")
	       (:file "search")
	       (:file "test")
	       (:file "bootstrap")
	       (:file "web"))
  :serial t)
