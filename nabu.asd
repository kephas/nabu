(defpackage :nothos.net/2014.05.nabu-system
  (:use :common-lisp :asdf))

(in-package :nothos.net/2014.05.nabu-system)

(defsystem "nabu"
  :description "Prototype palaeographic table builder"
  :version "0.1"
  :author "Pierre Thierry <pierre@nothos.net>"
  :licence "AGPL"
  :depends-on ("scheme" "alexandria" "cl-fad" "cl-ppcre" "split-sequence" "metabang-bind" "cl-who" "hunchentoot" "drakma"
			"hu.dwim.stefil")
  :components ((:file "package")
	       (:file "model")
	       (:file "date")
	       (:file "test")
	       (:file "web"))
  :serial t)
