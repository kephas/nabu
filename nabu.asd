(defpackage :nothos.net/2014.05.nabu-system
  (:use :common-lisp :asdf))

(in-package :nothos.net/2014.05.nabu-system)

(defsystem "nabu"
  :description "Prototype palaeographic chart builder"
  :version "0.2"
  :author "Pierre Thierry <pierre@nothos.net>"
  :licence "AGPL"
  :depends-on ("scheme" "alexandria" "cl-fad" "cl-ppcre" "split-sequence" "metabang-bind" "cl-who" "drakma"
			"hu.dwim.stefil" "cl-match" "cl-base64" "uuid" "do-urlencode" "caveman2" "elephant"
			"cl-json")
  :components ((:module	"src"
	        :components ((:file "package")
			     (:file "config")
			     (:file "misc")
			     (:file "tag")
			     (:file "date")
			     (:module "model"
			      :components ((:file "model")
					   (:file "shell")
					   (:file "search")))
			     (:module "web"
			      :components ((:file "bootstrap")
					   (:file "web")
					   (:file "json")
					   (:file "shell")))
			     (:file "test")
			     (:file "repl"))))
  :serial t)
