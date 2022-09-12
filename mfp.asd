(defsystem :mfp
  :depends-on (#:LQUERY
	       #:DRAKMA
	       #:CXML
	       #:CL-PPCRE
	       #:QURI
               #:PERCENT-ENCODING
	       #:LPARALLEL
	       #:UIOP
	       #:OSICAT)

  :components ((:file "package")
	       (:file "macros")
	       (:file "utils")
	       (:file "mfp")))
