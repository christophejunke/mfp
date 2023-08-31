(defsystem :mfp
  :depends-on (#:LQUERY
	       #:DRAKMA
	       #:CXML
	       #:CL-PPCRE
               #:PERCENT-ENCODING
               #:PURI
	       #:LPARALLEL
	       #:UIOP
	       #:OSICAT
               #:BRICABRAC/SHELL)

  :components ((:file "package")
	       (:file "macros")
	       (:file "utils")
	       (:file "mfp")
               (:file "player")))
