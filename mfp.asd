(defsystem :mfp
  :depends-on (#:LQUERY
	       #:DRAKMA
	       #:CXML
	       #:CL-PPCRE
	       #:QURI
               #:PERCENT-ENCODING
	       #:LPARALLEL
	       #:UIOP
	       #:OSICAT

               #:BRICABRAC) ;; only the shell part

  :components ((:file "package")
	       (:file "macros")
	       (:file "utils")
	       (:file "mfp")
               (:file "player")))
