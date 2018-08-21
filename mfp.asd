(defsystem #:mfp-dependecies
  :depends-on (:lquery :drakma :cxml :cl-ppcre :puri))

(defsystem :mfp
  :depends-on (#:mfp-dependecies)
  :components ((:file "mfp")))

(defsystem :mfp+lparallel
  :depends-on (#:mfp-dependecies :lparallel)
  :components ((:file "mfp")))
