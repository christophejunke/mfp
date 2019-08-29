(defsystem :mfp
  :depends-on (:lquery :drakma :cxml :cl-ppcre :puri :lparallel :uiop)
  :components ((:file "package")
               (:file "macros")
               (:file "mfp")))
