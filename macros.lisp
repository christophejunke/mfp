(in-package :mfp)

(defmacro with-captured-bindings ((rebinding-name &rest symbols) &body body)
  (assert (every #'symbolp symbols))
  (with-gensyms (inner-body)
    (if symbols
        (loop for s in symbols
           for c = (gensym)
           collect (list c s) into capture
           collect (list s c) into rebind
           finally
             (return
               `(let ,capture
                  (macrolet ((,rebinding-name (&body ,inner-body)
                               `(let ,',rebind ,@,inner-body)))
                    ,@body))))
        `(macrolet ((,rebinding-name (&body ,inner-body)
                      `(progn ,@,inner-body)))
           ,@body))))
