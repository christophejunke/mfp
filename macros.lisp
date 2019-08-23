(in-package :mfp)

(defmacro with-captured-bindings ((rebinding-name &rest symbols) &body body)
  "Lexically capture dynamic bindings and rebind them in another context.

For example, capture standard output at closure creation and rebind it
to captured value inside closure:

   (with-captured-bindings (rebind *standard-output*)
     (lambda () (rebind (print :test))))

This is useful in particular with threads when the dynamic environment
is local to each thread.
"
  (assert (every #'symbolp symbols))
  (with-gensyms (inner-body)
    (if symbols
        (loop
           for s in symbols
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
