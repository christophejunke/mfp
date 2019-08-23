(defpackage :mfp
  (:use :cl
        :puri
        :lquery
        :drakma
        :alexandria
        :ppcre
        :lparallel)
  (:import-from #:lparallel.kernel-util
		#:with-temp-kernel)
  (:export #:*path*
           #:*rss-url*
           #:*index-width*
           #:*music-pathname-type*
           #:*max-parallel-downloads*
           #:*naming-function*

           ;; download
           #:download-to-file
           #:download

           ;; entries
           #:path
           #:index
           #:title
           #:link
	   #:filename

           ;; rss
           #:rss
           #:fetch

           ;; main
           #:download-from-rss
           #:update))
