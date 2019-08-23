(defpackage :mfp
  (:use :cl
        :puri
        :lquery
        :drakma
        :alexandria
        :ppcre
        :lparallel)
  (:export #:*path*
           #:*mfp-rss-url*
           #:*music-pathname-type*
           #:*max-parallel-downloads*
           #:download-to-file
           #:download
           #:entry-pathname
           #:rss
           #:fetch-from-rss
           #:download-from-rss
           #:update))
