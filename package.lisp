(defpackage :mfp
  (:use :cl
        :puri
        :lquery
        :drakma
        :alexandria
        :ppcre
        :lparallel)
  (:export #:*path*
           #:*rss-url*
           #:*index-width*
           #:*music-pathname-type*
           #:*max-parallel-downloads*

           ;; download
           #:download-to-file
           #:download

           ;; entries
           #:path
           #:index
           #:title
           #:link

           ;; rss
           #:rss
           #:fetch-from-rss

           ;; main
           #:download-from-rss
           #:update))
