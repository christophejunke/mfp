(defpackage :mfp
  (:use :cl
	:puri
	:bordeaux-threads
	:lquery
	:drakma
	:alexandria
	:ppcre
	:lparallel)
  (:import-from #:lparallel.kernel-util
		#:with-temp-kernel)
  (:import-from #:uiop
		#:copy-stream-to-stream)
  (:export #:*path*
	   #:*rss-url*
	   #:*index-width*
	   #:*music-pathname-type*
	   #:*max-parallel-downloads*
	   #:*naming-function*

	   ;; errors
	   #:request-failed
	   #:request-failed/message
	   #:request-failed/uri
	   #:request-failed/status

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

	   ;; filters
	   #:/dev
	   #:/gid
	   #:/ino
	   #:/uid
	   #:/mode
	   #:/rdev
	   #:/size
	   #:/atime
	   #:/ctime
	   #:/mtime
	   #:/nlink
	   #:/blocks
	   #:/blksize

	   ;; main
	   #:existing-files
	   #:download-from-rss
	   #:update))
