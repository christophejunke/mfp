(defpackage :mfp

  (:use :alexandria
	:bordeaux-threads
	:common-lisp
	:drakma
	:lparallel
	:lquery
	:ppcre
	:puri
        #:bricabrac.shell)

  (:import-from #:osicat-posix
		#:stat
		#:stat-dev
		#:stat-gid
		#:stat-ino
		#:stat-uid
		#:stat-mode
		#:stat-rdev
		#:stat-size
		#:stat-atime
		#:stat-ctime
		#:stat-mtime
		#:stat-nlink
		#:stat-blocks
		#:stat-blksize)

  (:import-from #:lparallel.kernel-util
		#:with-temp-kernel)
  
  (:import-from #:uiop
		#:copy-stream-to-stream)

  (:export 

   ;; vars
   #:*path*
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
   #:entry
   #:path
   #:index
   #:title
   #:link
   #:filename
   #:entries

   ;; exported utils
   #:concat
   #:cleanup-title
   #:download-to-file
   #:merge-single-letters

   ;; rss
   #:rss
   #:fetch

   ;; filters (slash prefix helps with completion)
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
   #:update

   ;; player
   #:mpv
   #:mpv-loop
   #:mpv-loop-stop
   #:mpv-loop-next
   #:mpv-loop-volume
   #:mpv-loop-current-file
   ))
