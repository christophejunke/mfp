(in-package :mfp)

(macrolet
    ((def (n f d)
       (let ((doc (let ((*package* (find-package :keyword)))
		    (format () "~a.~%~%  (~s (~s ~a))" d f 'stat 'file))))
	 `(progn
	    (defun ,n (file) ,doc (,f (stat file)))
	    (defvar ,n (function ,n) ,doc)))))

  (def /dev      stat-dev      "ID of device containing file"    )
  (def /ino      stat-ino      "Inode number"                    )
  (def /mode     stat-mode     "File type and mode"              )
  (def /nlink    stat-nlink    "Number of hard links"            )
  (def /uid      stat-uid      "User ID of owner"                )
  (def /gid      stat-gid      "Group ID of owner"               )
  (def /rdev     stat-rdev     "Device ID (if special file)"     )
  (def /size     stat-size     "Total size, in bytes"            )
  (def /blksize  stat-blksize  "Block size for filesystem I/O"   )
  (def /blocks   stat-blocks   "Number of 512B blocks allocated" )
  (def /atime    stat-atime    "Time of last access"             )
  (def /mtime    stat-mtime    "Time of last modification"       )
  (def /ctime    stat-ctime    "Time of last status change"      ))
