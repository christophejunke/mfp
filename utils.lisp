(in-package :mfp)

(macrolet ((def (n f d)
	     (let ((doc (format () "Compose ~a over OSICAT-POSIX:STAT ~
                                   for FILE (i.e. ~(~a~))" f d)))
	       `(progn
		  (defun ,n (file) ,doc (,f (osicat-posix:stat file)))
		  (defvar ,n (function ,n) ,doc)))))

  (def /dev      osicat-posix:stat-dev     "ID of device containing file"   )
  (def /ino      osicat-posix:stat-ino     "Inode number"                   )
  (def /mode     osicat-posix:stat-mode    "File type and mode"             )
  (def /nlink    osicat-posix:stat-nlink   "Number of hard links"           )
  (def /uid      osicat-posix:stat-uid     "User ID of owner"               )
  (def /gid      osicat-posix:stat-gid     "Group ID of owner"              )
  (def /rdev     osicat-posix:stat-rdev    "Device ID (if special file)"    )
  (def /size     osicat-posix:stat-size    "Total size, in bytes"           )
  (def /blksize  osicat-posix:stat-blksize "Block size for filesystem I/O"  )
  (def /blocks   osicat-posix:stat-blocks  "Number of 512B blocks allocated")
  (def /atime    osicat-posix:stat-atime   "Time of last access"            )
  (def /mtime    osicat-posix:stat-mtime   "Time of last modification"      )
  (def /ctime    osicat-posix:stat-ctime   "Time of last status change"     ))
