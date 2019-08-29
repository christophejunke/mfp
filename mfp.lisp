(in-package :mfp)

;;;; CONFIGURATION

(defvar *rss-url*
  "https://musicforprogramming.net/rss.php")

(defvar *path*
  (merge-pathnames #P"music/mfp/" (user-homedir-pathname))
  "Directory for music storage.")

(defvar *index-width* 3
  "Number of digits when printing an index")

(defvar *suffix* "mp3"
  "File type for downloaded music files")

(defvar *default-worker-count* 4
  "Default worker count when creating temporary lparallel kernel.")

(defvar *max-parallel-downloads* nil
  "Max number of parallel downloads.

   NIL means the current worker count of lparallel's *kernel*,
   but if *kernel* is NIL, it means *default-worker-count*.")

(defvar *naming-function* nil
  "Custom naming function for downloaded file.

   Either NIL or a function called with INDEX and TITLE, that should
   return a string to be used as a pathname's name.")

;;;; ERRORS

(define-condition request-failed (error)
  ((status :initarg :status :accessor request-failed/status)
   (message :initarg :message :accessor request-failed/message)
   (uri :initarg :uri :accessor request-failed/uri))
  (:report
   (lambda (condition stream)
     (with-slots (status message uri) condition
       (format stream "request failed (~d): ~a - ~a" status message uri)))))

;;;; LOGGING

(defvar *info-lock*)

(defun info (printed)
  (prog1 printed
    (with-lock-held (*info-lock*)
      (print printed)
      (finish-output))))

;;;; DOWNLOAD

(defun call-with-http-stream (uri function)
  (multiple-value-bind (body status headers reply stream closep message)
      (http-request uri :force-binary t :want-stream t)
    (declare (ignore body headers reply))
    (unwind-protect (funcall function status stream message)
      (when closep (close stream)))))

(defmacro with-http-stream ((status stream message) uri &body body)
  `(call-with-http-stream ,uri (lambda (,status ,stream ,message) ,@body)))

(defun download-to-file (uri target-file &optional (if-exists :error))
  (let ((type '(unsigned-byte 8)))
    (with-open-file (target (ensure-directories-exist target-file)
                            :element-type type
                            :direction :output
                            :if-exists if-exists)
      (when target
        (prog1 (info (pathname target))
          (with-http-stream (status source message) uri
            (if (= status 200)
                (copy-stream-to-stream source target :element-type type)
                (error 'request-failed :status status :message message :uri uri))))))))

;;;; MFP ENTRIES

(defclass entry ()
  ((index :initarg :index :accessor index)
   (title :initarg :title :accessor title)
   (link :initarg :link :accessor link)))

(defun entry (index title link)
  (check-type index (integer 1))
  (check-type title string)
  (make-instance 'entry
		 :index index
		 :title title
		 :link (parse-uri link)))

(defmethod print-object ((e entry) stream)
  "Print an ENTRY readably."
  (prin1 `(entry ,(index e)
		 ,(title e)
		 ,(render-uri (link e) nil))
	 stream))

(defun filename (index title &optional (index-width *index-width*))
  (flet ((merge-single-letters (splitted-string &aux stack result)
	   (flet ((unstack (&aux (reversed (nreverse stack)))
		    (setf stack nil)
		    (if reversed
			(push (apply #'concatenate 'string reversed) result)
			result)))
	     (dolist (string splitted-string (nreverse (unstack)))
	       (if (= (length string) 1)
		   (push string stack)
		   (setf result (list* string (unstack))))))))
    (format nil
	    "~v,'0d-~(~{~a~^-~}~)"
	    index-width
	    index
	    (merge-single-letters
             (remove-if #'emptyp
                        (split
                         '(:alternation :non-word-char-class #\_)
                         (regex-replace-all
                          '(:sequence " + Untitled") title "")))))))

(defun path (entry)
  (merge-pathnames
   (make-pathname :type *suffix*
                  :name (funcall (or *naming-function*
                                     #'filename)
                                 (index entry)
                                 (title entry)))
   *path*))

;;;; DOWNLOAD ENTRY

(defun download (entry &optional force)
  (when entry
    (let ((file (path entry)))
      (prog1 file
	(download-to-file (link entry) file (if force :supersede nil))))))

;;;; PARSING

(defvar *title-regex*
  (create-scanner `(:sequence :start-anchor
			      "Episode "
			      (:register (:regex "\\d+"))
			      ": "
			      (:register (:regex ".*"))
			      :end-anchor)))

(defun parse-title (string)
  (register-groups-bind (number title) (*title-regex* string)
    (values (parse-integer number) title)))

;;;; RSS

(defun rss ()
  ($ (initialize (http-request *rss-url*))))

(defun fetch ()
  ($ (initialize (http-request *rss-url*))
     (find "item")
     (combine ($ (find "item guid") (text) #'first-elt)
	      ($ (find "title") (text) #'first-elt))
     (map (lambda (list)
	    (destructuring-bind (uri title) list
	      (multiple-value-bind (index title) (parse-title title)
		(entry index title uri)))))))

;;;; DYNAMIC BINDINGS ACROSS THREADS

(defun workerize (function)
  "Wrap FUNCTION to be used in a worker thread."
  (with-captured-bindings (rebind *path*
                                  *info-lock*
				  *index-width*
				  *naming-function*
				  *suffix*
				  *standard-output*
				  *error-output*)
    (lambda (&rest args)
      (rebind
       (handler-case (apply function args)
	 (error (e) (warn "caught error: ~a" e)))))))

;;;; MAIN FUNCTIONS

(defun %parallel-download ()
  (let ((*info-lock* (make-lock)))
    (pmap 'list
          (workerize #'download)
          :parts *max-parallel-downloads*
          (fetch))))

(defun download-from-rss ()
  (if lparallel:*kernel*
      (%parallel-download)
      (with-temp-kernel ((or *max-parallel-downloads*
                             *default-worker-count*))
        (%parallel-download))))

(defun existing-files ()
  (directory
   (merge-pathnames (make-pathname :name :wild :type *suffix*) *path*)))

(defun update ()
  (let ((existing (existing-files)))
    (set-difference (download-from-rss) existing :test #'equalp)))

;;;; MISC.

(defun %delete-some-files (&optional (count 5))
  "For tests (count = nil: deletes all)"
  (mapc #'delete-file (subseq (shuffle (existing-files)) 0 count)))
