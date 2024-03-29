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
    (cond
      ((boundp '*info-lock*)
       (with-lock-held (*info-lock*)
         (print printed)
         (finish-output)))
      (t (print printed)
         (finish-output)))))

;;;; ENTRY

(defvar *xml-node* nil)

(defclass entry ()
  ((index :initarg :index :accessor index)
   (title :initarg :title :accessor title)
   (link :initarg :link :accessor link)))

(defclass xml-entry (entry)
  ((xml-node :initform *xml-node* :accessor xml-node)))

(defun make-uri (string)
  (let ((uri (uri string)))
    (flet ((encode (s) (url-encode s :utf-8)))
      (let* ((parts (split "/" (uri-path uri)))
             (encoded (map-into parts #'encode parts))
             (path (format nil "~{~a~^/~}" encoded)))
        (merge-uris (make-instance 'uri :path path) uri)))))

(defun entry (index title link)
  (check-type index (integer 1))
  (check-type title string)
  (make-instance (if *xml-node* 'xml-entry 'entry)
                 :index index
                 :title title
                 :link (make-uri link)))

;;;; HTTP

(defun request-rss ()
  (let ((*text-content-types* '(("application" . "xml")))
        (*drakma-default-external-format* :utf-8))
    (http-request *rss-url* :accept "application/rss+xml" )))

;;;; RSS

(defun rss-document ()
  (let ((*text-content-types* (list* '("application" . "xml") *text-content-types*)))
    (http-request *rss-url* :external-format-in :utf-8)))

(defun rss ()
  ($ (initialize (request-rss))))

(defun fetch ()
  (declare (optimize (debug 3)))
  ($ (initialize (rss-document))
    (find "item")
    (combine ($1)
             ($1 (find "item guid") (text))
             ($1 (find "title") (text)))
    (map (lambda (list)
           (destructuring-bind (*xml-node* uri title) list
             (multiple-value-bind (index title) (parse-title title)
	       (entry index title uri)))))))

;;;; DOWNLOAD

(defgeneric encode-uri (uri)
  (:method ((str string))
    (encode-uri (uri str)))
  (:method ((uri uri) &aux (uri (copy-uri uri)))
    (prog1 uri
      (flet ((encode (s) (percent-encoding:encode s)))
	(destructuring-bind (keyword . path) (uri-parsed-path uri)
	  (setf (uri-parsed-path uri)
		(list keyword (mapcar #'encode path))))))))

(defun call-with-http-stream (uri function)
  (multiple-value-bind (body status headers reply stream closep message)
      (http-request uri :preserve-uri t :force-binary t :want-stream t)
    (declare (ignore body headers reply))
    (unwind-protect (funcall function status stream message)
      (when closep (close stream)))))

(defmacro with-http-stream ((status stream message) uri &body body)
  `(call-with-http-stream ,uri (lambda (,status ,stream ,message) ,@body)))

(defun download-to-file (uri target-file &key (if-exists :error))
  (let ((type '(unsigned-byte 8)))
    (with-open-file (target (ensure-directories-exist target-file)
                            :element-type type
                            :direction :output
                            :if-exists if-exists)
      (when target
        (prog1 (info (pathname target))
          (loop
            (file-position target 0)
            (with-simple-restart (retry "Retry downloading")
              (return
                (with-http-stream (status source message) uri
                  (if (= status 200)
                      (copy-stream-to-stream source
                                             target
                                             :element-type type)
                      (error 'request-failed
                             :status status
                             :message message
                             :uri uri)))))))))))

(defun make-periodic-retry (amount delay)
  (lambda (error)
    (when (plusp amount)
      (warn "Caught error ~s, retrying in ~s seconds (~d more attempts)"
            error delay amount)
      (finish-output *error-output*)
      (decf amount)
      (sleep delay)
      (invoke-restart (find-restart 'retry error)))))

;;;; MFP ENTRIES

(defmethod print-object ((e entry) stream)
  "Print an ENTRY readably."
  (prin1 `(entry ,(index e)
                 ,(title e)
                 ,(render-uri (link e) nil))
         stream))

(defun concat (strings)
  (flet ((length+ (n s) (+ n (length s))))
    (loop
       with result = (make-string (reduce #'length+ strings :initial-value 0))
       for string in strings
       for start = 0 then (+ start size)
       for size = (length string)
       do (replace result string :start1 start)
       finally (return result))))

(defun merge-single-letters (strings &aux stack result)
  "Remove empty strings and concat consecutive strings of size 1.

   (merge-single-letters '(\"ab\" \"c\" \"\" \"d\" \"e\" \"fg\"))
    => (\"ab\" \"cde\" \"fg\")
"
  (flet ((unstack ()
	   "Concat temporarily accumulated strings, push result"
           (when stack
	     (push (concat (nreverse (shiftf stack nil))) result))
	   result))
    (dolist (s strings (nreverse (unstack)))
      (case (length s)
	(0)
	(1 (push s stack))
	(t (setf result (list* s (unstack))))))))

(defun cleanup-title (title)
  (merge-single-letters
   (split '(:alternation :non-word-char-class #\_)
          (regex-replace-all '(:sequence " + Untitled") title ""))))

(defun filename (index title &optional (index-width *index-width*))
  "Default formatter for *NAMING-FUNCTION*.

   INDEX is a non-negative integer, TITLE is a string.

   Format filename as \"000-title\", where 000 is the zero-padded
   INDEX whose width is controlled by INDEX-WIDTH, and where \"title\"
   is a dash-separated string based on the list of strings obtained by
   calling CLEANUP-TITLE on TITLE."
  (format nil
          "~v,'0d-~(~{~a~^-~}~)"
          index-width
          index
          (cleanup-title title)))

(defun path (entry)
  (merge-pathnames
   (make-pathname :type *suffix*
                  :name (funcall (or *naming-function*
                                     #'filename)
                                 (index entry)
                                 (title entry)))
   *path*))

(defvar *entries* nil)

(defun entries (&key force)
  (when (or force (not *entries*))
    (setf *entries* (fetch)))
  *entries*)

;;;; DOWNLOAD ENTRY

(defun download (entry &optional force)
  (when entry
    (let ((pathname (path entry)))
      (prog1 pathname
        (download-to-file (link entry)
                          pathname
                          :if-exists (and force :supersede))))))

;;;; PARSING

(defvar *title-regex*
  (create-scanner `(:sequence :start-anchor
                              "Episode "
                              (:register (:regex "\\d+"))
                              ": "
                              (:register (:regex ".*"))
                              :end-anchor)))

(defun parse-title (string)
  (register-groups-bind ((#'parse-integer number) title) (*title-regex* string)
    (values number title)))

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
         (error (e)
	   (warn "caught error: ~a" e)))))))

;;;; MAIN FUNCTIONS

(defvar *parallel* t)

(defun %parallel-download ()
  (let ((*info-lock* (make-lock)))
    (pmap 'list
          (workerize #'download)
          :parts *max-parallel-downloads*
          (entries :force t))))

(defun download-from-rss ()
  (cond
    ((not *parallel*)
     (map 'list #'download (entries :force t)))
    (lparallel:*kernel*
     (%parallel-download))
    (t (with-temp-kernel ((or *max-parallel-downloads*
                              *default-worker-count*))
         (%parallel-download)))))

(defun wildcard ()
  (merge-pathnames (make-pathname :name :wild :type *suffix*) *path*))

(defun existing-files (&optional (sort #'string-greaterp) (key #'namestring))
  (let ((files (directory (wildcard))))
    (etypecase sort
      (null files)
      ((or function symbol)
       (sort files sort :key key)))))

(defun update ()
  (let ((existing (existing-files)))
    (set-difference (download-from-rss) existing :test #'equalp)))

;;;; MISC.

(defun %delete-some-files (&optional (count 5))
  "For tests (count = nil: deletes all)"
  (mapc #'delete-file (subseq (shuffle (existing-files)) 0 count)))
