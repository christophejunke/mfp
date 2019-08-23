(in-package :mfp)

;;;; CONFIGURATION

(defvar *rss-url*
  "https://musicforprogramming.net/rss.php")

(defvar *path*
  (merge-pathnames #P"music/for-programming/" (user-homedir-pathname))
  "Directory for music storage.")

(defvar *index-width* 3
  "Number of digits when printing an index")

(defvar *music-pathname-type* "mp3"
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

;;;; DYNAMIC BINDINGS ACROSS THREADS

(defun wrap-dynvars (function)
  "Capture bindings for variables that are used in worker threads."
  (with-captured-bindings (rebind *path*
                                  *index-width*
				  *naming-function*
                                  *music-pathname-type*)
    (lambda (&rest args)
      (rebind
       (apply function args)))))

;;;; DOWNLOAD

(defun download-to-file (uri target-file &optional (if-exists :error))
  (with-open-file (file-stream (ensure-directories-exist target-file)
                               :element-type '(unsigned-byte 8)
                               :direction :output
                               :if-exists if-exists)
    (when file-stream
      (prog1 (pathname file-stream)
        (multiple-value-bind (body status headers reply stream closep msg)
            (http-request uri :force-binary t :want-stream t)
          (declare (ignore body status headers reply msg))
          (unwind-protect (uiop:copy-stream-to-stream
                           stream
                           file-stream
                           :element-type '(unsigned-byte 8))
            (when closep
              (close stream))))))))

;;;; MFP ENTRIES

(defclass entry ()
  ((index :initarg :index :reader index)
   (title :initarg :title :reader title)
   (link :initarg :link :reader link)))

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
  (merge-pathnames (make-pathname
		    :name (funcall (or *naming-function* #'filename)
				   (index entry) (title entry))
		    :type *music-pathname-type*)
		   *path*))

;;;; DOWNLOAD ENTRY

(defun download (entry &optional force)
  (when entry
    (let ((file (path entry)))
      (prog1 file
        (download-to-file (link entry)
                          file
                          (if force :supersede nil))))))

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

;;;; MAIN FUNCTIONS

(defun download-from-rss ()
  (flet ((fetch-map ()
           (pmap 'list
                 (wrap-dynvars #'download)
                 :parts *max-parallel-downloads*
                 (fetch))))
    (if lparallel:*kernel*
        (fetch-map)
        (lparallel.kernel-util:with-temp-kernel
            ((or *max-parallel-downloads* *default-worker-count*))
          (fetch-map)))))

(defun update ()
  (let ((existing
         (directory
          (merge-pathnames (make-pathname :name :wild
                                          :type *music-pathname-type*)
                           *path*))))
    (set-difference (download-from-rss) existing :test #'equalp)))
