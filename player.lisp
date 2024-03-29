(in-package :mfp)

(defun mpv-random-file (&key (force-reload nil))
  (let ((entry (random-elt (mfp:entries :force force-reload))))
    (values (download entry) entry)))

(defvar *mpv-process* nil)

(defun play (&key file (volume 50) (jackp nil) entry)
  (cond
    (entry
     (setf file (download entry)))
    ((not file)
     (multiple-value-setq (file entry) (mpv-random-file))))
  (with-terminal-options ((:directory :tmp)
                          (:title (format nil "[~d] ~a" (index entry) (title entry))
                           :when entry)
                          (:title (format nil "~a" (pathname-name file))
                           :unless entry)
                          (:term :shell
                            :unless (osicat-posix:getenv "DISPLAY")))
    (with-tmux-session ("music" "main" "0")
      (terminal "mpv"
                (namestring file)
                (format nil "--volume=~a" volume)
                (and jackp "--ao=jack")))))

(defun mpv (&key (file (mpv-random-file)) (volume 50) (global t gp) (wait nil wp))
  (flet ((play () (play :file file :volume volume)))
    (when (and global wait)
      (cond
        ((and gp wp) (error "Bad combination of GLOBAL and WAIT"))
        (gp (setf wait nil))
        (wp (setf global nil))))
    (cond
      (global
       (when *mpv-process*
         (ignore-errors (uiop:terminate-process *mpv-process*))
         (setf *mpv-process* nil))
       (values (setf *mpv-process* (play))
               file))
      (t
       (with-terminal-options ((:wait wait) (:hold wait))
         (play))))))

(defclass %mpv-loop ()
  ((stop :initarg :stop :reader %ml-stop)
   (volume :initarg :volume :reader %ml-volume)
   (terminate :initarg :terminate :reader %ml-terminate)
   (thread :initarg :thread)
   (current-file :initarg :current-file :reader %ml-current-file)))

(defun mpv-loop-current-file (loop)
  (funcall (%ml-current-file loop)))
(macrolet ((def (name internal)
             `(defun ,name (loop) (prog1 loop (funcall (,internal loop))))))
  (def mpv-loop-stop %ml-stop)
  (def mpv-loop-next %ml-terminate))
(defun mpv-loop-volume (loop)
  (funcall (car (%ml-volume loop))))
(defun (setf mpv-loop-volume) (new loop)
  (funcall (cdr (%ml-volume loop)) new))
(defun mpv-loop (&key init-stack (volume 50))
  (let ((running t)
        (current-file)
        (process)
        (init-stack (ensure-list init-stack)))
    (labels ((current-file () current-file)
             (get-volume () volume)
             (set-volume (v) (setf volume v))
             (next ()
               (setf current-file
                     (if init-stack
                         (pop init-stack)
                         (mpv-random-file))))
             (terminate ()
               (when process
                 (uiop:terminate-process process)))
             (play ()
               (loop
                  while running
                  do (setf process (mfp:mpv :file (next)
                                            :volume volume
                                            :wait nil
                                            :global nil))
                  do (uiop:wait-process process)))
             (stop ()
               (setf running nil)
               (terminate)))
      (make-instance '%mpv-loop
                     :stop #'stop
                     :volume (cons #'get-volume #'set-volume)
                     :terminate #'terminate
                     :current-file #'current-file
                     :thread (bt:make-thread #'play)))))



