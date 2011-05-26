;;;; By Nikodemus Siivola <nikodemus@sb-studio.net>, 2011.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :sb-daemon)

(defconstant +default-mask+ #o022)
(defconstant +default-mode+ #o600)

(defun daemonize (&key input output error (umask +default-mask+)
                       pidfile exit-parent disable-debugger)
  "Forks off a daemonized child process. If PIDFILE is provided, it is
deleted before forking, and the child writes its PID in there after
forking. If EXIT-PARENT is true, the parent process exits after
forking, otherwise DAEMONIZE returns the PID of the child process in
parent, and NIL in child.

The child changes its current working directory to /, but
*DEFAULT-PATHNAME-DEFAULTS* is unaffected.

INPUT, OUTPUT, and ERROR designate files to which the child process
should connect its stdin, stdout, and stderr file descriptors.
Possible values are:

  NIL (the default), designating /dev/null.

  T, designating the corresponding file descriptor in parent.

  A pathname designator. The file is created if necessary,
  and opened for appending it if already exists.

  A list of the form:

   (pathname &key mkfifo create append truncate mode)

  where PATHNAME must be a pathname designator. If MKFIFO is true,
  a FIFO is created with the specied name and then opened. Otherwise
  a normal file is opened in the specified manner. If CREATE is true
  (default), and neither APPEND nor TRUNCATE is specified, the
  an error is signaled if the file already exists. MODE is the mode
  to use for the file if it is created, defaulting to #o600.

  In all cases the files are opened before forking for easier error
  handling, and closed in parent afterwards.

UMASK specifies the umask for the child process. Default is #o022.

If DISABLE-DEBUGGER is true, the debugger is turned off in the
child process: any unhandled error terminates the process.
"
  (declare
   (type (or null string pathname) directory pidfile)
   (type (unsigned-byte 32) umask))
  ;; Sanity checking.
  (flet ((check-fd (fd name)
             (let ((stream (symbol-value name)))
               (unless (and (typep stream 'sb-sys:fd-stream)
                            (= fd (sb-sys:fd-stream-fd stream)))
                 (error "~S should be an FD-STREAM on ~S"
                        name fd)))))
      (check-fd 0 'sb-sys:*stdin*)
      (check-fd 1 'sb-sys:*stdout*)
      (check-fd 2 'sb-sys:*stderr*))
  (when pidfile
    (ignore-errors (delete-file pidfile)))
  (let ((in (open-fd :input input))
        (out (open-fd :output output))
        (err (open-fd :error error)))
    ;; Most of the error-prone stuff is out of the way,
    ;; time to fork.
    (let ((pid (sb-posix:fork)))
      (unless (zerop pid)
        (when exit-parent
          (sb-ext:quit :unix-status 0 :recklessly-p t))
        (sb-posix:close in)
        (sb-posix:close out)
        (sb-posix:close err)
        (return-from daemonize pid)))
    (when disable-debugger
      (sb-ext:disable-debugger))
    ;; The only safe place to be.
    (sb-posix:chdir "/")
    ;; Throw away the old *TTY* stream.
    (let ((tty sb-sys:*tty*))
      (when (typep tty 'sb-sys:fd-stream)
        (close tty)
        (setf sb-sys:*tty* (make-two-way-stream sb-sys:*stdin*
                                                sb-sys:*stdout*))))
    ;; Rest of the setup.
    (sb-posix:setsid)
    (sb-posix:umask umask)
    (sb-posix:dup2 in 0)
    (sb-posix:dup2 out 1)
    (sb-posix:dup2 err 2)
    (when pidfile
      (with-open-file (f pidfile :direction :output
                                 :if-exists :supersede)
        (format f "~A~%" (sb-posix:getpid)))))  
  nil)

(defun open-fd (use spec)
  (if (eq spec t)
      (ecase use
        (:input (sb-posix:dup 0))
        (:output (sb-posix:dup 1))
        (:error (sb-posix:dup 2)))
      (multiple-value-bind (name flags mode)
          (etypecase spec
            (null
             (values "/dev/null" 0 0))
            ((or string pathname)
             (values (pathname spec)
                     (logior sb-posix:o-append sb-posix:o-creat)
                     +default-mode+))
            (cons
             (destructuring-bind
                 (name
                  &key mkfifo (create t) append truncate
                       (mode +default-mode+))
                 spec
               (let ((name (pathname name)))
                 (when mkfifo
                   (sb-posix:mkfifo name mode))
                 (values name
                         (logior
                          (if (and create (not mkfifo))
                              sb-posix:o-creat
                              0)
                          (if (or append truncate)
                              (if append
                                  sb-posix:o-append
                                  sb-posix:o-trunc)
                              (if mkfifo
                                  0
                                  sb-posix:o-excl)))
                         mode)))))
        (loop
          (with-simple-restart (retry "Retry opening ~A for ~S (flags=~x, mode=~x)"
                                      name use flags mode)
            (return-from open-fd
              (sb-posix:open name (logior sb-posix:o-rdwr flags) mode)))))))
