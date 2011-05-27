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

(sb-ext:defglobal **daemon-children** nil)
(sb-ext:defglobal **daemon-lock** (sb-thread:make-mutex :name "Daemon Lock"))

(defun daemonize (&key input output error (umask +default-mask+) pidfile
                       exit-parent (exit-hook t) (disable-debugger t)
                       sigterm sigabrt sigint)
  "Forks off a daemonized child process.

If PIDFILE is provided, it is deleted before forking, and the child
writes its PID in there after forking. Returns NIL in the child.

For complete daemonization use EXIT-PARENT, which causes the parent process to
exit after forking, otherwise the PID of the child process is returned in
parent. When EXIT-PARENT is used, the parent exits without unwinding or
running SB-EXT:*EXIT-HOOKS*.

When EXIT-PARENT is false (the default), EXIT-HOOK can be used to record
child's exit. It will be called asynchronously with three arguments: the pid
of the child, the manner of child's termination (:EXIT or :SIGNAL), and the
child's exit code or signal number that caused termination. The default exit
handler T will merely reap the child process so it will not remain in
zombiefied state. Users wanting to reap the child manually (via eg.
SB-POSIX:WAITPID) must explicitly provide NIL as the EXIT-HOOK to prevent
automatic reaping.

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
  (default), and neither APPEND nor TRUNCATE is specified, an error is
  signaled if the file already exists. MODE is the mode to use for the
  file if it is created, defaulting to #o600.

  In all cases the files are opened before forking for easier error
  handling, and closed in parent afterwards.

UMASK specifies the umask for the child process. Default is #o022.

If DISABLE-DEBUGGER is true (default), SBCL's debugger is turned off in the
child process: any unhandled error terminates the process.

SIGTERM, SIGABRT, and SIGINT can be used to specify alternative handlers for
those signals. :IGNORE and :DEFAULT can be used to indicate that the signal
should be ignored or that the default OS handler should be used. Otherwise the
handler should be a function which will be called with a keyword indicating
the signal. If they are not provided, the currently installed handlers are
used.
"
  (declare
   (type (or null string pathname) pidfile)
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
        (err (open-fd :error error))
        (term (make-handler :sigterm sigterm))
        (abrt (make-handler :sigabrt sigabrt))
        (int (make-handler :sigint sigint)))
    ;; Most of the error-prone stuff is out of the way, time to fork. Disable
    ;; interrupts before forking, so that we can put exit-hooks into place
    ;; before the SIGCHLD can be delivered.
    (sb-sys:without-interrupts
      (let ((pid (sb-posix:fork)))
        (cond ((zerop pid)
               ;; Child
               (when term
                 (sb-sys:enable-interrupt sb-posix:sigterm term))
               (when abrt
                 (sb-sys:enable-interrupt sb-posix:sigabrt abrt))
               (when int
                 (sb-sys:enable-interrupt sb-posix:sigint int))               )
              (t
               ;; Parent
               (when exit-parent
                 (sb-ext:quit :unix-status 0 :recklessly-p t))
               (sb-posix:close in)
               (sb-posix:close out)
               (sb-posix:close err)
               (when exit-hook
                 (sb-thread:with-mutex (**daemon-lock**)
                   (%enable-sigchld-handler)
                   (let ((hook (unless (eq t exit-hook) exit-hook)))
                     (push (cons pid hook) **daemon-children**))))
               (return-from daemonize pid)))))
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


(defun make-handler (name spec)
  (cond ((member spec '(nil :default :ignore))
         spec)
        ((or (symbolp spec) (functionp spec))
         (lambda (signo context info)
           (declare (ignore signo context info))
           (funcall spec name)))
        (t
         (error "~S is not a valid value for ~S." spec name))))

;;; SBCL has a SIGCHLD handler for RUN-PROGRAM already -- and there could be
;;; others as well, so we daisy-chain.
(sb-ext:defglobal **previous-sigchld-handler** nil)

(defun handle-sigchld (signal info context)
  (let (exited)
    (sb-thread:with-mutex (**daemon-lock**)
      (setf **daemon-children**
            (delete-if (lambda (child)
                         (handler-case
                             (multiple-value-bind (pid status)
                                 ;; KLUDGE: doublecolon as SBCL's older than
                                 ;; 1.0.48.26 don't export WNOHANG. Ignore err
                                 (sb-posix:waitpid (car child) sb-posix::wnohang)
                               (when (plusp pid)
                                 (when (cdr child)
                                   (let (reason code)
                                     (cond ((sb-posix:wifexited status)
                                            (setf reason :exit
                                                  code (sb-posix:wexitstatus status)))
                                           ((sb-posix:wifsignaled status)
                                            (setf reason :signal
                                                  code (sb-posix:wtermsig status))))
                                     (push (list (cdr child) pid reason code) exited)))
                                 t))
                           (sb-posix:syscall-error ()
                             ;; Someone else already reaped it?
                             t)))
                       **daemon-children**)))
    (dolist (exit exited)
      (handler-case
          (sb-sys:with-interrupts (apply (first exit) (rest exit)))
        (serious-condition (c)
          (warn "Exit hook ~S for daemon child (pid ~S) had trouble:~%  ~A"
                (first exit) (second exit) c)))))
  ;; Next.
  (funcall **previous-sigchld-handler** signal info context))

(defun %enable-sigchld-handler ()
  (unless **previous-sigchld-handler**
    (setf **previous-sigchld-handler**
          (sb-sys:enable-interrupt sb-posix:sigchld
                                   ;; Trampoline so that HANDLE-SIGCHLD can be
                                   ;; redefined.
                                   (lambda (signal info context)
                                     (handle-sigchld signal info context))))))
