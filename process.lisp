(in-package :fern)

(defclass process ()
  ((name :accessor process-name :initarg :name :initform nil
    :documentation "Gives our process a name.")
   (active :accessor process-active :initarg :active :initform nil
    :documentation "Whether or not this process is alive.")
   (ready :accessor process-ready :initform nil
    :documentation "Whether or not a process has been set up.")
   (mailbox :accessor process-mailbox :initform (make-queue)
    :documentation "The place we check for messages.")
   (message-callback :accessor process-message-callback :initform nil
    :documentation "Called for each received message.")
   (main :accessor process-main :initarg :main
    :documentation "This process' main() function.")
   (first-run :accessor process-first-run :initarg :first-run :initform nil
    :documentation "Whether or not this process' main() function is being run
                    for the first time.")
   (lock :accessor process-lock :initform (bt:make-lock)
    :documentation "Ensures consistency when reading/writing process metadata.")))

(defun make-process (main-function &key name active)
  "Create a new process with the given main function."
  (make-instance 'process
                 :name name
                 :active active
                 :first-run t
                 :main main-function))

(defmacro with-messages (process (bind-msg) &body body)
  "Called from within a process to set up message handling (otherwise process
   will just quit after it runs its main function)."
  (let ((fn-var (gensym "fn-var"))
        (process-var (gensym "process")))
    `(progn
       (let* ((,process-var ,process)
              (,fn-var (lambda (,bind-msg)
                         ,@body)))
         (setf (process-message-callback ,process-var) ,fn-var)))))

(defun message-poller (process)
  "Grab all messages from the queue and process them."
  (let ((mailbox (process-mailbox process))
        (msg-callback (process-message-callback process)))
    (if (and mailbox msg-callback)
        (handler-case
          (loop while (not (jpl-queues:empty? mailbox)) do
            (let ((message (jpl-queues:dequeue mailbox)))
              (funcall msg-callback message)))
          (t (e) (log:error "process: message poller: ~a" e)))
        (terminate process))))

(defun run-process (process)
  "Runs a process."
  ;; only run active processes
  (unless (and (process-main process)
               (process-active process))
    (terminate process)
    (return-from run-process nil))
  ;; run the main() function
  (handler-case
    (funcall (process-main process) process)
    (t (e) (log:error "process: main: ~a" e)))
  ;; mark the process as ready
  (bt:with-lock-held ((process-lock process))
    (setf (process-ready process) t))
  ;; if after running main() we don't have message handling set up, assuming it
  ;; was a run-once process and deactivate it, otherwise replace the main()
  ;; function with a message poller
  (if (process-message-callback process)
      (when (process-first-run process)
        (setf (process-main process) 'message-poller))
      (terminate process)))

(defun terminate (process)
  "Quit a process (should be called from within the process itself or at least
   its owning thread)."
  (with-slots (active main mailbox message-callback) process
    (setf active nil
          main nil
          mailbox nil
          message-callback nil)))

(defun copy-message (message)
  "Deep copy a message to remove ALL potential shared state."
  ;; TODO
  message)

(defun enqueue-message (process message)
  "Send a message to a process. MUST be a copyable structure: strings, numbers,
   symbols, or lists thereof."
  (let ((mailbox (process-mailbox process)))
    (when mailbox
      (jpl-queues:enqueue (copy-message message) mailbox))))

(defun process-ready-p (process)
  "Whether or not a process has been fully set up (and is ready to receive
   messages)."
  (bt:with-lock-held ((process-lock process))
    (process-ready process)))
  

