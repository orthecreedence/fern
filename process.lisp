(in-package :fern)

(define-condition bad-message () ())

(defclass process ()
  ((id :accessor process-id :initarg :id :initform nil
    :documentation "Gives our process an ID.")
   (active :accessor process-active :initarg :active :initform nil
    :documentation "Whether or not this process is alive.")
   (ready :accessor process-ready :initform nil
    :documentation "Whether or not a process has been set up.")
   (queued :accessor process-queued :initform nil
    :documentation "Whether or not a process has been queued to run.")
   (mailbox :accessor process-mailbox :initform (make-queue)
    :documentation "The place we check for messages.")
   (message-callback :accessor process-message-callback :initform nil
    :documentation "Called for each received message.")
   (main :accessor process-main :initarg :main
    :documentation "This process' main() function.")
   (first-run :accessor process-first-run :initarg :first-run :initform nil
    :documentation "Whether or not this process' main() function is being run
                    for the first time.")
   (locks :accessor process-locks :initform (list :active (bt:make-lock)
                                                  :ready (bt:make-lock)
                                                  :queued (bt:make-lock)
                                                  :mailbox (bt:make-lock)
                                                  :message-callback (bt:make-lock)
                                                  :main (bt:make-lock)
                                                  :first-run (bt:make-lock))
    :documentation "Lock list, ensures consistency when changing the process.")))

(defun make-process (main-function &key id active)
  "Create a new process with the given main function."
  (make-instance 'process
                 :id id
                 :active active
                 :first-run t
                 :main main-function))

(defmacro proclock ((process field) &body body)
  "Makes locking syntax easier."
  `(bt:with-lock-held ((getf (process-locks ,process) ,field))
     ,@body))

(defun mark-process-queued (process)
  "Mark a process as queued for execution. This keeps us from wasting cycles by
   queuing up a bunch of process activations (one per message) when we really
   only need it once since it will clear out all messages in one run."
  (proclock (process :queued) (setf (process-queued process) t)))

(defun message-poller (process)
  "Grab all messages from the queue and process them."
  (let ((mailbox (proclock (process :mailbox) (process-mailbox process)))
        (msg-callback (proclock (process :message-callback) (process-message-callback process))))
    (if (and mailbox msg-callback)
        (handler-case
          (progn
            ;; grab all our pending messages and process them
            (loop while (not (jpl-queues:empty? mailbox)) do
              (let ((message (jpl-queues:dequeue mailbox)))
                (apply msg-callback message)))
            ;; mark the process as no longer queued (can now be activate again)
            (proclock (process :queued) (setf (process-queued process) nil)))
          (t (e) (log:error "process: message poller: ~a" e)))
        (terminate process))))

(defun run-process (process)
  "Runs a process."
  (let ((active (proclock (process :active) (process-active process)))
        (main (proclock (process :main) (process-main process)))
        (first-run (proclock (process :first-run) (process-first-run process))))
    ;; only run active processes
    (unless (and main active)
      (terminate process)
      (return-from run-process nil))
    ;; run the main() function
    (handler-case
      (funcall main process)
      (t (e) (log:error "process: main: ~a" e)))
    ;; mark the process as ready
    (proclock (process :ready) (setf (process-ready process) t))
    ;; if after running main() we don't have message handling set up, assuming it
    ;; was a run-once process and deactivate it, otherwise replace the main()
    ;; function with a message poller
    (if (proclock (process :message-callback)
          (process-message-callback process))
        (when first-run
          (proclock (process :main) (setf (process-main process) 'message-poller))
          (proclock (process :first-run) (setf (process-first-run process) nil))
          ;; run the message poller now
          (run-process process))
        (terminate process))))

(defun copy-message (message)
  "Deep copy a message to remove ALL potential shared state. Only works with
   atoms, lists, sequences, and hash tables (basically anything you could
   serialize into JSON)."
  (cond ((numberp message)
         message)
        ((stringp message)
         (copy-seq message))
        ((arrayp message)
         (loop for x across message collect (copy-message x)))
        ((listp message)
         (cons-map 'copy-message message))
        ((hash-table-p message)
         (let ((hash (make-hash-table :test (hash-table-test message))))
           (loop for key being the hash-keys of message
                 for val being the hash-values of message do
             (setf (gethash (copy-message key) hash) (copy-message val)))
           hash))
        ((symbolp message)
         message)
        ((typep message 'process)
         (id message))
        (t
         (error 'bad-message))))

(defun enqueue-message (process message)
  "Send a message to a process. MUST be a copyable structure: strings, numbers,
   symbols, or lists thereof."
  (let ((mailbox (process-mailbox process)))
    (when mailbox
      (jpl-queues:enqueue (copy-message message) mailbox))))

(defun wipe-process (process)
  "Wipes out all a process' state. Used when terminating a process."
  (with-slots (active main mailbox message-callback) process
    (proclock (process :active) (setf active nil))
    (proclock (process :main) (setf main nil))
    (proclock (process :mailbox) (setf mailbox nil))
    (proclock (process :message-callback) (setf message-callback nil))))

;;; ----------------------------------------------------------------------------
;;; main API
;;; ----------------------------------------------------------------------------

(defun id (process)
  "Get a process' ID."
  (process-id process))

(defmacro with-messages (process bind-msg &body body)
  "Called from within a process to set up message handling (otherwise process
   will just quit after it runs its main function)."
  (let ((fn-var (gensym "fn-var"))
        (process-var (gensym "process")))
    `(let* ((,process-var ,process)
            (,fn-var (lambda ,bind-msg
                       ,@body)))
       (proclock (,process-var :message-callback)
         (setf (process-message-callback ,process-var) ,fn-var)))))

(defmacro receive (process &body clauses)
  "A wrapper around with-messages that utilizes pattern-matching via optima to
   provide a nice interface for pattern-matched message receival. Also provides
   a (self) funciton which returns the ID of the current process."
  (let ((message-var (gensym "message")))
    `(with-messages ,process (&rest ,message-var)
       (flet ((self () (id ,process)))
         (match ,message-var
           ,@(loop for clause in clauses collect
               `((list ,@(car clause)) (progn ,@(cdr clause)))))))))

(defun process-ready-p (process)
  "Whether or not a process has been fully set up (and is ready to receive
   messages)."
  (when (lookup process)
    (proclock (process :ready)
      (process-ready process))))

(defun process-active-p (process)
  "Whether or not a process is active/running."
  (when (lookup process)
    (proclock (process :active)
      (process-active process))))

(defun process-queued-p (process)
  "Whether or not the process is in queue for execution (usually this means it
   has messages waiting)."
  (when (lookup process)
    (proclock (process :queued)
      (process-queued process))))

