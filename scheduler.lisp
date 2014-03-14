(in-package :fern)

(define-condition process-not-found () ())

(defvar *process-queue* (make-queue)
  "Holds a queue of active processes.")

(defvar *process-registry* (make-hash-table :test #'equal)
  "Maps ids to processes.")

(defvar *process-registry-lock* (bt:make-lock)
  "Lock for the process registry.")

(defclass scheduler ()
  ((name :accessor scheduler-name :initarg :name :initform "slappy"
    :documentation "Holds the scheduler's name.")
   (active :accessor scheduler-active :initarg :active :initform nil
    :documentation "Whether or not this scheduler should be running.")
   (thread :accessor scheduler-thread :initarg :thread :initform nil
    :documentation "Holds a reference to the scheduler's thread.")
   (lock :accessor scheduler-lock :initform (bt:make-lock)
    :documentation "Lock for updating the scheduler's state.")))

(defun switch-main (scheduler)
  "Main scheduler loop."
  (unless (bt:with-lock-held ((scheduler-lock scheduler))
            (scheduler-active scheduler))
    (return-from switch-main nil))
  (if (jpl-queues:empty? *process-queue*)
      (as:delay (lambda () (switch-main scheduler)))
      (let* ((process (jpl-queues:dequeue *process-queue*)))
        (handler-case
          (run-process process)
          (t (e) (log:error "scheduler (~a): error running process: ~a" (scheduler-name scheduler) e)))
        (as:delay (lambda () (switch-main scheduler))))))

(defun activate-process (process)
  "Mark a process for execution."
  (when (process-active process)
    (jpl-queues:enqueue process *process-queue*)))

(defun format-process-id (id)
  "Standard function for formatting process ids."
  (string-downcase (string id)))

(defun register-process (id process)
  "Register a process id."
  (bt:with-lock-held (*process-registry-lock*)
    (setf (gethash (format-process-id id) *process-registry*) process)))

(defun unregister-process (id)
  "Unregister a process id."
  (bt:with-lock-held (*process-registry-lock*)
    (remhash (format-process-id id) *process-registry*)))

(defun lookup (id)
  "Lookup a process by id."
  (if (typep id 'process)
      id
      (bt:with-lock-held (*process-registry-lock*)
        (gethash (format-process-id id) *process-registry*))))

(defmacro with-process-by-id ((process &key errorp) &body body)
  "Gets a process by id (if it's not already a process object)."
  `(let* ((,process (lookup ,process)))
     ,(if errorp
          `(if process
               (progn ,@body)
               (error 'process-not-found))
          `(when ,process
             ,@body))))

;;; ----------------------------------------------------------------------------
;;; main API
;;; ----------------------------------------------------------------------------

(defun create-scheduler (&key (name (random 999)))
  "Create a threaded process scheduler."
  (let* ((scheduler (make-instance 'scheduler :name name :active t))
         (thread (bt:make-thread (lambda ()
                                   (log:debug "scheduler ~a starting" (scheduler-name scheduler))
                                   (as:with-event-loop (:catch-app-errors t)
                                     (switch-main scheduler))
                                   (log:debug "scheduler ~a ending" (scheduler-name scheduler))))))
    (setf (scheduler-thread scheduler) thread)
    scheduler))

(defun stop-scheduler (scheduler &key force)
  "Forcibly kill a scheduler."
  (bt:with-lock-held ((scheduler-lock scheduler))
    (setf (scheduler-active scheduler) nil))
  (when force
    (bt:destroy-thread (scheduler-thread scheduler))))

(defun process (id main-function)
  "Create a new process and queue it for execution."
  (let ((process (make-process main-function :id id :active t)))
    (register-process id process)
    (activate-process process)
    process))

(defmacro with-process ((id bind-process) &body body)
  "Convenience wrapper around process function."
  `(process ,id (lambda (,bind-process) ,@body)))

(defun terminate (process)
  "Quit a process."
  (with-process-by-id (process)
    (wipe-process process)
    (unregister-process (process-id process))))

(defun send (process &rest args)
  "Send a message to a process. MUST be a copyable structure: strings, numbers,
   symbols, or lists thereof."
  (with-process-by-id (process)
    (enqueue-message process args)
    (activate-process process)))

